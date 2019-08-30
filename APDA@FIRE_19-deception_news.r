# Including needed libraries
library(qdap)
library(tm)
library(splitstackshape)
library(caret)
library(stopwords)

start.time <- Sys.time()

# Preparing parameters
n <- 1000
#lang <- 'ar'
lang <- ''
path_training <- "/home/thanatos/Escritorio/Text Mining en Social Media/APDA/FIRE/deception/training/APDA@FIRE.Qatar-News-corpus.training.20190524.csv"    # Your training path
path_test <- "/home/thanatos/Escritorio/Text Mining en Social Media/APDA/FIRE/deception/test/APDA@FIRE.Qatar-News-corpus.test.20190608.csv"
k <- 4
r <- 1

# Auxiliar functions
# * GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
# * GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the Bag of Words representation
# * DistributeProbabilisticPrediction: Adjust class predictions by combining the prediction probabilities with
#                                       the classes probabilistic distribution
GenerateVocabulary <- function(path, n=1000, lowcase=TRUE, punctuations=TRUE, numbers=TRUE, whitespaces=TRUE, swlang='', swlist='', verbose=TRUE) {
  
  # Reading the corpus list of files
  csv = read.csv(path, header=FALSE)
  names(csv) <- c("ID", "CLASS", "TEXT")
  
  # Reading files contents and concatenating into the corpus.raw variable
  corpus.raw <- paste(csv$TEXT, collapse=' ')
  
  # Preprocessing the corpus
  corpus.preprocessed <- corpus.raw
  
  if (lowcase) {
    if (verbose) print("To lower...")
    corpus.preprocessed <- tolower(corpus.preprocessed)
  }
  
  if (numbers) {
    if (verbose) print("Removing numbers...")
    corpus.preprocessed <- removeNumbers(corpus.preprocessed)
  }
  
  if (whitespaces) {
    if (verbose) print("Stripping whitespaces...")
    corpus.preprocessed <- stripWhitespace(corpus.preprocessed)
  }
  
  if (swlang != '') {
    if (verbose) print(paste("Removing stopwords for language ", swlang, '...'))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang, source="stopwords-iso"))
  }
  
  if (length(swlist) > 0 || swlist != '') {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  #corpus.preprocessed <- arabicStemR::removeStopWords(corpus.preprocessed)$text
  
  if (punctuations) {
    if (verbose) print("Removing punctuations...")
    corpus.preprocessed <- removePunctuation(corpus.preprocessed)
  }
  
  if (swlang != '') {
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang, source="stopwords-iso"))
  }
  if (length(swlist) > 0 || swlist != '') {
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  #corpus.preprocessed <- arabicStemR::removeStopWords(corpus.preprocessed)$text
  
  # Generating the vocabulary as the n most frequent terms
  if (verbose) print("Generating frequency terms")
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  if (verbose) plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}

# GenerateBoW: Given a corpus (trining or test), and a vocabulary, obtains the BoW representation
GenerateBoW <- function(path, vocabulary, n=100000, lowcase=TRUE, punctuations=TRUE, numbers=TRUE, whitespaces=TRUE, swlang='', swlist='', verbose=TRUE) {
  
  # Reading the file
  csv = read.csv(path, header=FALSE)
  names(csv) <- c("ID", "CLASS", "TEXT")
  
  i <- 0
  bow <- NULL
  
  # Reading the list of files in the corpus
  for (iRow in 1:nrow(csv)) {
    # Obtaining truth information for the current author
    author <- csv[iRow, 1]
    theClass <- csv[iRow, 2]
    txtdata <- csv[iRow, 3]
    
    # Preprocessing the text
    if (lowcase) {
      txtdata <- tolower(txtdata)
    }
    
    if (punctuations) {
      txtdata <- removePunctuation(txtdata)
    }
    
    if (numbers) {
      txtdata <- removeNumbers(txtdata)
    }
    
    if (whitespaces) {
      txtdata <- stripWhitespace(txtdata)
    }
    
    if (swlang != '') {
      txtdata <- removeWords(stopwords(swlang, source="stopwords-iso"))
    }
    
    if (length(swlist) > 0 && swlist != '') {
      txtdata <- removeWords(swlist)
    }
    
    # Building the vector space model. For each word in the vocabulary, it obtains the frequency of occurrence in the current author.
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word, "FREQ"]) > 0) {
        thefreq <- freq[freq$WORD==word, "FREQ"]
      }
      line <- paste(line, ',', thefreq, sep='')
    }
    
    line <- paste(theClass, ',', line, sep='')
    
    # New row in the vector space model matrix
    bow <- rbind(bow, line)
    i <- i + 1
    
    if (verbose) {
      print(paste(i, author, theClass))
    }
  }
  
  return (bow)
}

# DistributeProbabilisticPrediction: Adjust class predictions combining the prediction probabilities with
#                                     the classes probabilistic distribution
distributeProbabilisticPrediction <- function(pred, classes, class_probs) {
  predicted_class <- as.character(pred['Class'])
  
  # If class prediction probability is lower than the distribution probability for that class,
  # select the class which maximizes the product of prediction probability and distribution probability
  if (as.double(pred[predicted_class]) < class_probs[predicted_class]) {
    prod_class <- pred['Class']
    prod_prob <- as.double(pred[predicted_class]) * class_probs[predicted_class]
    for (c in classes) {
      prod_aux = as.double(pred[c]) * class_probs[c]
      if (prod_aux > prod_prob) {
        prod_prob <- prod_aux
        prod_class <- c
      }
    }
    return (prod_class)
  } else {
    return (pred['Class'])
  }
}

# GENERATE VOCABULARY ######################################
sw <- c(stopwords('en', source="stopwords-iso"), stopwords('en', source="snowball"))
vocabulary <- GenerateVocabulary(path_training, n, swlang=lang, swlist=sw)

# GENERATING THE BoW FOR THE TRAINING SET ##################
bow_training <- GenerateBoW(path_training, vocabulary)

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET ####
training <- concat.split(bow_training, 'V1', sep=',', drop=TRUE)
training <- cbind(training[,1], apply(training[,3:ncol(training)], 2, function(x) (x - mean(x)) / sd(x) ^ as.logical(sd(x))))
names(training)[1] <- "theClass"

# Computing classes probability distribution
classes <- levels(training$theClass)
class_probs <- prop.table(table(training$theClass))

# Learning a SVM with the whole training set and without evaluating it
train_control <- trainControl(method="none")
model_SVM <- train(theClass~., data=training, trControl=train_control, method="svmLinear", scale=FALSE)

# Learning a SVM evaluating it with k-fold cross-validation
train_control <- trainControl(method="repeatedcv", number=k, repeats=r, classProbs=TRUE)
model_SVM <- train(theClass~., data=training, trControl=train_control, method="svmLinear", scale=FALSE)
print(model_SVM)
confusionMatrix(model_SVM)

# GENERATING THE BoW FOR THE TEST SET ######################
bow_test <- GenerateBoW(path_test, vocabulary, verbose=FALSE)

# Preparing the vector space model and truth for the test set
test <- concat.split(bow_test, 'V1', ',', drop=TRUE)
truth <- unlist(test[,1])    # Only when the truth file is available
authors <- as.data.frame(unlist(test[,2]))
test <- test[,3:ncol(test)]

# Predicting and evaluating the prediction
pred_SVM.class <- as.data.frame(predict(model_SVM, test, type="raw"))
pred_SVM.prob  <- as.data.frame(predict(model_SVM, test, type="prob"))
pred_SVM <- cbind(pred_SVM.class, pred_SVM.prob)
names(pred_SVM)[1] <- "Class"

# Check and adjust predicted classes based on prediction probabilities
# and density probability distribution for every class
#pred_SVM.class <- as.data.frame(apply(pred_SVM, 1, distributeProbabilisticPrediction, classes, class_probs), stringAsFactors=TRUE)
pred_SVM[1] <- pred_SVM.class

#confusionMatrix(pred_SVM.class, truth)    # Only when the truth file is available

# EXPORT PREDICTIONS TO TRUTH FILE #########################
test_predicted <- cbind(authors, pred_SVM.class)
names(test_predicted) <- c("id", "prediction")
test_truth_path <- sub("corpus", "predictions", path_test, fixed=TRUE)
write.csv(test_predicted, test_truth_path, row.names=FALSE)

end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste("Time taken: ", time.taken))
