# Including needed libraries
library(qdap)
library(XML)
library(tm)
library(splitstackshape)
library(caret)
library(stopwords)

start.time <- Sys.time()

# Preparing parameters
n <- 1000
lang <- 'ar'
path_training <- "/home/thanatos/Escritorio/Text Mining en Social Media/APDA/FIRE/profiling/training/corpus"    # Your training path
path_test <- "/home/thanatos/Escritorio/Text Mining en Social Media/APDA/FIRE/profiling/test/corpus"
k <- 4
r <- 1

# Auxiliar functions
# * GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
# * GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the BoW representation
# * DistributeProbabilisticPrediction: Adjust class predictions by combining the prediction probabilities
#                                       with the classes probabilistic distribution

# GenerateVocabulary: Given a corpus (training set), obtains the n most frequent words
GenerateVocabulary <- function(path, n=1000, lowcase=TRUE, punctuations=TRUE, numbers=TRUE, whitespaces=TRUE, swlang="", swlist="", verbose=TRUE) {
  setwd(path)
  
  # Reading corpus list of files
  files = list.files(pattern="*.xml")
  
  # Reading files contents and concatenating into the corpus.raw variable
  corpus.raw <- NULL
  i <- 0
  for (file in files) {
    xmlfile <- xmlTreeParse(file, useInternalNodes=TRUE)
    corpus.raw <- c(corpus.raw, xpathApply(xmlfile, "//document", function(x) xmlValue(x)))
    i <- i + 1
    if (verbose) print(paste(i, " ", file))
  }
  
  # Preprocessing the corpus
  corpus.preprocessed <- corpus.raw
  
  if (lowcase) {
    if (verbose) print("Tolower...")
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
    if (verbose) print(paste("Removing stopwords for language ", swlang, "..."))
    corpus.preprocessed <- removeWords(corpus.preprocessed, stopwords(swlang, source="stopwords-iso"))
  }
  
  if (length(swlist) > 0 || swlist != '') {
    if (verbose) print("Removing provided stopwords...")
    corpus.preprocessed <- removeWords(corpus.preprocessed, swlist)
  }
  
  corpus.preprocessed <- arabicStemR::removeStopWords(corpus.preprocessed)$text
  
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
  corpus.preprocessed <- arabicStemR::removeStopWords(corpus.preprocessed)$text
  
  # Generating the vocabulary as the n most frequent terms
  if (verbose) print("Generating frequency terms")
  corpus.frequentterms <- freq_terms(corpus.preprocessed, n)
  if (verbose) plot(corpus.frequentterms)
  
  return (corpus.frequentterms)
}

# GenerateBoW: Given a corpus (training or test), and a vocabulary, obtains the BoW representation
GenerateBoW <- function(path, vocabulary, n=100000, lowcase=TRUE, punctuations=TRUE, numbers=TRUE, whitespaces=TRUE, swlang="", swlist="", class="variety", verbose=TRUE) {
  setwd(path)
  
  # Reading the truth file
  training <- file.exists("../truth.txt")
  if (training) {
    truth <- read.csv("../truth.txt", sep=':', header=FALSE)
    truth <- truth[, c(1, 4, 7, 10)]
    colnames(truth) <- c("author", "gender", "age", "variety")
  }
  
  i <- 0
  bow <- NULL
  
  # Reading the list of files in the corpus
  files = list.files(pattern="*.xml")
  for (file in files) {
    # Obtaining truth information for the current author
    author <- gsub(".xml", "", file)
    if (training) {
      variety <- truth[truth$author == author, "variety"]
      gender <- truth[truth$author == author, "gender"]
      age <- truth[truth$author == author, "age"]
    }
    
    # Reading contents for the current author
    xmlfile <- xmlTreeParse(file, useInternalNodes=TRUE)
    txtdata <- xpathApply(xmlfile, "//document", function(x) xmlValue(x))
    
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
    
    # Building the vector space model. For each word in the vocabulary,
    # it obtains the frequency of occurrence in the current author.
    line <- author
    freq <- freq_terms(txtdata, n)
    for (word in vocabulary$WORD) {
      thefreq <- 0
      if (length(freq[freq$WORD==word, "FREQ"]) > 0) {
        thefreq <- freq[freq$WORD==word, "FREQ"]
      }
      line <- paste(line, ',', thefreq, sep='')
    }
    
    # Concatenating the corresponding class: age, variety or gender
    if (training) {
      if (class == "variety") {
        line <- paste(variety, ',', line, sep='')
      } else if (class == "gender") {
        line <- paste(gender, ',', line, sep='')
      } else {
        line <- paste(age, ',', line, sep='')
      }
    } else {
      line <- paste(',', line, sep='')
    }
    
    # New row in the vector space model matrix
    bow <- rbind(bow, line)
    i <- i + 1
    
    if (verbose) {
      if (training) {
        if (class == "variety") {
          print(paste(i, author, variety))
        } else if (class == "gender") {
          print(paste(i, author, gender))
        } else {
          print(paste(i, author, age))
        }
      } else {
        print(paste(i, author))
      }
    }
  }
  
  return (bow)
}

# DistributeProbabilisticPrediction: Adjust class predictions combining the prediction probabilities
#                                     with the classes probabilistic distribution
distributeProbabilisticPrediction <- function(pred, classes, class_probs) {
  predicted_class <- as.character(pred['Class'])
  
  # If class prediction probability is lower than the distribution probability for that class,
  # select the class which maximizes the product of prediction probability and distribution probability
  if (as.double(pred[predicted_class]) < class_probs[predicted_class]) {
    prod_class <- pred['Class']
    prob_prob <- as.double(pred[predicted_class]) * class_probs[predicted_class]
    for (c in classes) {
      prod_aux = as.double(pred[c]) * class_probs[c]
      if (prod_aux > prod_prob) {
        prod_prob < prod_aux
        prod_class <- c
      }
    }
    return (prod_class)
  } else {
    return (pred['Class'])
  }
}



# GENERATE VOCABULARY
sw <- c(stopwords('en', source="stopwords-iso"), stopwords('en', source="snowball"))
vocabulary <- GenerateVocabulary(path_training, n, swlang=lang, swlist=sw)

# GENERATING THE BoW FOR THE TEST SET
bow_test <- GenerateBoW(path_test, vocabulary, verbose=FALSE)

# Preparing the vector space model and truth for the test set
test <- concat.split(bow_test, 'V1', ',', drop=TRUE)
truth <- unlist(test[,1])
test <- cbind(test[,2], test[,3:ncol(test)])



# GENDER IDENTIFICATION
#######################
# GENERATING THE BoW FOR THE GENDER SUBTASK FOR THE TRAINING SET
bow_training_gender <- GenerateBoW(path_training, vocabulary, class="gender", verbose=FALSE)

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET
training_gender <- concat.split(bow_training_gender, 'V1', ',', drop=TRUE)
training_gender <- cbind(training_gender[,1], apply(training_gender[,3:ncol(training_gender)], 2, function(x) (x - mean(x)) / sd(x) ^ as.logical(sd(x))))
names(training_gender)[1] <- "theClass"

# Computing classes probability distribution
classes_gender <- levels(training_gender$theClass)
class_probs_gender <- prop.table(table(training_gender$theClass))

# Learning a SVM with the whole training set and without evaluating it
train_control <- trainControl(method="none")
model_SVM_gender <- train(theClass~., data=training_gender, trControl=train_control, method="svmLinear", scale=FALSE)

# Learning a SVM and evaluating it with k-fold cross-validation
train_control <- trainControl(method="repeatedcv", number=k, repeats=r, classProbs=TRUE)
model_SVM_gender <- train(theClass~., data=training_gender, trControl=train_control, method="svmLinear", scale=FALSE)
print(model_SVM_gender)
confusionMatrix(model_SVM_gender)

# Predicting and evaluating the prediction
pred_SVM_gender.class <- as.data.frame(predict(model_SVM_gender, test, type="raw"))
pred_SVM_gender.prob <- as.data.frame(predict(model_SVM_gender, test, type="prob"))
pred_SVM_gender <- cbind(pred_SVM_gender.class, pred_SVM_gender.prob)
names(pred_SVM_gender)[1] <- "Class"

# Check and adjust predicted classes based on prediction probabilities
# and density probability distribution for every class
pred_SVM_gender.class <- as.data.frame(apply(pred_SVM_gender, 1, distributeProbabilisticPrediction, classes_gender, class_probs_gender), stringsAsFactors=TRUE)
pred_SVM_gender[1] <- pred_SVM_gender.class

#confusionMatrix(pred_SVM_gender.class, truth)    # Only when the truth file is available



# AGE IDENTIFICATION
####################
# GENERATING THE BoW FOR THE AGE SUBTASK FOR THE TRAINING SET
bow_training_age <- GenerateBoW(path_training, vocabulary, class="age", verbose=FALSE)

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET
training_age <- concat.split(bow_training_age, 'V1', ',', drop=TRUE)
training_age <- cbind(training_age[,1], apply(training_age[,3:ncol(training_age)], 2, function(x) (x - mean(x)) / sd(x) ^ as.logical(sd(x))))
names(training_age)[1] <- "theClass"

# Computing classes probability distribution
classes_age <- levels(training_age$theClass)
class_probs_age <- prop.table(table(training_age$theClass))

# Learning a SVM with the whole training set and without evaluating it
train_control <- trainControl(method="none")
model_SVM_age <- train(theClass~., data=training_age, trControl=train_control, method="svmLinear", scale=FALSE)

# Learning a SVM and evaluating it with k-fold cross-validation
train_control <- trainControl(method="repeatedcv", number=k, repeats=r, classProbs=TRUE)
model_SVM_age <- train(theClass~., data=training_age, trControl=train_control, method="svmLinear", scale=FALSE)
print(model_SVM_age)
confusionMatrix(model_SVM_age)

# Predicting and evaluating the prediction
pred_SVM_age.class <- as.data.frame(predict(model_SVM_age, test, type="raw"))
pred_SVM_age.prob  <- as.data.frame(predict(model_SVM_age, test, type="prob"))
pred_SVM_age <- cbind(pred_SVM_age.class, pred_SVM_age.prob)
names(pred_SVM_age)[1] <- "Class"

# Check and adjust predicted classes based on prediction probabilities
# and density probability distribution for every class
pred_SVM_age.class <- as.data.frame(apply(pred_SVM_age, 1, distributeProbabilisticPrediction, classes_age, class_probs_age), stringsAsFactors=TRUE)
pred_SVM_age[1] <- pred_SVM_age.class

#confusionMatrix(pred_SVM_age.class, truth)    # Only when the truth file is available



# VARIETY IDENTIFICATION
########################
# GENERATING THE BoW FOR THE VARIETY SUBTASK FOR THE TRAINING SET
bow_training_variety <- GenerateBoW(path_training, vocabulary, class="variety", verbose=FALSE)

# PREPARING THE VECTOR SPACE MODEL FOR THE TRAINING SET
training_variety <- concat.split(bow_training_variety, 'V1', ',', drop=TRUE)
training_variety <- cbind(training_variety[,1], apply(training_variety[,3:ncol(training_variety)], 2, function(x) (x - mean(x)) / sd(x) ^ as.logical(sd(x))))
names(training_variety)[1] <- "theClass"

# Computing classes probability distribution
classes_variety <- levels(training_variety$theClass)
class_probs_variety <- prop.table(table(training_variety$theClass))

# Learning a SVN with the whole training set and without evaluating it
train_control <- trainControl(method="none")
model_SVM_variety <- train(theClass~., data=training_variety, trControl=train_control, method="svmLinear", scale=FALSE)

# Learning a SVM and evaluating it with k-fold cross-validation
train_control <- trainControl(method="repeatedcv", number=k, repeats=r, classProbs=TRUE)
model_SVM_variety <- train(theClass~., data=training_variety, trControl=train_control, method="svmLinear", scale=FALSE)
print(model_SVM_variety)
confusionMatrix(model_SVM_variety)

# Predicting and evaluating the prediction
pred_SVM_variety.class <- as.data.frame(predict(model_SVM_variety, test, type="raw"))
pred_SVM_variety.prob  <- as.data.frame(predict(model_SVM_variety, test, type="prob"))
pred_SVM_variety <- cbind(pred_SVM_variety.class, pred_SVM_variety.prob)
names(pred_SVM_variety)[1] <- "Class"

# Check and adjust predicted classes based on prediction probabilities
# and density probability distribution for every class
pred_SVM_variety.class <- as.data.frame(apply(pred_SVM_variety, 1, distributeProbabilisticPrediction, classes_variety, class_probs_variety), stringsAsFactors=TRUE)
pred_SVM_variety[1] <- pred_SVM_variety.class

#confusionMatrix(pred_SVM_variety.class, truth)    # Only when the truth file is available



# JOINT EVALUATION
##################
#joint <- data.frame(pred_SVM_gender, truth_gender, pred_SVM_variety, truth_variety, pred_SVM_age, truth_age)
#joint <- cbind(joint, ifelse(joint[,1] == joint[,2], 1, 0), ifelse(joint[,3] == joint[,4], 1, 0), ifelse(joint[,5] == joint[,6], 1, 0))
#joint <- cbind(joint, joint[,5] * joint[,6] * joint[,7])
#colnames(joint) <- c("pgender", "tgender", "pvariety", "tvariety", "page", "tage", "gender", "variety", "age", "joint")

#acc.gender <- sum(joint$gender) / nrow(joint)
#acc.variety <- sum(joint$variety) / nrow(joint)
#acc.age <- sum(joint$age) / nrow(joint)
#acc.joint <- sum(joint$joint) / nrow(joint)

end.time <- Sys.time()
time.taken <- end.time - start.time

#print(paste(acc.gender, acc.variety, acc.age, acc.joint, time.taken))
print(paste("Time taken: ", time.taken))

# EXPORT PREDICTIONS TO FILE
##################################
pred <- data.frame(test[,1], pred_SVM_gender, pred_SVM_age, pred_SVM_variety, check.names=FALSE, stringsAsFactors=FALSE)
colnames(pred) <- c("author", "gender", "age", "variety")
sink(paste(path_test, "/../", "predictions.txt", sep=""))
for (n in 1:nrow(pred)) {
  author  <- pred[n, "author"]
  gender  <- pred[n, "gender"]
  age     <- pred[n, "age"]
  variety <- pred[n, "variety"]
  cat(paste(author, ":::", gender, ":::", age, ":::", variety, '\n', sep=""))
}
sink()
