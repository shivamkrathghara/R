# Automating Medical Reviews 

library(tm) # text analytics
library(caTools) # create training and test datasets
library(rpart) # Classificaiton and Regression Tree
library(rpart.plot)
library(ROCR) # receiver operator characteristic

trials = read.csv(file.choose(), stringsAsFactors=FALSE)
summary(trials)

cat("\nNumber of characters in longest abstract : ", max(nchar(trials$abstract)))
cat("\nNumber of search results with no abstract : ", sum(nchar(trials$abstract)==0))
cat("\nTitle of observation with shortest title : ", trials$title[which.min(nchar(trials$title))])
corpusTitle <- VCorpus(VectorSource(trials$title))
corpusAbstract <- VCorpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, content_transformer(tolower)) # lower-case
corpusTitle = tm_map(corpusTitle, removePunctuation)            # remove punctuation
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english")) # remove stop words
corpusTitle = tm_map(corpusTitle, stemDocument)                 # reduce words to 'stems'

corpusAbstract = tm_map(corpusAbstract, content_transformer(tolower)) # lower-case
corpusAbstract = tm_map(corpusAbstract, removePunctuation)            # remove punctuation
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english")) # remove stop words
corpusAbstract = tm_map(corpusAbstract, stemDocument)                 # reduce words to 'stems'

dtmTitle <- as.data.frame(as.matrix(removeSparseTerms(DocumentTermMatrix(corpusTitle), 0.95)))
dtmAbstract <- as.data.frame(as.matrix(removeSparseTerms(DocumentTermMatrix(corpusAbstract), 0.95)))
# create document term matrices. remove terms not present in at least 5% of documents

str(dtmTitle)

str(dtmAbstract)

cat("\nMost frequent word stem across all abstracts :", colnames(dtmAbstract)[which.max(colSums(dtmAbstract))])


colnames(dtmTitle) <-  paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) <-  paste0("A", colnames(dtmAbstract))
# Adding the letter T in front of all the title variable names and adding the letter A in front of all the abstract variable names.

dtm <- cbind(dtmTitle, dtmAbstract)
# Using cbind(), combine dtmTitle and dtmAbstract into a single data frame called dtm:
dtm$trial <- trials$trial

paste("Number of columns in 'dtm' datta-frame", ncol(dtm))

set.seed(144)

split <- sample.split(dtm$trial, SplitRatio = 0.7)
train <- subset(dtm, split == TRUE)
test <- subset(dtm, split == FALSE)

summary(train$trial)

cat("\nBaseline accuracty of most common outcome of train$trial : ", 1-mean(train$trial))

trialCART <- rpart(trial ~ . ,
                   data = train, method = "class")
prp(trialCART)

predictCARTtrain <- predict(trialCART)
cat("\nMaximum predicted probability for any result\nwhen applying model to training set :", max(predictCARTtrain[,2]))
predictCARTtrainconfusion <- table(train$trial, predictCARTtrain[,2] >= 0.5)
predictCARTtrainconfusion
cat("\nAccuracy : ", sum(diag(predictCARTtrainconfusion))/nrow(train))
cat("\nSensitivity : ", predictCARTtrainconfusion["1","TRUE"]/sum(predictCARTtrainconfusion["1",]))
cat("\nSpecificity : ", predictCARTtrainconfusion["0","FALSE"]/sum(predictCARTtrainconfusion["0",]))
predTest =  predict(trialCART,
                    newdata = test)

predTestconfusion <- table(test$trial, predTest[,2] >= 0.5)
predTestconfusion
cat("\nAccuracy : ", sum(diag(predTestconfusion))/nrow(test))
cat("\nSensitivity : ", predTestconfusion["1","TRUE"]/sum(predTestconfusion["1",]))
cat("\nSpecificity : ", predTestconfusion["0","FALSE"]/sum(predTestconfusion["0",]))
predictROCR <- prediction(predTest[,2], test$trial)
perfROCR <- performance(predictROCR, "tpr", "fpr") # true-positive rate vs false-positive rate

plot(perfROCR, colorize=TRUE,
     print.cutoffs.at = seq(0, 1, 0.1), text.adj = c(-0.2, 1.7))
cat("\nAUC :", performance(predictROCR, "auc")@y.values[[1]])
