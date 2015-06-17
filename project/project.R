#Load the libraries for data processing
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)

#Download the data
setwd('/Users/knguyen1/Documents/Practical-Machine-Learning')
trainUrl = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

#Check if the files exist
if (!file.exists(trainingFile)) {
  download.file(trainUrl, destfile=trainingFile, method="curl")
}

if (!file.exists(testingFile)) {
  download.file(testingUrl, destfile=testingFile, method="curl")
}

trainingFile = "pml-training.csv"
testingFile = "pml-testing.csv"

#Loading the data into R
trainingRaw = read.csv('pml-training.csv')
testingRaw = read.csv('pml-testing.csv')
dim(trainingRaw)

dim(testingRaw)

sum(complete.cases(trainingRaw))

#Remove columns containing NA values
trainingRaw = trainingRaw[, colSums(is.na(trainingRaw)) == 0]
testingRaw = testingRaw[, colSums(is.na(testingRaw)) == 0]






