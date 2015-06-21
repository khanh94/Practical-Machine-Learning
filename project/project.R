#Load the libraries for data processing
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(corrplot)

#Download the data
setwd('/Users/knguyen1/Documents/Practical-Machine-Learning/project')
trainUrl = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
testUrl = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"

trainingFile = "pml-training.csv"

testingFile = "pml-testing.csv"

#Check if the files exist
if (!file.exists(trainingFile)) {
  download.file(trainUrl, destfile=trainingFile, method="curl")
}



if (!file.exists(testingFile)) {
  download.file(testingUrl, destfile=testingFile, method="curl")
}


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

classe = trainingRaw$classe
trainingRemoved = grepl("^X|timestamp|window", names(trainingRaw))
trainingRaw = trainingRaw[, !trainingRemoved]

trainingCleaned = trainingRaw[, sapply(trainingRaw, is.numeric)]
trainingCleaned$classe = classe
testingRemoved = grepl("^X|timestamp|window", names(testingRaw))
testingRaw = testingRaw[, !testingRemoved]
testingCleaned = testingRaw[, sapply(testingRaw, is.numeric)]

#Data slicing
set.seed(42355)
inTrain = createDataPartition(trainingCleaned$classe, p=0.60, list=FALSE)
trainingData = trainingCleaned[inTrain, ]
testingData = trainingCleaned[-inTrain, ]

#Modeling the data using random forest and 10-fold cross validation
controlRF = trainControl(method="cv", 10)
modelRF = train(classe ~ ., data=trainingData, method="rf", trControl=controlRF, ntree=150)
modelRF

predictRF = predict(modelRF, testingData)
confusionMatrix(testingData$classe, predictRF)

accuracy = postResample(predictRF, testingData$classe)
accuracy

#Applying the model to the testing dataset
finalresult = predict(modelRF, testingCleaned[, -length(names(testingCleaned))])
finalresult

#Writing results to files
PML_write_files = function(x) {
  n = length(x)
  for(i in 1:n){
    filenames = paste0("problem_id_", i, ".txt")
    write.table(x[i], file=filenames, quote=FALSE, row.names=FALSE, col.names=FALSE)
  }
}

PML_write_files(finalresult)





