#Quiz 3
#Question 1

library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)
data = segmentationOriginal
set.seed(125)
inTrain = data$Case == "Train"
trainData = data[inTrain, ]
testData = data[-inTrain, ]

cartModel = train(Class ~ ., data=trainData, method='rpart')
cartModel$finalModel

plot(cartModel$finalModel, uniform=T)
text(cartModel$finalMode, cex=0.8)

#Question 3
#Load the olive oil data
library(pgmm)
data(olive)
dim(olive)
head(olive)
olive = olive[, -1]
treeModel = train(Area ~., data=olive, method="rpart2")
treeModel
newdata = as.data.frame(t(colMeans(olive)))
predict(treeModel, newdata)

#Question 4
library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1], size=dim(SAheart)[1]/2, replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

set.seed(13234)
logitModel = train(chd ~ age + alcohol + obesity + tobacco + typea + =ldl, data=trainSA, method="glm", family="binomial")
logitModel

missClass = function(values, prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
predictTrain = predict(logitModel, trainSA)
predictTest = predict(logitModel, testSA)

#Training set misclassification rate
missClass(trainSA$chd, predictTrain)

#Testing set misclassification rate
missClass(trainSA$chd, predictTest)

#Question 5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test)
head(vowel.train)
head(vowel.test)
dim(vowel.train)
dim(vowel.test)
vowel.train$y = as.factor(vowel.train$y)
vowel.test$y = as.factor(vowel.test$y)
set.seed(33833)
modelRf = randomForest(y ~ ., data=vowel.train, importance = FALSE)
order(varImp(modelRf), decreasing=T)


