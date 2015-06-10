library(caret);
library(kernlab);
data(spam)
inTrain = createDataPartition(y = spam$type, p=0.75, list=FALSE)
training = spam[inTrain,]
testing = spam[-inTrain,]
hist(training$capitalAve,main="". xlab="ave. capital run length")
mean(training$capitalAve)
sd(training$capitalAve)
trainCapAve = training$capitalAve
trainCapAves = (trainCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(trainCapAves)

testCapAve = testing$capitalAve
testCapAves = (testCapAve - mean(trainCapAve))/sd(trainCapAve)
mean(testCapAves)
sd(testCapAves)
preObj = preProcess(training[,-58], method=c("center", "scale"))
trainCapAves = predict(preObj, training[,-58])$capitalAve
mean(trainCapAves)

testCapAveS = predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)

#Centering and scaling
set.seed(32343)
modelFit = train(type ~., data=training, preProcess=c("center", "scale"), method="glm")

modelFit

#Box-cox transformation
preObj = preProcess(training[,-58], method=c("BoxCox"))
trainCapAveS = predict(preObj, training[-58])$capitalAve
par(mfrow=c(1,2)); hist(trainCapAveS); qqnorm(trainCapAveS)

set.seed(13343)

#Make some values NA
training$capAve = training$capitalAve
selectNA = rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capAve[selectNA] = NA

#Impute and standardize
preObj = preProcess(training[,-58], method="knnImpute")
capAve = predict(preObj, training[,-58])$capAve

#Standardize true values
capAveTruth = training$capitalAve
capAveTruth = (capAveTruth-mean(capAveTruth))/sd(capAveTruth)

#Finding quantiles of data sets
quantile(capAve - capAveTruth)
quantile((capAve - capAveTruth)[selectNA])
quantile((capAve - capAveTruth)[!selectNA])



