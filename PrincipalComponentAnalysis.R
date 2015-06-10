library(caret);
library(kernlab);
data(spam)

#Finding correlated predictors
inTrain = createDataPartition(y=spam$type, p=0.75, list=FALSE)

training = spam[inTrain,]
testing = spam[-inTrain,]

M = abs(cor(training[,-58]))
diag(M) = 0
which(M > -, arr.ind = T)

names(spam)[c(34, 32)]

plot(spam[,34], spam[,32])

#Creating new variables
X = 0.71*training$num415 + 0.71*training$num857
Y = 0.71*training$num415 - 0.71*training$num857
plot(X,Y)

#Use less variable to explain what's going on

#Principal components
smallSpam = spam[,c(34,32)]
prComp = prcomp(smallSpam)
plot(prComp$x[,1], prComp$[,2])

prComp$rotation

#PCA on spam data
typeColor = ((spam$type=="spam")*1 + 1)
prComp = prcomp(log10(spam,[-58]+1))
plot(prComp$x[,1], prComp$x[,2], col=typeColor, xlab="PC1", ylab="PC2")

#PCA with caret
preProc = preProcess(log10(spam[,-58] + 1), method="pca", pcaComp=2)
spamPC = predict(preProc, log10(spam[,-58] + 1))
plot(spamPC[,1], spamPC[,2], col=typeColor)

trainPC = predict(preProc, log10(training[,-58] + 1))
modelFit - train(training$type ~ ., method="glm", data=trainPC)

testPC = predict(preProc, log10(testing[,-58] + 1))
confusionMatrix(testing$type, predict(modelFit, testPC))
