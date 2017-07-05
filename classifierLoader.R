classifierLoader <- function(trainPca, trainClass, testPca, testClass, nComp, classifierName)
{
  #install dependencies of caret
  #install.packages("caret", dependencies = c("Depends", "Suggests"))
  library(caret)
  library(randomForest)
  library(gbm)
  library(naivebayes)
  library(C50)
  switch(classifierName,
         knn = {#knn
           trainMatrix <- cbind(trainPca, trainClass)
           testMatrix <- cbind(testPca, testClass)
           predict <- knn(train = data.matrix(trainMatrix[,1:nComp]), test = data.matrix(testMatrix[,1:nComp]), cl = trainMatrix[,nComp+1],k=1)
           outputAcc <- mean(predict==testMatrix[,nComp+1])
         },
        
         rf = {
           # rfModel <- randomForest( x = trainPca, y = trainClass, xtest = testPca, ytest = testClass)
           # #calculate accuracy
           # confusionMx <- rfModel$test$confusion[,-ncol(rfModel$test$confusion)]
           # outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
           modelFit <- train(trainPca, trainClass, method = "rf")
           confusionMx     <- table(testClass, predict(modelFit, testPca))
           outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
         },
         nb = {#nb
           # modelFit <- naive_bayes( x = trainPca, y = trainClass, xtest = testPca, ytest = testClass)
           modelFit <- train(trainPca, trainClass, method = "nb")
           confusionMx     <- table(testClass, predict(modelFit, testPca))
           outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
           # nbModel <- naive_bayes(x = trainPca, y = trainClass, xtest = testPca, ytest = testClass)
           # confusionMx <- nbModel$test$confusion[,-ncol(nbModel$test$confusion)]
           # outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
         },
         # gbm ={
         #   modelFit <- train(trainPca, trainClass, method = "gbm", verbose = FALSE)
         #   confusionMx     <- table(testClass,predict(modelFit, testPca))
         #   outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
         # },
         
         { 
           print('\nMethod not found\n')
         }
        
  )
  outputAcc

}