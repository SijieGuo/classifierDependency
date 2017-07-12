classifierLoader <- function(trainAttrb, trainClass, testAttrb, testClass, classifierName)
{
  #install dependencies of caret
  #install.packages("caret", dependencies = c("Depends", "Suggests"))
  library(caret)

  # classifiers : 1. knn, 2. rf, 3. nb, 4. gbm, 5. svm, 6. adaboost, 7. C50, 8. AdaBag, 9. manb, 10. hdda
  # 11. xyf, 12. M5, 13. rocc, 14. Linda, 15. pls, 16. glmStepAIC, 17. gaussprLinear, 18. gamboost, 19. bayesglm
  # 20. rpart, 21. cforest, 22. glmnet, 23. J48, 24. LMT, 25. M5Rules, 26. PART, 27. OneR, 28. JRip
  
  switch(classifierName,
         # 1. knn
         knn = {
           modelFit <- train(trainAttrb, trainClass, method = "knn")
           confusionMx     <- table(testClass, predict(modelFit, testAttrb))
           outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
         },
        # 2. rf
         rf = {
           # rfModel <- randomForest( x = trainAttrb, y = trainClass, xtest = testAttrb, ytest = testClass)
           # #calculate accuracy
           # confusionMx <- rfModel$test$confusion[,-ncol(rfModel$test$confusion)]
           # outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
           modelFit <- train(trainAttrb, trainClass, method = "rf")
           confusionMx     <- table(testClass, predict(modelFit, testAttrb))
           outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
         },
        # 3. nb
         nb = {
           modelFit <- train(trainAttrb, trainClass, method = "nb")
           confusionMx     <- table(testClass, predict(modelFit, testAttrb))
           outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
         },
        # 4. gbm
         gbm ={
           modelFit <- train(trainAttrb, trainClass, method = "gbm", verbose = FALSE)
           confusionMx     <- table(testClass,predict(modelFit, testAttrb))
           outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
         },
        # 5. svm
        svm = {
          modelFit <- train(trainAttrb, trainClass, method = "svmRadial")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 6. adaboost
        adaboost = {
          modelFit <- train(trainAttrb, trainClass, method = "adaboost")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 7. C50
        C50 = {
          modelFit <- train(trainAttrb, trainClass, method = "C5.0")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 8. AdaBag
        AdaBag = {
          modelFit <- train(trainAttrb, trainClass, method = "AdaBag")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 9. manb
        manb = {
          modelFit <- train(trainAttrb, trainClass, method = "manb")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        #10. hdda
        hdda = {
          modelFit <- train(trainAttrb, trainClass, method = "hdda")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        #11. xyf
        xyf = {
          modelFit <- train(trainAttrb, trainClass, method = "xyf")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        #12. M5
        M5 = {
          modelFit <- train(trainAttrb, trainClass, method = "M5")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        #13. rocc
        rocc = {
          modelFit <- train(trainAttrb, trainClass, method = "rocc")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        #14. Linda
        Linda = {
          modelFit <- train(trainAttrb, trainClass, method = "Linda")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        #15.pls
        pls = {
          modelFit <- train(trainAttrb, trainClass, method = "pls")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        #16. glmStepAIC
        glmStepAIC = {
          modelFit <- train(trainAttrb, trainClass, method = "glmStepAIC")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        #17. gaussprLinear
        gaussprLinear = {
          modelFit <- train(trainAttrb, trainClass, method = "gaussprLinear")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 18. gamboost
        gamboost = {
          modelFit <- train(trainAttrb, trainClass, method = "gamboost")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 19. bayesglm
        bayesglm = {
          modelFit <- train(trainAttrb, trainClass, method = "bayesglm")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 20. rpart
        rpart = {
          modelFit <- train(trainAttrb, trainClass, method = "rpart")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 21. cforest
        cforest = {
          modelFit <- train(trainAttrb, trainClass, method = "cforest")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 22. glmnet
        glmnet = {
          modelFit <- train(trainAttrb, trainClass, method = "glmnet")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 23. J48
        J48 = {
          modelFit <- train(trainAttrb, trainClass, method = "J48")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 24. LMT
        LMT = {
          modelFit <- train(trainAttrb, trainClass, method = "LMT")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 25. M5Rules
        M5Rules = {
          modelFit <- train(trainAttrb, trainClass, method = "M5Rules")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 26. PART
        PART = {
          modelFit <- train(trainAttrb, trainClass, method = "PART")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 27. OneR
        OneR = {
          modelFit <- train(trainAttrb, trainClass, method = "OneR")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        # 28. JRip
        JRip = {
          modelFit <- train(trainAttrb, trainClass, method = "JRip")
          confusionMx     <- table(testClass, predict(modelFit, testAttrb))
          outputAcc <- sum(diag(confusionMx))/sum(confusionMx)
        },
        { 
           print('\nMethod not found\n')
        }
        
  )
  outputAcc

}