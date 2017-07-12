
source("~/analysis/Rcode/datasetLoader.R")
source("~/analysis/Rcode/pcaLoader.R")
source("~/analysis/Rcode/classifierLoader.R")
dataArray <- c("australian","balance", "breastcancer", "dermatology", "heart","ionosphere","iris","liver","sonar","vehicle","waveform","yeast","letter","glass","diabetes","mfeat","isolet","abalone","adult")
pcaArray <- c("prcomp", "pcal1","PCAproj","PCAgrid","l1pca","l1pcastar","l1pcahp")
classifierArray <- c("knn", "rf", "nb", "gbm", "svm", "adaboost", "C50","AdaBag","manb", "hdda", "xyf", "M5", "rocc", "Linda", "pls", "glmStepAIC", "gaussprLinear", "gamboost", "bayesglm","rpart", "cforest", "glmnet", "J48", "LMT", "M5Rules", "PART", "OneR", "JRip")
# dataArray <- c("yeast","letter","glass","diabetes","mfeat","isolet","abalone","adult","arrhythmia")
# pcaArray <- c("prcomp", "pcal1","PCAproj","PCAgrid","l1pca","l1pcastar","l1pcahp")
# classifierArray <- c("knn")
results <- array(0, dim = c(length(dataArray),length(pcaArray),length(classifierArray),10, 10, 10))
dimnames(results) <- list(c("australian","balance", "breastcancer", "dermatology", "heart","ionosphere","iris","liver","sonar","vehicle","waveform","yeast","letter","glass","diabetes","mfeat","isolet","abalone","adult"),
                          c("prcomp", "pcal1","PCAproj","PCAgrid","l1pca","l1pcastar","l1pcahp"), 
                          c("knn", "rf", "nb", "gbm", "svm", "adaboost", "C50","AdaBag","manb", "hdda", "xyf", "M5", "rocc", "Linda", "pls", "glmStepAIC", "gaussprLinear", "gamboost", "bayesglm","rpart", "cforest", "glmnet", "J48", "LMT", "M5Rules", "PART", "OneR", "JRip"),
                          c(1:10),
                          c(1:10),
                          c(1:10)
                          )  
#dim_acc <- c()
for(dArr in 1:length(dataArray)){
  print(dataArray[dArr])
  newData <- datasetLoader(dataArray[dArr])
  attrbData <- newData$attributes
  dimNum <- ncol(attrbData)
  j <- min(dimNum, 10)
  seeds <- c(1234, 1989, 290889, 251091, 240664, 190364, 120863, 101295, 31089, 3573113)
  for(nComp in 1:j){
    for(k in 1:2){
      set.seed(seeds[k])
      dataAtt <- newData$attributes
      dataClass <- as.factor(newData$class)
      chAtt <- runif(nrow(dataAtt))
      dataAtt <- dataAtt[order(chAtt),]
      dataClass <- dataClass[order(chAtt)]
      folds <- cut(seq(1,nrow(dataAtt)),breaks=10,labels=FALSE)
      for(i in 1:2){
        testIndexes <- which(folds==i,arr.ind=TRUE)
        testData <- dataAtt[testIndexes, ]
        trainData <- dataAtt[-testIndexes, ]
        testClass <- dataClass[testIndexes]
        trainClass <- dataClass[-testIndexes]
        for(pArr in 1 :length(pcaArray)){
         print(pcaArray[pArr])
          pcaLoad <- pcaLoader(trainData, nComp, pcaArray[pArr])
          trainAttrb <- data.matrix(trainData) %*% pcaLoad$loadings
          testAttrb <- data.matrix(testData) %*% pcaLoad$loadings
          if (nComp == 1){
            colnames(trainAttrb) <- "1"
            colnames(testAttrb) <- "1"
          }
          for(cArr in 1: length(classifierArray)){
            print(classifierArray[cArr])
            results[dataArray[dArr],pcaArray[pArr],classifierArray[cArr],nComp, k, i] <- classifierLoader(data.frame(trainAttrb), trainClass, data.frame(testAttrb), testClass, classifierArray[cArr])

          }

        }

      }

    }

  }

}