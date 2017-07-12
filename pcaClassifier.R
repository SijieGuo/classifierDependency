pcaAccuracy <- function(pcaName, dataset, classifierName, lowDim, highDim)
{
  #call function: pcaAccuracy <- array(dataset,pca,classifiers,projDim(10),shuffle(10),folder(10))
  #install dependencies of caret
  #install.packages("caret", dependencies = c("Depends", "Suggests")
  source("~/analysis/Rcode/datasetLoader.R")
  source("~/analysis/Rcode/pcaLoader.R")
  source("~/analysis/Rcode/classifierLoader.R")
  newData <- datasetLoader(dataset)
  #set seeds
  seeds <- c(1234, 1989, 290889, 251091, 240664, 190364, 120863, 101295, 31089, 3573113)
  outputAcc <- array(0, dim = c(highDim - lowDim + 1, 10, 10))
  for(nComp in lowDim:highDim){
    #avg_acc <- c()
   # for(k in 1:length(seeds)){#shuffle 10 times
    for(k in 1:10){
      set.seed(seeds[k])
      dataAtt <- newData$attributes
      dataClass <- as.factor(newData$class)
      chAtt <- runif(nrow(dataAtt))
      dataAtt <- dataAtt[order(chAtt),]
      dataClass <- dataClass[order(chAtt)]
      #pca_acc <- numeric()
      
      #Create 10 equally size folds
      folds <- cut(seq(1,nrow(dataAtt)),breaks=10,labels=FALSE)
      for(i in 1:10){
      #for(i in 1:10){
        #Segement iris by fold using the which() function 
        testIndexes <- which(folds==i,arr.ind=TRUE)
        testData <- dataAtt[testIndexes, ]
        trainData <- dataAtt[-testIndexes, ]
        testClass <- dataClass[testIndexes]
        trainClass <- dataClass[-testIndexes]
        # call pcaLoader
        # switch from pca : prcomp, pcal1,PCAproj,PCAgrid,l1pca,l1pcastar,l1pcahp
        #pca
        # function return testpca and trainpca
        pcaLoad <- pcaLoader(trainData, nComp, pcaName)
        trainAttrb <- data.matrix(trainData) %*% pcaLoad$loadings
        testAttrb <- data.matrix(testData) %*% pcaLoad$loadings

        # load classifier loader return accuracy
        if (nComp == 1){
          colnames(trainAttrb) <- "1"
          colnames(testAttrb) <- "1"
        }
        outputAcc[nComp - lowDim + 1, k, i] <- classifierLoader(data.frame(trainAttrb), trainClass, data.frame(testAttrb), testClass, classifierName)
        #outputAcc[dataArray[dArr], pcaArray[pArr],classifierArray[cArr],nComp - lowDim + 1, k, i] <- classifierLoader(data.frame(trainAttrb), trainClass, data.frame(testAttrb), testClass, classifierName)
      }
      
    }
    
  }
  outputAcc
}
