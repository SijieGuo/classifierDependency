pcaAccuracy <- function(pcaName, dataset, lowDim, highDim)
{
  library(class) #Has the knn function
  library(pcaL1)
  library(pcaPP)
  library(stats)
  source("./datasetLoader.R")
  source("./pcaLoader.R")
  newData <- datasetLoader(dataset)
  #set seeds
  dim_acc <- c()
  for(nComp in lowDim:highDim){
    avg_acc <- c()
    for(seeds in 1:10){
      set.seed(seeds)
      dataAtt <- newData$attributes
      dataClass <- newData$class
      chAtt <- runif(nrow(dataAtt))
      dataAtt <- dataAtt[order(chAtt),]
      dataClass <- dataClass[order(chAtt)]
      pca_acc <- numeric()
      
      #Create 10 equally size folds
      folds <- cut(seq(1,nrow(dataAtt)),breaks=10,labels=FALSE)
      for(i in 1:10){
        #Segement iris by fold using the which() function 
        testIndexes <- which(folds==i,arr.ind=TRUE)
        testData <- dataAtt[testIndexes, ]
        trainData <- dataAtt[-testIndexes, ]
        testClass <- dataClass[testIndexes]
        trainClass <- dataClass[-testIndexes]
        # call pcaLoader
        # switch from pca : prcomp, pcal1,l2pca,Pcaproj,PCAgrid,l1pca,l1pcastar,l1pcahp
        #pca
        pcaLoad <- pcaLoader(trainData, nComp, pcaName)
        trainPca <- data.matrix(trainData) %*% pcaLoad$loadings
        testPca <- data.matrix(testData) %*% pcaLoad$loadings

        # function return testpca and trainpca
        trainMatrix <- cbind(trainPca, trainClass)
        testMatrix <- cbind(testPca, testClass)
        #Apply knn with k = 1
        # load classification loader return accuracy
    
        predict <- knn(train = data.matrix(trainMatrix[,1:nComp]), test = data.matrix(testMatrix[,1:nComp]), cl = trainMatrix[,nComp+1],k=1)
        #calculate accuracy        
       output_acc[nComp - lowDim + 1, k, i] <- mean(predict==testMatrix[,nComp+1])
        
      }
     
    }
   
  }
   output_acc
}
