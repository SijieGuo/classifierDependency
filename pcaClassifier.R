pcaAccuracy <- function(pcaName, dataset, classifierName, lowDim, highDim)
{
  #call function: pcaAccuracy <- array(dataset,pca,classifiers,projDim(10),shuffle(10),folder(10))
  library(class) #Has the knn function
  library(pcaL1)
  library(pcaPP)
  library(stats)
  #install dependencies of caret
  #install.packages("caret", dependencies = c("Depends", "Suggests"))
  library(caret)
  library(gbm)
  library(randomForest)
  library(naivebayes)
  library(C50)
  source("./datasetLoader.R")
  source("./pcaLoader.R")
  #source("./classifierLoader.R")
  newData <- datasetLoader(dataset)
  #set seeds
  seeds <- c(1234, 1989, 290889, 251091, 240664, 190364, 120863, 101295, 31089, 3573113)
  
  dim_acc <- c()
  output_acc <- array(0, dim = c(highDim - lowDim + 1, 10, 10))
  for(nComp in lowDim:highDim){
    #avg_acc <- c()
    for(k in 1:length(seeds)){#shuffle 10 times
      set.seed(seeds[k])
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
        # switch from pca : prcomp, pcal1,Pcaproj,PCAgrid,l1pca,l1pcastar,l1pcahp
        #pca
        pcaLoad <- pcaLoader(trainData, nComp, pcaName)
        trainPca <- data.matrix(trainData) %*% pcaLoad$loadings
        testPca <- data.matrix(testData) %*% pcaLoad$loadings
        
        # function return testpca and trainpca
        trainMatrix <- cbind(trainPca, trainClass)
        testMatrix <- cbind(testPca, testClass)
        # Apply knn with k = 1
        # load classifier loader return accuracy
        #predict <- classifierLoader(trainMatrix, testMatrix, nComp, classifierName)
        #knn
        #predict <- knn(train = data.matrix(trainMatrix[,1:nComp]), test = data.matrix(testMatrix[,1:nComp]), cl = trainMatrix[,nComp+1],k=1)
        print(trainClass)
        print(trainPca)
        predict <- randomForest(trainClass,trainPca)
        #predict <- predict(prerf, testMatrix[,1:nComp])
        #randomForest
        #predict <- randomForest(testPca,testClass,prox = TRUE)
        #rf <- randomForest(trainPca[,1:nComp-1],trainClass[,1:nComp-1],prox = TRUE)
        #pr <- classCenter(trainPca[,1:nComp-1],trainClass[,1:nComp-1],rf$prox)
        
        #gbm
        
        
        #nb
        
        
        #C50
        
        
        
        #calculate accuracy 
        output_acc[nComp - lowDim + 1, k, i] <- mean(predict==testMatrix[,nComp+1])
        #output_acc[nComp - lowDim + 1, k, i] <- mean(pr==trainPca[,nComp-1])
        #pca_acc<-c(iris_acc,mean(predict==testMatrix[,nComp+1]))
      }
      #avg_acc <- c(avg_acc, mean(pca_acc))
    }
    #dim_acc <- c(dim_acc,mean(avg_acc))
    # dim_acc <- c(dim_acc, cbind(dim_acc, avg_acc))
  }
  output_acc
  #dim_acc
}
