
compPCA <- function(dataset, pca1, pca2, minDims,maxDims)
{
  #load functions
  source("./pcaAccuracy.R")
  pcaFirst <- pcaAccuracy(pca1, dataset, minDims, maxDims)
  pcaSecond <- pcaAccuracy(pca2, dataset, minDims, maxDims)
  difference <- pcaFirst - pcaSecond
  difference 
}
