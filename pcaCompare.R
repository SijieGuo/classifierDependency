
compPCA <- function(dataset, pca1, pca2, minDims,maxDims)
{
  #load functions
  source("D:/´óËÄÏÂ/analysis/R code/pcaAccuracy.R")
  #load library
  pcaFirst <- pcaAccuracy(pca1, dataset, minDims, maxDims)
  pcaSecond <- pcaAccuracy(pca2, dataset, minDims, maxDims)
  difference <- pcaFirst - pcaSecond
  difference 
}