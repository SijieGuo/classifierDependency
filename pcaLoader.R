pcaLoader <- function (trainData, nComp, pcaName)
{
  pca <- new.env()
  # switch from pca : prcomp, pcal1,Pcaproj,PCAgrid,l1pca,l1pcastar,l1pcahp
  switch(pcaName,  
         prcomp = {#prcomp
           library(stats)
           pca <- prcomp(trainData, center = FALSE, scale. = FALSE)#prcomp
           pca$loadings <- pca$rotation[,1:nComp]
           pca$name <- "prcomp" 
         },
         pcal1 = {#pcal1
           library(pcaL1)
           pca <- pcal1(trainData,projDim = nComp)#pcal1
           pca$name <- "pcal1" 
         },
         PCAproj = {#PCAproj
           library(pcaPP)
           pca <- PCAproj(trainData,k = nComp)#Pcaproj
           pca$name <- "PCAproj" 
         },
         PCAgrid = {#PCAgrid
           library(pcaPP)
           pca <- PCAgrid(trainData,projDim = nComp)#PCAgrid
           pca$loadings <- pca$loadings[,1:nComp]
           pca$name <- "PCAgrid" 
         },
         l1pca = {#l1pca
           library(pcaL1)
           pca <- l1pca(trainData,projDim = nComp)#l1pca
           pca$name <- "pcal1" 
         },
         l1pcastar = {#l1pcastar
           library(pcaL1)
           pca <- l1pcastar(trainData,projDim = nComp)#l1pcastar
           pca$loadings <- pca$loadings[,1:nComp]
           pca$name <- "l1pcastar" 
         },
         l1pcahp = {#l1pcahp
           library(pcaL1)
           pca <- l1pcahp(trainData,projDim = nComp)#l1pcahp
           pca$name <- "l1pcahp" 
         },
         { 
           print('\nMethod not found\n')
         }
         
  )
  pca
}
