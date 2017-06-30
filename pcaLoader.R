pcaLoader <- function (trainData, nComp, pcaName)
{
  library(class) #Has the knn function
  library(pcaL1)
  library(pcaPP)
  library(stats)
  pca <- new.env()
  # switch from pca : prcomp, pcal1,Pcaproj,PCAgrid,l1pca,l1pcastar,l1pcahp
  switch(pcaName,  
         prcomp = {#prcomp
          pca <- prcomp(trainData, center = FALSE, scale. = FALSE)#prcomp
          pca$loadings <- pca$rotation[,1:nComp]
          pca$name <- "prcomp" 
        },
          pcal1 = {#pcal1
            pca <- pcal1(trainData,projDim = nComp)#pcal1
            pca$name <- "pcal1" 
        },
          Pcaproj = {#Pcaproj
            pca <- Pcaproj(trainData,projDim = nComp)#Pcaproj
            pca$name <- "Pcaproj" 
          },
          PCAgrid = {#PCAgrid
            pca <- PCAgrid(trainData,projDim = nComp)#PCAgrid
            pca$name <- "PCAgrid" 
          },
          l1pca = {#l1pca
              pca <- l1pca(trainData,projDim = nComp)#l1pca
              pca$name <- "pcal1" 
            },
          l1pcastar = {#l1pcastar
            pca <- l1pcastar(trainData,projDim = nComp)#l1pcastar
            pca$name <- "l1pcastar" 
          },
          l1pcahp = {#l1pcahp
              pca <- l1pcahp(trainData,projDim = nComp)#l1pcahp
              pca$name <- "pcal1" 
            }
     
         )
  pca
}
  