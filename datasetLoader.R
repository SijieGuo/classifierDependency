datasetLoader <- function (datasetName)
{
  #datasets:  1."australian" 2. "balance" 3. "breastcancer" 4. "dermatology" 5. "heart" 6. "ionsphere" 
  #7."iris" 8."liver" 9. "sonar" 10. "vehicle" 11. "waveform" 12. "yeast" 13. "letter"
  #dataset is called by name 
  # for each dataset there are 3 features: attributes, class and name
  dataset <- new.env()
  switch(datasetName,
         #1.australian dataset
         australian={
          dataLoad <- read.csv(file="~/analysis/Rcode/australian.data",header=FALSE,sep=" ")
          dataset$attributes <- sapply(dataLoad[complete.cases(dataLoad),1:14],as.numeric)
          dataset$class <- dataLoad[complete.cases(dataLoad),15]
          dataset$name <- "australian"
         },
         #2. balance dataset
         balance={ 
           dataLoad <-read.csv(file="~/analysis/Rcode/balance-scale.data",header=FALSE,sep=",")
           dataset$attributes <- sapply(dataLoad[complete.cases(dataLoad),2:5],as.numeric)
           dataset$class <- dataLoad[complete.cases(dataLoad),1]
           dataset$name <- "balance"
         },
         
         #3. breastcancer dataset
         breastcancer={
           library(mlbench)
           data(BreastCancer)
           dataset$attributes <- sapply(BreastCancer[complete.cases(BreastCancer),2:10],as.numeric)
           dataset$class <- BreastCancer[complete.cases(BreastCancer),11]
           dataset$name <- "breastcancer"
         },
         
         #4. dermatology dataset
         dermatology ={
           dataLoad <-read.csv(file="~/analysis/Rcode/dermatology.data",header=FALSE,sep=",")
           dataset$attributes <- sapply(dataLoad[complete.cases(dataLoad),1:34],as.numeric)
           dataset$class <- dataLoad[complete.cases(dataLoad),35]
           dataset$name <- "dermatology"
         },
         
         #5. heart dataset 
         heart={
           dataLoad <-read.csv(file="~/analysis/Rcode/processed.cleveland.data",header=FALSE,sep=",")
           dataset$attributes <- sapply(dataLoad[complete.cases(dataLoad),1:13],as.numeric)
           dataset$class <- dataLoad[complete.cases(dataLoad),14]
           dataset$name <- "heart"
         },
         
         #6. ionosphere dataset
         ionosphere={
           library(mlbench)
           data(Ionosphere)
           dataset$attributes <- Ionosphere[complete.cases(Ionosphere),3:34]
           dataset$class <- Ionosphere[complete.cases(Ionosphere),35]
           dataset$name <- "ionosphere"
         },
         
         #7. iris dataset
         iris={
           library(class)
           data(iris)
           dataset$attributes <- iris[complete.cases(iris),1:4]
           dataset$class <- iris[complete.cases(iris),5]
           dataset$name <- "iris"
         },
         
         #8. liver dataset
         liver={
           dataLoad <-read.csv(file="~/analysis/Rcode/bupa.data",header=FALSE,sep=",")
           dataset$attributes <- sapply(dataLoad[complete.cases(dataLoad),1:6],as.numeric)
           dataset$class <- dataLoad[complete.cases(dataLoad),7]
           dataset$name <- "liver"
         },
         
         #9. sonar dataset
         sonar={
           library(mlbench)
           data(Sonar)
           dataset$attributes <- Sonar[complete.cases(Sonar),1:60]
           dataset$class <- Sonar[complete.cases(Sonar),61]
           dataset$name <- "sonar"
         },
         
         #10. vehicle dataset
         vehicle={
           library(mlbench)
           data(Vehicle)
           dataset$attributes <- Vehicle[complete.cases(Vehicle),1:18]
           dataset$class <- Vehicle[complete.cases(Vehicle),19]
           dataset$name <- "vehicle"
         },
         
         #11. waveform dataset
         waveform={
           dataLoad <-read.csv(file="~/analysis/Rcode/waveform-+noise.data",header=FALSE,sep=",")
           dataset$attributes <- sapply(dataLoad[complete.cases(dataLoad),1:21],as.numeric)
           dataset$class <- dataLoad[complete.cases(dataLoad),41]
           dataset$name <- "waveform"
         },
         
         #12. yeast dataset
         yeast ={
           dataLoad <-read.csv(file="~/analysis/Rcode/yeast.data",header=FALSE,sep=" ")
           dataset$attributes <- sapply(dataLoad[,2:18],as.numeric)
           dataset$class <- dataLoad[,19]
           dataset$name <- "yeast"
         },
         
         #13. letter dataset
         letter={
           library(mlbench)
           data(LetterRecognition)
           dataset$attributes <- LetterRecognition[complete.cases(LetterRecognition),2:17]
           dataset$class <- LetterRecognition[complete.cases(LetterRecognition),1]
           dataset$name <- "letter"
         }
  )
  dataset

}
