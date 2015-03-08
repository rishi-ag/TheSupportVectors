library(dplyr)

#set WD
setwd("C:/Users/Rishabh/Documents/GitHub/TheSupportVectors/")


preprocess <- function() {
  #Read Data
  data.train <- read.csv(file = "data/Kaggle_Covertype_training.csv", nrows = 50000, header = T)
  data.test <- read.csv(file = "data/Kaggle_Covertype_test.csv", header = T, nrows = 100000)
  
  #Process and standardise test and train data 
  #Seperate labels and features
  labels.train <- factor(select(data.train, Cover_Type)[,1])
  
  #remove labels and id
  features.train <- select(data.train, -Cover_Type, -id)
  features.test <- select(data.test, -id)
  #Standardise function
  
  
  #There are 10 quantitative variables. Standardise them
  features.train[,1:10] <- apply(features.train[,1:10], 2, function(x) (x - mean(x)) / sd(x))
  features.test[,1:10] <- apply(features.test[,1:10], 2, function(x) (x - mean(x)) / sd(x)) 
  
  #rename variables
  names(features.test)[1:10] <- paste0(names(features.test)[1:10], "_std")
  names(features.train)[1:10] <- paste0(names(features.train)[1:10], "_std")
  
  #save files to /data
  write.csv(x = features.train, file = "data/train_features_std.csv")
  write.csv(x = labels.train, file = "data/train_labels.csv")
  write.csv(x = features.test, file = "data/test_features_std.csv")
}

preprocess()
