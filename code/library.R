library("dplyr")

#set WD
#setwd("C:/Users/Rishabh/Documents/GitHub/TheSupportVectors/")
# setwd("D:/master/kaggle/TheSupportVectors")

std.ang <- function(x) {
  #function to standrdize angular data 
  m.sin <- mean(sin(x))
  m.cos <- mean(cos(x))
  
  m.x <- atan(m.sin/m.cos)
  
  R <- sqrt(m.sin^2 + m.cos^2)
  
  s.d <- sqrt(-2 * log(R))
  
  return((x - m.x) / s.d)
}

preprocess <- function() {
  #Function produces three files. One where A test label file. Two features file. One where 
  #Read Data
  data.train <- read.csv(file = "data/Kaggle_Covertype_training.csv", nrows = 50000, header = T)
  data.test <- read.csv(file = "data/Kaggle_Covertype_test.csv", header = T, nrows = 100000)
  
  #Process and standardise test and train data 
  #Seperate labels and features
  labels.train <- factor(select(data.train, Cover_Type)[,1])
  
  #remove labels and id
  features.train <- select(data.train, -Cover_Type, -id)
  features.train.std <- features.train
  features.test <- select(data.test, -id)
  features.test.std <- features.test
  
  #Standardise function
  #Convert slope and aspect to radians
  features.train.std[,2:3] <- features.train.std[,2:3] * (pi/180)
  features.test.std[,2:3] <- features.test.std[,2:3] * (pi/180)
  
  #There are 10 quantitative variables. Standardise them
  features.train.std[,2:3] <- apply(features.train.std[,2:3], 2, function(x) std.ang(x))
  features.train.std[,c(1, 4:10)] <- apply(features.train.std[,c(1, 4:10)], 2, function(x) (x - mean(x)) / sd(x))
  features.test.std[,2:3] <- apply(features.test.std[,2:3], 2, function(x) std.ang(x))
  features.test.std[,c(1, 4:10)] <- apply(features.test.std[,c(1, 4:10)], 2, function(x) (x - mean(x)) / sd(x)) 
  
  #rename standardised variables
  names(features.train.std)[1:10] <- paste0(names(features.train.std)[1:10], "_std")
  names(features.test.std)[1:10] <- paste0(names(features.test.std)[1:10], "_std")
  
  #save files to /data
  write.csv(x = features.train.std, file = "data/train_features_std.csv", row.names = FALSE)
  write.csv(x = labels.train, file = "data/train_labels.csv", row.names = FALSE)
  write.csv(x = features.train, file = "data/train_features.csv", row.names = FALSE)
  write.csv(x = features.test, file = "data/test_features.csv", row.names = FALSE)
  write.csv(x = features.test.std, file = "data/test_features_std.csv", row.names = FALSE)
}

compare.k <- function(k, train, label) {
  #compare errors for different k on features
  
  knn.sum <- knn.cv(train = train, cl = label, k = k)
  error <- sum(knn.sum != label) / length(label)
  return(error)
}


get.train.data <- function() {
  labels <- read.csv("data/train_labels.csv", header = T,
                     nrows = 50000)[,1]
  features <- read.csv("data/train_features.csv", header = T, nrows = 50000)
  
  features.std <- read.csv("data/train_features_std.csv", header = T, nrows = 50000)
  
  return(list(labels, features, features.std))
}

get.test.data <- function() {
  features <- read.csv("data/test_features.csv", header = T)
  
  features.std <- read.csv("data/test_features_std.csv", header = T)
  
  return(list(features, features.std))
}