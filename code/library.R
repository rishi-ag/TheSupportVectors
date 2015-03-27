library("dplyr")
library("psych")

#set WD
#setwd("C:/Users/Rishabh/Documents/GitHub/TheSupportVectors/")
# setwd("D:/master/kaggle/TheSupportVectors")


rescale.features <- function(train.feat, test.feat) {
  #function resclaes all continuous features from -1 to 1
  .rescale.col <- function(x, old.min, old.max, new.min = -1, new.max = 1) {
    old.range = old.max - old.min
    new.range = new.max - new.min
    new.value = (((x - old.min) * new.range) / old.range) + new.min
    new.value
  }
  
  .rescale.data <- function(feat, min, max) {
    
    feat.scale <- c()
    
    for (i in 1:8) {
      cols <- c(1, 4:10)
      feat.scale <- cbind(feat.scale, .rescale.col(feat[,cols[i]], min[i], max[i]))
    }
    
    colnames(feat.scale)[1:8] <- names(feat)[c(1, 4:10)]
    
    #covert aspect and slope to sin and cos representations
    feat.scale <- cbind(feat.scale, sin(feat[,2] * (pi/180)),
                        cos(feat[,2] * (pi/180)), 
                        sin(feat[,3] * (pi/180)),
                        cos(feat[,3] * (pi/180)))
    
    colnames(feat.scale)[9:12] <- c("aspect.sin", "aspect.cos", "slope.sin", "slope.cos")
    
    feat.scale <- cbind(feat.scale, feat[,11:ncol(feat)])
    
    return(feat.scale)
  }
  
  
  d1 <- describe(train.feat[,c(1, 4:10)])
  d2 <- describe(test.feat[,c(1, 4:10)])
  
  min.max <- data.frame(train.min = d1$min, test.min = d2$min,
                        train.max = d1$max, test.max = d2$max)
  
  min <- apply(min.max, 1, function(x) min(x))
  max <- apply(min.max, 1, function(x) max(x))
  
  train.feat.scale <- .rescale.data(train.feat, min, max)
  test.feat.scale <- .rescale.data(test.feat, min, max)
  
  return(list(train.feat.scale, test.feat.scale))
} 



preprocess <- function() {
  #Function produces three files. One where A test label file. Two features file. One where 
  #Read Data
  train.data <- read.csv(file = "data/Kaggle_Covertype_training.csv", nrows = 50000, header = T)
  test.data <- read.csv(file = "data/Kaggle_Covertype_test.csv", header = T, nrows = 100000)
  
  #Process and standardise test and train data 
  #Seperate labels and features
  train.labels <- factor(select(train.data, Cover_Type)[,1])
  
  #remove labels and id
  train.features <- select(train.data, -Cover_Type, -id)
  train.features.std <- train.features
  test.features <- select(test.data, -id)
  test.features.std <- test.features
  
  #Standardise function
  #Convert aspect to radians
  features.train.std[,2] <- features.train.std[,2] * (pi/180)
  features.test.std[,2] <- features.test.std[,2] * (pi/180)
  
  #There are 10 quantitative variables. Standardise them. Aspect converted to sin and cos
  features.train.std[,c(1, 3:10)] <- apply(features.train.std[,c(1, 3:10)], 2, function(x) (x - mean(x)) / sd(x))
  features.train.std$sin<-sin(features.train.std[,2])
  features.train.std$cos<-cos(features.train.std[,2])
  
  features.test.std[,c(1, 3:10)] <- apply(features.test.std[,c(1, 3:10)], 2, function(x) (x - mean(x)) / sd(x)) 
  features.test.std$sin<-sin(features.test.std[,2])
  features.test.std$cos<-cos(features.test.std[,2])

  #rename standardised variables
  names(train.features.std)[1:10] <- paste0(names(train.features.std)[1:10], "_std")
  names(test.features.std)[1:10] <- paste0(names(test.features.std)[1:10], "_std")
  
  #rescale features data from -1 to 1
  feat.list <- rescale.features(train.feat = train.features, test.feat = test.features)
  train.features.scale <- feat.list[[1]]
  test.features.scale <- feat.list[[2]]
  
  #remove aspect var
  features.train.std <- select(features.train.std, -aspect_std)
  features.test.std <- select(features.test.std, -aspect_std)

  #save files to /data
  write.csv(x = train.features.std, file = "data/train_features_std.csv", row.names = FALSE)
  write.csv(x = train.labels, file = "data/train_labels.csv", row.names = FALSE)
  write.csv(x = train.features, file = "data/train_features.csv", row.names = FALSE)
  write.csv(x = train.features.scale, file = "data/train_features_scale.csv", row.names = FALSE)
  write.csv(x = test.features, file = "data/test_features.csv", row.names = FALSE)
  write.csv(x = test.features.std, file = "data/test_features_std.csv", row.names = FALSE)
  write.csv(x = test.features.scale, file = "data/test_features_scale.csv", row.names = FALSE)
  
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
  
  features.scale <- read.csv("data/train_features_scale.csv", header = T)
  
  return(list(labels, features, features.std, features.scale))
}

get.test.data <- function() {
  features <- read.csv("data/test_features.csv", header = T)
  
  features.std <- read.csv("data/test_features_std.csv", header = T)
  
  features.scale <- read.csv("data/test_features_scale.csv", header = T)
  
  return(list(features, features.std, features.scale))
}