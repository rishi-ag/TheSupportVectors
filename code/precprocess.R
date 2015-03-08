library(dplyr)
library("psych")
library("")

#set WD
setwd("C:/Users/Rishabh/Documents/GitHub/TheSupportVectors/")

#Read Raw Data
data <- read.csv(file = "data/Kaggle_Covertype_training.csv", nrows = 50000, header = T)

#Seperate labels and features
labels <- factor(select(data, Cover_Type)[,1])

#remove labels and id
features <- select(data, -Cover_Type, -id)

#Standardise function
std.col <- function(x) (x - mean(x)) / sd(x) 
names(features)

#There are 10 quantitative variables. Standardise them
features[,1:10] <- apply(features[,1:10], 2, function(x) std.col(x)) 

#rename variables
names(features)[1:10] <- paste0(names(features)[1:10], "_std")

#save files to /data
write.csv(x = features, file = "data/features_std.csv")
write.csv(x = labels, file = "data/labels.csv")
