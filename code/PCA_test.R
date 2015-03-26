
setwd("~/GitHub/TheSupportVectors")
source("code/library.R")

train <- get.train.data()
train.label <- train[[1]]
#labels_df <- as.data.frame(train.label)
train.feat <- train[[2]]
train.feat_st <- train[[3]]

pca <- prcomp(x=train.feat_st)
?prcomp

retr_values <- pca$x
dim(retr_values)
rot<-pca$rotation
covpca<-cov(retr_values)
