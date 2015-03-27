
setwd("~/GitHub/TheSupportVectors")
source("code/library.R")






train <- get.train.data()
str(train)
train.label <- train[[1]]
#labels_df <- as.data.frame(train.label)
train.feat <- train[[2]]
train.feat_st <- train[[3]]




# We use the prcomp function which calculation is done by a singular value decomposition
#  of the (centered) data matrix. Although the topic is quite ambiguous, it seems like
# the specific technique is appropriate for handling binary (categorical) data.
pca <- prcomp(x=train.feat_st)

?prcomp

retr_values <- pca$x
dim(retr_values)
rot<-pca$rotation
covpca<-cov(retr_values)

sd_variables <- pca$sdev
sort(sd_variables, decreasing = TRUE)
Variability <- sum(sd_variables)
sum(sd_variables[1:20])

class(sd_variables)


