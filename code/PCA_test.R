setwd("~/GitHub/TheSupportVectors")
source("code/library.R")

#Step 1: Getting the labels and the standardized features
train <- get.train.data()
train.label <- train[[1]]
train.feat_st <- train[[3]] # getting the standardized features of the training set


#Step2: PCA
#We use the prcomp function which calculation is done by a singular
#value decomposition of the (centered) data matrix. Although the topic is quite
#ambiguous, it seems like the specific technique is appropriate for handling
#binary (categorical) data.

pca <- prcomp(x=train.feat_st)

retr_values <- pca$x
rot<-pca$rotation
covpca<-cov(retr_values)

sd_variables <- pca$sdev
sd_variables <- sort(sd_variables, decreasing = TRUE)

Variability <- sum(sd_variables)

```

#Analysis of PCA results
"""
The prcomp function returns an object of class prcomp, which have some methods available.

The print method returns the standard deviation of each of the PCs, and their rotation (or loadings), which are the coefficients of the linear combinations of the continuous variables.

The plot method returns a plot of the variances (y-axis) associated with the PCs (x-axis). The Figure created is useful to decide how many PCs to retain for further analysis.we can see that the first two PCs explain most of the variability in the data.
"""
StDev_princomp <- print(pca)
plot(pca, type = "l")

summary(pca)
