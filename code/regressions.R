if(!require("doParallel"))install.packages("doParallel")
if(!require("reshape2"))install.packages("reshape2")
if(!require("ggplot2"))install.packages("ggplot2")
if(!require("ggthemes"))install.packages("ggthemes")
if(!require("glmnet"))install.packages("glmnet")

source("code/library.R")

#get data for knn
train.data <- get.train.data()
labels <- train.data[[1]]
features <- train.data[[2]]
features.std <- train.data[[3]]

#Try Lasso,  ElasticNet and KernelElasticNet
# lambda 0 is LASSO, alpha 1 is Ridge
model<-glmnet(x=as.matrix(features.std),y=labels,family="multinomial",lambda=0.5)
#show selected variables
model$beta
plot.glmnet(model)

############ optimization of Lambda

#set up cluster
cores <- detectCores()
cl <- makeCluster(cores)
clusterExport(cl, list("labels", "features", "features.std", "compare.k", "class", "knn.cv"), envir = environment())
registerDoParallel(cl)

# lambda 0 is LASSO, alpha 1 is Ridge
lambda<-seq(0,1,length.out=16)
model2<-cv.glmnet(x=as.matrix(features.std),y=labels,family="multinomial",
                  lambda=lambda,parallel=T)
stopCluster(cl)

#show selected variables
model2$beta
model2$cvm #errors
model2$nzero
model2$lambda.min
plot.cv.glmnet(model2)


