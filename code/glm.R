if(!require("doParallel"))install.packages("doParallel")
if(!require("reshape2"))install.packages("reshape2")
if(!require("ggplot2"))install.packages("ggplot2")
if(!require("ggthemes"))install.packages("ggthemes")
if(!require("glmnet"))install.packages("glmnet")

source("code/library.R")

#get data for knn
train.data <- get.train.data()
labels <- as.factor(train.data[[1]])
features <- train.data[[2]]
features.std <- train.data[[3]]
features.std[,10:53]<-as.data.frame(apply(features.std[,10:53],2,as.factor))


#Try Lasso,  ElasticNet and KernelElasticNet
# lambda 0 is LASSO, alpha 1 is Ridge
model<-glmnet(x=as.matrix(features.std),y=labels,family="multinomial",lambda=0)
#show selected variables
model$beta
plot(model)

#save plot
jpeg(filename = 'plots/glm_elnet_05.jpg', units = "in", width = 5, height = 5, res = 400)
plot.glmnet(model)
dev.off()


############ optimization of Lambda and Alpha

alphasOfInterest<-seq(0,1,by=0.1) #or something similar
#step 1: do all crossvalidations for each alpha

#set up cluster
cores <- detectCores()
cl <- makeCluster(cores)
clusterExport(cl, list("labels", "features", "features.std", "cv.glmnet", 
                       "glmnet", "lambda","alphasOfInterest"), envir = environment())
registerDoParallel(cl)

# lambda 0 is LASSO, alpha 1 is Ridge

cvs<-parLapply(alphasOfInterest, function(curAlpha){
    cv.glmnet(x=as.matrix(features.std),y=labels,family="multinomial",
              alpha=curAlpha,parallel=T)
})

stopCluster(cl)


#step 2: collect the optimum lambda for each alpha
optimumPerAlpha<-sapply(seq_along(alphasOfInterest), function(curi){
    curcvs<-cvs[[curi]]
    curAlpha<-alphasOfInterest[curi]
    indOfMin<-match(curcvs$lambda.min, curcvs$lambda)
    c(lam=curcvs$lambda.min, alph=curAlpha, cvup=curcvs$cvup[indOfMin])
})

#step 3: find the overall optimum
posOfOptimum<-which.min(optimumPerAlpha["lam",])
overall.lambda.min<-optimumPerAlpha["lam",posOfOptimum]
overall.alpha.min<-optimumPerAlpha["alph",posOfOptimum]
overall.criterionthreshold<-optimumPerAlpha["cvup",posOfOptimum]

# Step 4: Run with the optimal one
model2<-glmnet(x=as.matrix(features.std),y=labels,family="multinomial",
               alpha=overall.alpha.min,lambda=overall.lambda.min)

#show selected variables
model2$beta
model2$cvm #errors
model2$nzero
model2$lambda.min
plot(model2)


# Step 5: Predict
test.data <- get.test.data()
test.feat.std <- test.data[[2]]

predict<-predict(model2,as.matrix(test.feat.std),type="class")
prob<-predict(model2,as.matrix(test.feat.std),type="response")

id <- read.csv(file = "data/Kaggle_Covertype_test_id.csv", header = T)[,1] 

prediction <- data.frame(id =id, Cover_Type = as.vector(predict))
write.csv(x = prediction, file = "data/glm/glm_test_prediction.csv", row.names = FALSE)

probs<- data.frame(id =id, Cover_Type = as.vector(prob))
write.csv(x = probs, file = "data/glm/glm_test_probs.csv", row.names = FALSE)

