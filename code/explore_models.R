## Course project machine learning, Data Science 

if(!require("randomForest"))install.packages("randomForest")
if(!require("reshape2"))install.packages("reshape2")
if(!require("ggplot2"))install.packages("ggplot2")
if(!require("caret"))install.packages("caret")
if(!require("dplyr"))install.packages("dplyr")
if(!require("ggthemes"))install.packages("ggthemes")
if (!require("caret")) install.packages("caret")
if (!require("rattle")) install.packages("rattle")
if (!require("rpart.plot")) install.packages("rpart.plot")
if (!require("e1071")) install.packages("e1071")
if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")


## Read data
setwd("D:/master/kaggle/TheSupportVectors")
source("code/library.R")

# funtion to unify related factor data
unify<-function(data,vector,text){
    nobs<-dim(data)[1]
    out<-rep(0,nobs)
    for (j in 1:nobs){
        for (i in seq_along(vector)){
            out[j]<-paste0(out[j],data[[paste0(text,as.character(vector[i]))]][j])
        }
    }
    return(as.factor(out))
}

#get data for rforest
train.data <- get.train.data()
labels <- as.factor(train.data[[1]])
features <- train.data[[3]] #standarized data
#features[,10:53]<-as.data.frame(apply(features[,10:53],2,as.factor))
test.data <- get.test.data()
test.feat <- test.data[[2]] #standarized data
#test.feat[,10:53]<-as.data.frame(apply(test.feat[,10:53],2,as.factor))

prova<-unify(features,1:40,"soil_type_")
area<-unify(features,1:4,"wild_area_")
features<-features[,c(1:9,54,55)]
features$soil<-prova
features$area<-area


# PCA
pca<-prcomp(features)
features<-pca$x
vectors<-pca$rotation
var<-diag(cov(features))/sum(diag(cov(features)))
cumvar<-cumsum(var)
barplot(cumvar)
nvars<-sum(cumvar<0.99)
sum(cumvar<0.95)
# Get only the 99% variance variables from PCA variables
features<-features[,1:nvars]
vectors<-vectors[,1:nvars]

#Subsample
sample<-createDataPartition(y=labels,p=0.01,list=FALSE)#sample preserving proportion of classes
features<-features[sample,]
labels<-labels[sample]

## data partition to select model
#partition preserving proportion of classes
subtrain<-createDataPartition(y=labels,p=0.8,list=FALSE)
trainstrain<-features[subtrain,]
trainstest<-features[-subtrain,]
trainset<-data.frame(labels=labels[subtrain],trainstrain)
testset<-data.frame(labels=labels[-subtrain],trainstest)

# Chech different models
mod_names<-c('Bagged AdaBoost', 'Bagged CART', 'Bagged Flexible Discriminant Analysis','Bagged Logic Regression','Model Averaged Neural Network','Random Forest','Boosted Classification Trees','Boosted Generalized Linear Model','Boosted Logistic Regression','Boosted Tree','Support Vector Machines with Class Weights','High Dimensional Discriminant Analysis','Linear Discriminant Analysis','Oblique Random Forest','Parallel Random Forest','glmnet','Least Squares Support Vector Machine with Polynomial Kernel','Least Squares Support Vector Machine with Radial Basis Function Kernel','Adaptive Mixture Discriminant Analysis','Logistic Model Trees','Neural Network','k-Nearest Neighbors')
mod_code<-c('AdaBag', 'treebag', 'bagFDA','logicBag','avNNet','rf','ada','glmboost','LogitBoost','bstTree','svmRadialWeights','hdda','lda','ORFsvm','parRF','glmnet','lssvmPoly','lssvmRadial','amdai','LMT','nnet','knn')
set.seed(10)

# preparing the parallelization
noCores <- detectCores()-1
cl <- makeCluster(noCores, type="SOCK", outfile="") 
registerDoSNOW(cl)

models_outcome<-foreach(mod = 2:4, 
        .combine=rbind, .packages=c("caret")) %dopar% {
            
        cat("The current model is ", mod, "\n")
        # model
        model<-train(labels~.,data=trainset,method=mod_code[mod])
        pred<-predict(model,newdata=trainstest)
        res<-confusionMatrix(pred,testset$labels)
        result<-c(res$overall,res$byClass)
        }

stopCluster(cl)



### Try some models ####

## First quick models: multiple regression and tree

# Tree
model1<-train(labels~.,data=trainset,method="rpart")
pred1<-predict(model1,newdata=trainstest)
library(rattle)
fancyRpartPlot(model1$finalModel)

res1<-confusionMatrix(pred1,testset$labels)
res1
# out: 61% accuracy 

## LDA

model2 <- train(labels~.,data=trainset,method="lda")
out2<-confusionMatrix(model2)$table
pred2<-predict(model2,newdata=trainstest)
res2<-confusionMatrix(pred2,testset$labels)
# out: 66%, 94% for class 4

#  QDA

model3 <- train(labels~.,data=trainset,method="qda")
pred3<-predict(model3,newdata=trainstest)
out3<-confusionMatrix(model3)$table
res3<-confusionMatrix(pred3,testset$labels)
## out acc

# random forest

model4<-train(labels~.,data=trainset,method="rf")
pred4<-predict(model4,newdata=trainstest)
out4<-confusionMatrix(model4)$table
res4<-confusionMatrix(pred4,trainstest$classe)
# acc 0.993
varImp(model4)
vars<-varImp(model4)$importance
vars$names<-rownames(vars)
vars<-vars[order(vars[,1],decreasing=T),]

names<-vars$names[vars[,1]>8]
names2<-c(names,"classe") # add output

# plot accuracy by class and model
model1<-c(res1$overall[1],res1$byClass[,8])
model2<-c(res2$overall[1],res2$byClass[,8])
model3<-c(res3$overall[1],res3$byClass[,8])
model4<-c(res4$overall[1],res4$byClass[,8])

accuracy<-data.frame(tree=model1,lda=model2,qda=model3,rforest=model4)

library(reshape2)
accuracy$Accuracy<-row.names(accuracy)
accuracy$Accuracy[1]<-"Overall"
ac.long<-melt(accuracy)
names(ac.long)<-c("Type","Model","Accuracy")
ggplot(data=ac.long, aes(y=Accuracy,x=Model, fill=Type)) + 
    geom_bar(stat="identity", position=position_dodge())+
    scale_fill_manual(values=c("red", "blue","orange","green","purple","black"))




