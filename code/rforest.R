if(!require("randomForest"))install.packages("randomForest")
if(!require("doParallel"))install.packages("doParallel")
if(!require("reshape2"))install.packages("reshape2")
if(!require("ggplot2"))install.packages("ggplot2")
if(!require("caret"))install.packages("caret")
if(!require("dplyr"))install.packages("dplyr")
if(!require("ggthemes"))install.packages("ggthemes")
if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")


# setwd("D:/master/kaggle/TheSupportVectors")
source("code/library.R")

# Function for random forest
compare.k <- function(k, train, label) {
    #compare errors for different k maximum variables on features
    model <- randomForest(x = train, y = label, mtry = k)
    # error rate is computed from out of bag elements, so it's a cross-val error
    rfor.error<-sum(model$predicted!=label)/length(label)
    return(model$err.rate[dim(model$err.rate)[1],])
}


#get data for rforest
train.data <- get.train.data()
labels <- as.factor(train.data[[1]])
features <- train.data[[3]] #standarized data

features[,10:53]<-as.data.frame(apply(features[,10:53],2,as.factor))

labels1<-as.factor(1*(labels==1))

#subset data
#features<-features[1:5000,]
#labels<-labels[1:5000]

#one model
#model <- randomForest(x = features, y = labels, mtry = 12)

# Bounds for mtry parameter (max of variables per step)
mink<-round(sqrt(dim(features)[2]))
maxk<-round(dim(features)[2]/1.5)
#mink<-28
#maxk<-55

# Initial check
#set up cluster
cores <- detectCores()
cl <- makeCluster(cores)
clusterExport(cl, list("labels1", "features","mink","maxk",
                       "compare.k", "randomForest"), envir = environment())
registerDoParallel(cl)


#call function to compare k error rates on features and standardised features
system.time(rfor.perf<- parSapply(cl, seq(mink,maxk, 2), 
                                  function(x) compare.k(x, features, labels1)))

stopCluster(cl)




#create and write error data frame to file 
comparison <- data.frame(k = seq(mink, maxk, 2), feat.err = t(rfor.perf))

write.csv(x = comparison, file = "data/rfor/rfor_data_error2.csv")

#read comparison file
comparison <- read.csv(file = "data/rfor/rfor_data_error2.csv", header = T)[,-1]

#melt daa for graph
comparison.long <- melt(data = comparison, id.vars = "k", 
                        measure.vars = c("feat.err.OOB","feat.err.1","feat.err.2","feat.err.3",
                                         "feat.err.4","feat.err.5","feat.err.6","feat.err.7"),
                        variable.name = "feature_type", value.name = "error")



#plot
plot1 <- ggplot(data = comparison.long, aes(x = k,y = error, color = feature_type )) +
    geom_line(size = 0.5) + 
    geom_point(size = 3) + 
    scale_x_continuous(breaks= seq(mink,maxk,2)) + 
    ggtitle("Mtry error ") +
    scale_color_wsj(name  ="Feature Type", labels=levels(comparison.long$feature_type))

#save plot
jpeg(filename = 'plots/RforMtryErrorFeature2.jpg', units = "in", width = 5, height = 5, res = 400)
plot1
dev.off()


# predict using best k on best data set
best.k = comparison$k[which.min(comparison$feat.err.OOB)]
test.data <- get.test.data()
test.feat <- test.data[[2]]
test.feat[,10:53]<-as.data.frame(apply(test.feat[,10:53],2,as.factor))
model <- randomForest(x = features, y = labels, mtry = best.k)
predict<-predict(model,test.feat)

id <- read.csv(file = "data/Kaggle_Covertype_test_id.csv", header = T)[,1] 

prediction <- data.frame(id =id, Cover_Type = predict)
write.csv(x = prediction, file = "data/rfor/rfor_test_prediction2.csv", row.names = FALSE)

saveRDS(model,"rforest_model2.rds")
# model1: k=28 optim4to28
# model2: k=34 optim28to54

#### make models for each specific label
best.k<-34
nlabels<-length(levels(labels))
models<-list()
predicts<-list()
probs<-list()
i<-1:nlabels
#set up cluster
cores <- detectCores()
cl <- makeCluster(cores, type="SOCK", outfile="")
registerDoSNOW(cl)

models<-foreach(i = i,
                .packages=c("randomForest")) %dopar% {
                    label<-as.factor(i*(labels==levels(labels)[i]))
                    randomForest(x = features, y = label, mtry = best.k)
                }
predicts<-foreach(i = i,
                  .packages=c("randomForest")) %dopar% {
                      predict(models[[i]], test.feat)
                  }
probs<-foreach(i = i,
                  .packages=c("randomForest")) %dopar% {
                      predict(models[[i]], test.feat,type="prob")
}

stopCluster(cl)

saveRDS(models,"rforest_models.rds")
saveRDS(predicts,"rforest_predicts.rds")
saveRDS(probs,"rforest_probs.rds")



probs<-readRDS("data/rforest_probs.rds")

probs_class<-matrix(NA,dim(test.feat)[1],length(levels(labels)))
for (i in 1:7){
    probs_class[,i]<-probs[[i]][,2]
}
predict<-apply(probs_class,1,which.max)
predict_prob<-apply(probs_class,1,max)

id <- read.csv(file = "data/Kaggle_Covertype_test_id.csv", header = T)[,1] 

prediction <- data.frame(id =id, Cover_Type = predict)
write.csv(x = prediction, file = "data/rfor/rfor_test_prediction_agg.csv", row.names = FALSE)



