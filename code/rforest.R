if(!require("randomForest"))install.packages("randomForest")
if(!require("doParallel"))install.packages("doParallel")
if(!require("reshape2"))install.packages("reshape2")
if(!require("ggplot2"))install.packages("ggplot2")
if(!require("caret"))install.packages("caret")
if(!require("dplyr"))install.packages("dplyr")
if(!require("ggthemes"))install.packages("ggthemes")


# setwd("D:/master/kaggle/TheSupportVectors")
source("code/library.R")

# Function for random forest
compare.k <- function(k, train, label) {
    #compare errors for different k maximum variables on features
    model <- randomForest(x = train, y = label, mtry = k)
    # error rate is computed from out of bag elements, so it's a cross-val error
    rfor.error<-sum(model$predicted!=label)/length(label)
    return(rfor.error)
}

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
labels <- train.data[[1]]
features <- train.data[[2]]
features.std <- train.data[[3]]

#Read data
data.train <- read.csv(file = "data/Kaggle_Covertype_training.csv", nrows = 50000, header = T)
data.test <- read.csv(file = "data/Kaggle_Covertype_test.csv", header = T, nrows = 100000)

#Process and standardise test and train data 
#Seperate labels and features
labels.train <- factor(select(data.train, Cover_Type)[,1])

#remove labels and id
features.train <- select(data.train, -Cover_Type, -id)
features.test <- select(data.test, -id)
#### Reduce dimensions

# aspect to sin and cos
features.train$sin<-sin(features.train$aspect*pi/180)
features.train$cos<-cos(features.train$aspect*pi/180)
features.test$sin<-sin(features.test$aspect*pi/180)
features.test$cos<-cos(features.test$aspect*pi/180)


# Unify binary soil_types
features.train$soil<-unify(features.train,1:40,'soil_type_')
features.test$soil<-unify(features.test,1:40,'soil_type_')

# Unify binary areas
features.train$area<-unify(features.train,1:4,'wild_area_')
features.test$area<-unify(features.test,1:4,'wild_area_')

# Hill shade is redundant with aspect and slope, aspect no more useful
features.train<-subset(features.train, select=-c(hill_9am,hill_noon,hill_3pm,aspect))
features.test<-subset(features.test, select=-c())

# Final selection (hill eliminated in one case, seem redundant to aspect and slope)
features.train.red<-subset(features.train,select=c(elevation,slope,sin,cos,hor_dist_hyd,
            ver_dist_hyd,hor_dist_road,hor_dist_fire,soil,area))
features.train.unif<-subset(features.train,select=c(elevation,slope,sin,cos,hor_dist_hyd,
    ver_dist_hyd,hor_dist_road,hor_dist_fire,soil,area,hill_9am,hill_noon,hill_3pm))
features.test.red<-subset(features.test,select=c(elevation,slope,sin,cos,hor_dist_hyd,
                        ver_dist_hyd,hor_dist_road,hor_dist_fire,soil,area))


# Bounds for mtry parameter (max of variables per step)
mink<-round(sqrt(dim(features.train.red)[2])/2)
maxk<-round(dim(features.train.red)[2]/2)
#model<-randomForest(x = features.train.red, y = labels.train)
#error<-sum(model$predicted!=labels.train)/length(labels.train)

#set up cluster
cores <- detectCores()
cl <- makeCluster(cores-1)
clusterExport(cl, list("labels.train", "features.train.red","features.train.unif",
                       "compare.k", "randomForest"), envir = environment())
registerDoParallel(cl)


#call function to compare k error rates on features and standardised features
system.time(rfor.perf.red<- parSapply(cl, seq(2,5, 1), function(x) compare.k(x, features.train.red, labels.train)))
system.time(rfor.perf.unif <- parSapply(cl, seq(2,5, 1), function(x) compare.k(x, features.train.unif, labels.train)))

stopCluster(cl)

#create and write error data frame to file 
comparison <- data.frame(k = seq(mink, maxk, 2), red.feat.err = rfor.perf.red, 
                         unif.feat.err=rfor.perf.unif)

write.csv(x = comparison, file = "data/rfor/rfor_data_error.csv")

#read comparison file
comparison <- read.csv(file = "data/rfor/rfor_data_error.csv", header = T)[,-1]

#melt daa for graph
comparison.long <- melt(data = comparison, id.vars = "k", 
                        measure.vars = c("red.feat.err","unif.feat.err"),
                        variable.name = "feature_type", value.name = "error")



#plot
plot1 <- ggplot(data = comparison.long, aes(x = k,y = error, color = feature_type )) +
    geom_line(size = 0.5) + 
    geom_point(size = 3) + 
    scale_x_continuous(breaks= seq(mink,maxk,1)) + 
    ggtitle("Mtry error ") +
    scale_color_wsj(name  ="Feature Type", labels=c("Reduced and unified","Unified"))

#save plot
jpeg(filename = 'plots/RforMtryErrorFeature.jpg', units = "in", width = 5, height = 5, res = 400)
plot1
dev.off()


# predict using best k on best data set
best.k = 1
test.data <- get.test.data()
test.feat.raw <- test.data[[1]]
test.sum <- randomForest(x = train, y = label, mtry = best.k)

id <- read.csv(file = "data/Kaggle_Covertype_test.csv", header = T)[,1] 

prediction <- data.frame(id =id, Cover_Type = test.sum$predicted)
write.csv(x = prediction, file = "data/rfor/rfor_test_prediction.csv", row.names = FALSE)
