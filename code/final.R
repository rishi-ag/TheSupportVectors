# Machine Learning Project, Support Vectors Group
# Rishabh Agnighotri, Anestis Papanikolaou, Joan Verdu

# This algorithm needs the following files to be previouly set into a /data
# subfolder: Kaggle_Covertype_training.csv, Kaggle_Covertype_test.csv

#---------------------#
### Package loading ###
#---------------------#

if(!require("doParallel"))install.packages("doParallel")
if(!require("reshape2"))install.packages("reshape2")
if(!require("ggplot2"))install.packages("ggplot2")
if(!require("caret"))install.packages("caret")
if(!require("dplyr"))install.packages("dplyr")
if(!require("ggthemes"))install.packages("ggthemes")
if (!require("caret")) install.packages("caret")
if (!require("psych")) install.packages("psych")
if(!require("modeest")) install.packages("modeest")

#--------------------#
### Functions     ####
#--------------------#

#function that rescales all continuous features from -1 to 1
rescale.features <- function(train.feat, test.feat) {
    
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


#Function produces three files. One where A test label file. Two features file. One where 
preprocess <- function() {
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
    
    #Standardise
    #Convert aspect to radians
    train.features.std[,2] <- train.features.std[,2] * (pi/180)
    test.features.std[,2] <- test.features.std[,2] * (pi/180)
    
    #There are 10 quantitative variables. Standardise them. Aspect converted to sin and cos
    train.features.std[,c(1, 3:10)] <- apply(train.features.std[,c(1, 3:10)], 2, function(x) (x - mean(x)) / sd(x))
    train.features.std$sin<-sin(train.features.std[,2])
    train.features.std$cos<-cos(train.features.std[,2])
    
    test.features.std[,c(1, 3:10)] <- apply(test.features.std[,c(1, 3:10)], 2, function(x) (x - mean(x)) / sd(x)) 
    test.features.std$sin<-sin(test.features.std[,2])
    test.features.std$cos<-cos(test.features.std[,2])
    
    #rename standardised variables
    names(train.features.std)[1:10] <- paste0(names(train.features.std)[1:10], "_std")
    names(test.features.std)[1:10] <- paste0(names(test.features.std)[1:10], "_std")
    
    #rescale features data from -1 to 1
    feat.list <- rescale.features(train.feat = train.features, test.feat = test.features)
    train.features.scale <- feat.list[[1]]
    test.features.scale <- feat.list[[2]]
    
    #remove aspect var
    train.features.std <- select(train.features.std, -aspect_std)
    test.features.std <- select(test.features.std, -aspect_std)
    
    #save files to /data
    write.csv(x = train.features.std, file = "data/train_features_std.csv", row.names = FALSE)
    write.csv(x = train.labels, file = "data/train_labels.csv", row.names = FALSE)
    write.csv(x = train.features, file = "data/train_features.csv", row.names = FALSE)
    write.csv(x = train.features.scale, file = "data/train_features_scale.csv", row.names = FALSE)
    write.csv(x = test.features, file = "data/test_features.csv", row.names = FALSE)
    write.csv(x = test.features.std, file = "data/test_features_std.csv", row.names = FALSE)
    write.csv(x = test.features.scale, file = "data/test_features_scale.csv", row.names = FALSE)
    
}


# Function to retrieve pre-processed train data
get.train.data <- function() {
    labels <- read.csv("data/train_labels.csv", header = T,
                       nrows = 50000)[,1]
    features <- read.csv("data/train_features.csv", header = T, nrows = 50000)
    
    features.std <- read.csv("data/train_features_std.csv", header = T, nrows = 50000)
    
    features.scale <- read.csv("data/train_features_scale.csv", header = T)
    
    return(list(labels, features, features.std, features.scale))
}

# Function to retrieve pre-processed test data
get.test.data <- function() {
    features <- read.csv("data/test_features.csv", header = T)
    
    features.std <- read.csv("data/test_features_std.csv", header = T)
    
    features.scale <- read.csv("data/test_features_scale.csv", header = T)
    
    return(list(features, features.std, features.scale))
}

# Function to compare errors for different k on features (knn classifier)
compare.k <- function(k, train, label) {
    
    
    knn.sum <- knn.cv(train = train, cl = label, k = k)
    error <- sum(knn.sum != label) / length(label)
    return(error)
}


# Function to optimize mtry parameter, random forest classifier
compare.mtry <- function(k, train, label) {
    #compare errors for different k maximum variables on features
    model <- randomForest(x = train, y = label, mtry = k)
    # error rate is computed from out of bag elements, so it's a cross-val error
    rfor.error<-sum(model$predicted!=label)/length(label)
    return(model$err.rate[dim(model$err.rate)[1],])
}

#----------------------------------------------------#
######    Main Algorithm                        ######
#----------------------------------------------------#

# Load and preprocess data (to be saved to /data folder)
preprocess()
# Retrieve standarized data
train.data <- get.train.data()
labels <- as.factor(train.data[[1]])
features <- train.data[[3]] #standarized data
features[,10:53]<-as.data.frame(apply(features[,10:53],2,as.factor))

# Uncheck this to run PCA and select PCAs that explain 99% of variance
        #pca<-prcomp(features)
        #features<-pca$x
        #vectors<-pca$rotation
        #var<-diag(cov(features))/sum(diag(cov(features)))
        #cumvar<-cumsum(var)
        #nvars<-sum(cumvar<0.99)
        # Get only the 99% variance variables from PCA variables
        #features<-features[,1:nvars]
        #vectors<-vectors[,1:nvars]

#-----------------------#
## Random Forest model ##
#-----------------------#

    # Bounds for mtry parameter (max of variables per step)
    mink<-round(sqrt(dim(features)[2]))
    maxk<-round(dim(features)[2]/1.5)
    
    #set up cluster paralellization
    cores <- detectCores()
    cl <- makeCluster(cores)
    clusterExport(cl, list("labels", "features","mink","maxk",
                           "compare.mtry", "randomForest"), envir = environment())
    registerDoParallel(cl)
    
    
    #call function to compare k error rates on features and standardised features
    system.time(rfor.perf<- parSapply(cl, seq(mink,maxk, 2), 
                                      function(x) compare.mtry(x, features, labels)))
    
    stopCluster(cl)
    
    #create and write error data frame to file 
    comparison <- data.frame(k = seq(mink, maxk, 2), feat.err = t(rfor.perf))
    
    # predict using best k on best data set
    best.k = comparison$k[which.min(comparison$feat.err.OOB)]
    test.data <- get.test.data()
    test.feat <- test.data[[2]] #standarized
    test.feat[,10:53]<-as.data.frame(apply(test.feat[,10:53],2,as.factor))
    model <- randomForest(x = features, y = labels, mtry = best.k)
    predict<-predict(model,test.feat)
    probs<-predict(model, test.feat,type="prob")
    
    # Prepare data for submission
    id <- seq(50001,150000,1) 
    prediction <- data.frame(id =id, Cover_Type = predict)
    write.csv(x = prediction, file = "data/rfor/rfor_test_prediction.csv", row.names = FALSE)
    
    
    ######
    #Now we select a final prediction by pulling a majority vote of the three models that
    #gave us the best results
    ######
    knn <- read.csv("data/knn/knn_test_prediction.csv", header = T)[,2]
    rfor <- read.csv("data/rfor/rfor_test_prediction.csv", header = T)[,2]
    svm <- read.csv("data/svm/svm_test_prediction8.csv", header = T)[,2]
    
    #DF with predctions from knn, randomforest and SVM
    pred <- cbind(knn, rfor, svm)
    
    #Calculate majority vote
    maj_vote <- apply(pred, 1, function(x) min(mfv(x)))
    maj_vote <- unlist(maj_vote) 
    maj_vote <- cbind(read.csv("data/Kaggle_Covertype_test_id.csv", header = T)[,1], maj_vote)
    colnames(maj_vote) <- c("id", "Cover_Type")
    
    #Write file
    write.csv(maj_vote, "data/maj_vote.csv", row.names = F)
    