if(!require("e1071"))install.packages("e1071")
if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if(!require("doParallel"))install.packages("doParallel")
library("e1071")
#Set working directory
setwd("C:/Users/Rishabh/Documents/GitHub/TheSupportVectors/")

#source library

source("code/library.R")

#read labels and scaled features

train.data <- get.train.data()
labels <- train.data[[1]]
train.feat.scale <- train.data[[4]]

test.data <- get.test.data()
test.feat.scale <- test.data[[3]]


#set griod for c and gamma parameters for svm
cost.param <- 2 ^ seq(-5, 15, 2)
gamma.param <- 2 ^ seq(-15, 5, 2)

#set up cluster
cores <- detectCores()
cl <- makeCluster(cores, type="SOCK", outfile="") 
registerDoSNOW(cl)

#.packages=c("class", "dplyr")
results<- foreach(cost = cost.param, gamma = gamma.param, 
                  .packages=c("e1071")) %dopar% {
                    
                    svm(x = as.matrix(train.feat.scale), y = factor(labels),
                        type = "C", kernel = "radial", gamma = gamma, cost = cost,
                        cross = 10, scale = FALSE)
                  }

stopCluster(cl)

#Train svm on training data and grid of parameters
result <- svm(x = as.matrix(train.feat.scale), y = factor(labels), type = "C", kernel = "radial",
    gamma = 2 ^ -3, cost = 2 ^ -1, cross = 10, scale = FALSE)

svm.model <- tune.svm(x = train.set, y = train.label, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, test.set)
error <- sum(svm.pred != test.label) / length(test.label)

table(svm.pred, true = test.label)

table(train.label)
table(test.label)
plot(svm.model, train.set)

save(results, file = "svm_loose_grid_results.RData")
