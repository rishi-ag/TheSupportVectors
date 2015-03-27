if(!require("e1071"))install.packages("e1071")
if (!require("foreach")) install.packages("foreach")
if (!require("doSNOW")) install.packages("doSNOW")
if(!require("doParallel"))install.packages("doParallel")
#Set working directory
setwd("C:/Users/Rishabh/Documents/GitHub/TheSupportVectors/")

#source library

source("code/library.R")

#read labels and scaled features

train.data <- get.train.data()
labels <- factor(train.data[[1]])
train.feat.scale <- train.data[[4]]


test.data <- get.test.data()
test.feat.scale <- test.data[[3]]


#Coarse grid search for c and gamma parameters for svm
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


save(results, file = "data/svm/svm_coarse_results.RData")

load("data/svm/svm_coarse_results.RData")

coarse_results <- c()

for(i in 1:11){
  coarse_results <- rbind(coarse_results, c(coarse_grid[[i]]$cost, coarse_grid[[i]]$gamma,
                          coarse_grid[[i]]$tot.nSV, 
                          sum(coarse_grid[[i]]$fitted != labels)/ length(labels)))
}

colnames(coarse_results) <- c("Cost", "Gamma", "Support Vectors", "Error")

#write coarse grid search results to file 
write.csv(x = coarse_results, file = "data/svm/coarse_grid_errors.csv", row.names = F)

pred <- predict(coarse_grid[[7]], test.feat.scale)
id <- read.csv(file = "data/Kaggle_Covertype_test.csv", header = T)[,1] 

prediction <- data.frame(id =id, Cover_Type = pred)
write.csv(x = prediction, file = "data/svm/svm_test_prediction7.csv", row.names = FALSE)

#Fine grid search for c and gamma parameters for svm
#choose suitabe combo of c and gamma for fine grid results
#cost <- coarse_results[8,1]
#gamma <- coarse_results[8,2]
cost <- 512
gamma <- 0.5


cost.param <- cost + seq(-50, 50, 10)
gamma.param <- gamma + seq(-0.5, 0.5, 0.1)

#set up cluster
cores <- detectCores()
cl <- makeCluster(cores, type="SOCK", outfile="") 
registerDoSNOW(cl)

#.packages=c("class", "dplyr")
fine_grid_results<- foreach(cost = cost.param, gamma = gamma.param, 
                  .packages=c("e1071")) %dopar% {
                    
                    svm(x = as.matrix(train.feat.scale), y = factor(labels),
                        type = "C", kernel = "radial", gamma = gamma, cost = cost,
                        cross = 10, scale = FALSE)
                  }

stopCluster(cl)
