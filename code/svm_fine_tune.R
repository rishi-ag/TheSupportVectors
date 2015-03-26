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


#Fine grid search for c and gamma parameters for svm
#choose suitabe combo of c and gamma for fine grid results
#cost <- coarse_results[8,1]
#gamma <- coarse_results[8,2]
cost <- 512
gamma <- 0.5


cost.param <- cost + seq(-50, 50, 10)
gamma.param <- gamma + seq(.2, 1.9, length.out = 11)

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

save(fine_grid_results, file = "data/svm/svm_fine_results.RData")

fine_results <- c()

for(i in 1:11){
  fine_results <- rbind(fine_results, c(fine_grid_results[[i]]$cost, fine_grid_results[[i]]$gamma,
                                            fine_grid_results[[i]]$tot.nSV, 
                                            sum(fine_grid_results[[i]]$fitted != labels)/ length(labels)))
}

colnames(fine_results) <- c("Cost", "Gamma", "Support Vectors", "Error")
fine_results

test.feat.scale <- read.csv("data/test_features_scale.csv", header = T)
pred <- predict(fine_grid_results[[1]], test.feat.scale)
id <- read.csv(file = "data/Kaggle_Covertype_test.csv", header = T)[,1] 
prediction <- data.frame(id =id, Cover_Type = pred)
write.csv(x = prediction, file = "data/svm/svm_test_prediction_fine_1.csv", row.names = FALSE)


pred <- predict(fine_grid_results[[2]], test.feat.scale)
prediction <- data.frame(id =id, Cover_Type = pred)
write.csv(x = prediction, file = "data/svm/svm_test_prediction_fine_2.csv", row.names = FALSE)

pred <- predict(fine_grid_results[[7]], test.feat.scale)
prediction <- data.frame(id =id, Cover_Type = pred)
write.csv(x = prediction, file = "data/svm/svm_test_prediction_fine_7.csv", row.names = FALSE)

pred <- predict(fine_grid_results[[8]], test.feat.scale)
prediction <- data.frame(id =id, Cover_Type = pred)
write.csv(x = prediction, file = "data/svm/svm_test_prediction_fine_8.csv", row.names = FALSE)
