if(!require("modeest")) install.packages("modeest")

setwd("c:/Users/Rishabh/Documents/Github/TheSupportVectors/")
knn <- read.csv("data/knn/knn_test_prediction.csv", header = T)[,2]
rfor <- read.csv("data/rfor/rfor_test_prediction.csv", header = T)[,2]
svm <- read.csv("data/svm/svm_test_prediction8.csv", header = T)[,2]

pred <- cbind(knn, rfor, svm)

maj_vote <- apply(pred, 1, function(x) min(mfv(x)))
maj_vote <- unlist(maj_vote) 
maj_vote <- cbind(read.csv("data/Kaggle_Covertype_test_id.csv", header = T)[,1], maj_vote)
colnames(maj_vote) <- c("id", "Cover_Type")
write.csv(maj_vote, "data/maj_vote.csv", row.names = F)
