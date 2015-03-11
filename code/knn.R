if(!require("class"))install.packages("class")
if(!require("doParallel"))install.packages("doParallel")
if(!require("reshape2"))install.packages("reshape2")
if(!require("ggplot2"))install.packages("ggplot2")
if(!require("ggthemes"))install.packages("ggthemes")

source("code/library.R")

#get data for knn
train.data <- get.train.data()
labels <- train.data[[1]]
features <- train.data[[2]]
features.std <- train.data[[3]]

#set up cluster
cores <- detectCores()
cl <- makeCluster(cores)
clusterExport(cl, list("labels", "features", "features.std", "compare.k", "class", "knn.cv"), envir = environment())
registerDoParallel(cl)

#call function to compare k error rates on features and standardised features
system.time(k.perf.std <- parSapply(cl, seq(1,21, 2), function(x) compare.k(x, features.std, labels)))
system.time(k.perf <- parSapply(cl, seq(1,21, 2), function(x) compare.k(x, features, labels)))

stopCluster(cl)

#create and write error data frame to file 
comparison <- data.frame(k = seq(1, 21, 2), raw.feat.err = k.perf,
                         std.feat.err = k.perf.std)

write.csv(x = comparison, file = "data/knn/knn_data_error.csv")

#read comparison file
comparison <- read.csv(file = "data/knn/knn_data_error.csv", header = T)[,-1]

#melt daa for graph
comparison.long <- melt(data = comparison, id.vars = "k", 
                        measure.vars = c("raw.feat.err", "std.feat.err"),
                        variable.name = "feature_type", value.name = "error")



#plot
plot1 <- ggplot(data = comparison.long, aes(x = k,y = error, color = feature_type )) +
  geom_line(size = 0.5) + 
  geom_point(size = 3) + 
  scale_x_continuous(breaks= seq(1,21,2)) + 
  ggtitle("k error (raw vs std features)") +
  scale_color_wsj(name  ="Feature Type", labels=c("Raw", "Standardised"))

#save plot
jpeg(filename = 'plots/kErrorFeature.jpg', units = "in", width = 5, height = 5, res = 400)
plot1
dev.off()


# predict using best k on best data set
best.k = 1
test.data <- get.test.data()
test.feat.raw <- test.data[[1]]
test.sum <- knn(train = features, test = test.feat.raw, cl = labels, k = best.k)

id <- read.csv(file = "data/Kaggle_Covertype_test.csv", header = T)[,1] 

prediction <- data.frame(id =id, Cover_Type = test.sum)
write.csv(x = prediction, file = "data/knn/knn_test_prediction.csv", row.names = FALSE)