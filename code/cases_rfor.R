# Studied cases Random Forest

# Model 1 rfor optimized for mtry 4 to 28, mtry_optim=28
mink<-round(sqrt(dim(features)[2])/2)
maxk<-round(dim(features)[2]/1.5)


# Model 2 rfor optimized for mtry 4 to 28, mtry_optim=34 but almost equal
mink<-28
maxk<-34

# Model 3
# One rfor per class
labels1<-as.factor(1*(labels==1))


#model 3 rfor optimized for ntree



#### make models for each specific label


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

### Finally we keep the second case to predict, and look at results
setwd("D:/master/kaggle/TheSupportVectors")
model<-readRDS("data/rforest_model2.rds")

# Look at importance of features
importance<-model$importance
importance<-data.frame(feature=row.names(importance),importance=as.vector(importance))
importance$type<-c(rep("topo",2),rep("water",2),"antropic",rep("topo",3),"antropic",
                   rep("area",4),rep("soil",40),rep("topo",2))
importance2<-importance[order(importance$importance,decreasing=TRUE),]
importance2$importance<-sqrt(importance2$importance)
importance2 <- transform(importance2, 
                          feature = reorder(feature, importance))
plot<-ggplot(importance2, aes(x=feature, y=importance,fill=type)) +
    geom_bar(stat='identity') + ylab("Importance (sqrt of mean decrease Gini index)")+
    coord_flip()
#save plot
jpeg(filename = 'plots/importance_rfor.jpg', units = "in", width = 7, height = 7, res = 600)
plot
dev.off()

# Check at accuracy
confusion<-model$confusion
table(confusion)
heatmap(confusion[,-8])

write.csv(confusion,"data/rfor_confusion.csv")
