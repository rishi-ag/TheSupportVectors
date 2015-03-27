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


