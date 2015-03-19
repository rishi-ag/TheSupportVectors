
# Machine learning project
# --------------------
# Exploratory analysis
# --------------------

if(!require("dplyr"))install.packages("dplyr")
if(!require("tidyr"))install.packages("tidyr")


source("code/library.R")

#get data
train <- get.train.data()
labels <- train.data[[1]]
features <- train.data[[2]]
features.std <- train.data[[3]]



# Data per category
train<-tbl_df(train)
#means and st.dev of explanatory variables per category
means<-aggregate(train[,2:55],by=list(train$Cover_Type),FUN="mean")
sd<-aggregate(train[,2:55],by=list(train$Cover_Type),FUN="mean")

# Soil types vs cat
soil_cat<- train %>%
    group_by(Cover_Type) %>%
    summarise(
        sc1=sum(soil_type_1),
        sc2=sum(soil_type_2),
        sc3=sum(soil_type_3),
        sc4=sum(soil_type_4),
        sc5=sum(soil_type_5),
        sc6=sum(soil_type_6),
        sc7=sum(soil_type_7),
        sc8=sum(soil_type_8),
        sc9=sum(soil_type_9),
        sc10=sum(soil_type_10),
        sc11=sum(soil_type_11),
        sc12=sum(soil_type_12),
        sc13=sum(soil_type_13),
        sc14=sum(soil_type_14),
        sc15=sum(soil_type_15),
        sc16=sum(soil_type_16),
        sc17=sum(soil_type_17),
        sc18=sum(soil_type_18),
        sc19=sum(soil_type_19),
        sc20=sum(soil_type_20),
        sc21=sum(soil_type_21),
        sc22=sum(soil_type_22),
        sc23=sum(soil_type_23),
        sc24=sum(soil_type_24),
        sc25=sum(soil_type_25),
        sc26=sum(soil_type_26),
        sc27=sum(soil_type_27),
        sc28=sum(soil_type_28),
        sc29=sum(soil_type_29),
        sc30=sum(soil_type_30),
        sc31=sum(soil_type_31),
        sc32=sum(soil_type_32),
        sc33=sum(soil_type_33),
        sc34=sum(soil_type_34),
        sc35=sum(soil_type_35),
        sc36=sum(soil_type_36),
        sc37=sum(soil_type_37),
        sc38=sum(soil_type_38),
        sc39=sum(soil_type_39),
        sc40=sum(soil_type_40)
    )
soil_cat
# total obs per category
cat<- train %>%
    group_by(Cover_Type) %>%
    summarise(
      nobs=n()   
    )

# soil type in percent inside category
soil_pct_cat<-merge (soil_cat,cat)
soil_pct_cat[,-1]<-round(soil_pct_cat[,-1]/soil_pct_cat[,c("nobs")],5)
soil_pct_cat<-subset(soil_pct_cat,select=-nobs)


par(mfrow=c(2,5))
for (i in (2:11)){
    boxplot(train[[i]]~train$Cover_Type,main=names(train)[i])
}
#save plot
jpeg(filename = 'plots/soil_explore.jpg', units = "in", width = 5, height = 5, res = 400)
par(mfrow=c(2,5))
for (i in (2:11)){
    boxplot(train[[i]]~train$Cover_Type,main=names(train)[i])
}
dev.off()


# Data per wild area
par(mfrow=c(2,5))
for (i in (2:11)){
    boxplot(train[[i]]~train$area,main=names(train)[i])
}
#save plot
jpeg(filename = 'plots/area_explore.jpg', units = "in", width = 5, height = 5, res = 400)
par(mfrow=c(2,5))
for (i in (2:11)){
    boxplot(train[[i]]~train$area,main=names(train)[i])
}
dev.off()


soil_area<- train %>%
    group_by(area) %>%
    summarise(
        sc1=sum(soil_type_1),
        sc2=sum(soil_type_2),
        sc3=sum(soil_type_3),
        sc4=sum(soil_type_4),
        sc5=sum(soil_type_5),
        sc6=sum(soil_type_6),
        sc7=sum(soil_type_7),
        sc8=sum(soil_type_8),
        sc9=sum(soil_type_9),
        sc10=sum(soil_type_10),
        sc11=sum(soil_type_11),
        sc12=sum(soil_type_12),
        sc13=sum(soil_type_13),
        sc14=sum(soil_type_14),
        sc15=sum(soil_type_15),
        sc16=sum(soil_type_16),
        sc17=sum(soil_type_17),
        sc18=sum(soil_type_18),
        sc19=sum(soil_type_19),
        sc20=sum(soil_type_20),
        sc21=sum(soil_type_21),
        sc22=sum(soil_type_22),
        sc23=sum(soil_type_23),
        sc24=sum(soil_type_24),
        sc25=sum(soil_type_25),
        sc26=sum(soil_type_26),
        sc27=sum(soil_type_27),
        sc28=sum(soil_type_28),
        sc29=sum(soil_type_29),
        sc30=sum(soil_type_30),
        sc31=sum(soil_type_31),
        sc32=sum(soil_type_32),
        sc33=sum(soil_type_33),
        sc34=sum(soil_type_34),
        sc35=sum(soil_type_35),
        sc36=sum(soil_type_36),
        sc37=sum(soil_type_37),
        sc38=sum(soil_type_38),
        sc39=sum(soil_type_39),
        sc40=sum(soil_type_40)
    )

soil_area
# Total obs per area
area<- train %>%
    group_by(area) %>%
    summarise(
        nobs=n()   
    )

# soil type in percent inside area
soil_pct_area<-merge (soil_area,area)
soil_pct_area[,-1]<-round(soil_pct_area[,-1]/soil_pct_area[,c("nobs")],5)
soil_pct_area<-subset(soil_pct_area,select=-nobs)



# Correlation between variables

pairs(train[,c(2:11,57)],pch=19,alpha=0.2)

cor<-cor(train[,c(2:11)])
plot3<-heatmap(cor)
#save plot
jpeg(filename = 'plots/heatmap_explore.jpg', units = "in", width = 5, height = 5, res = 400)
plot3
dev.off()

# Categories vs areas
cat_area<- train %>%
    select(area,Cover_Type,id)%>%
    group_by(area,Cover_Type) %>%
    summarise(
        nobs=n()   
    )
cat_area <- spread(
    cat_area, # table to make wider
    area, # key defining columns
    nobs # values to fill columns
)


par(mfrow=c(1,1))
plot(nobs~ Cover_Type,data=cat_area,type="p")
points(cat_area$Cover_Type,data=cat_area,type="p")
cat_area


