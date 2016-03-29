
#Models
load("./proc_files/data1")
load("./proc_files/help_data1")

#First random forest, get important features
data$target<-as.factor(as.integer(data$target)-1)
rf1<-randomForest(target~.,data=data[1:help_data$n_train,],importance=T,ntree=300)
help_data$preds_rf1<-predict(rf1,newdata=data[1:help_data$n_train,],type="response")

#Use the subset to try an another random forest model
feat_sub<-c("education","major_occupation_code","age","major_industry_code",
            "dividends_from_stocks","capital_gains","target")
rf2<-randomForest(target~.,data=data[1:help_data$n_train,feat_sub],importance=T,ntree=100)
help_data$preds_rf2<-predict(rf2,newdata=data[1:help_data$n_train,feat_sub],type="response")

feat_sub<-c("education","major_occupation_code","age","major_industry_code",
            "dividends_from_stocks","capital_gains","target")

#gbm model
source("./gbm_grid.R")

gbm_best<-gbm_grid(data[,c(names(data)%in%"target")],y=data$target[1:1000],
                   n_folds=10,shrink=c(1,10),ntrees=100,depth=c(10,20),
                   n_minob=c(20,50),bag_frac=c(0.6,0.9))


#Run gbm with best parameters found using grid search with cross-validation
data1<-data[,feat_sub]
gbm<-gbm.fit(x=data[1:help_data$n_train,!c(names(data)%in%"target")],
             y=as.integer(data$target[1:help_data$n_train])-1,
             distribution="bernoulli",
             interaction.depth=1,
             shrinkage=0.1,
             n.minobsinnode=200,
             bag.fraction=0.8,
             n.trees=100)

preds_gbm<-predict(gbm,x=data[1:help_data$n_train,!c(names(data)%in%"target")],
                   y=as.integer(data$target[1:help_data$n_train])-1,
                   type="response",n.trees=100)
preds_gbm[preds_gbm<0.5]<-0
preds_gbm[preds_gbm>=0.5]<-1
help_data$preds_gbm<-preds_gbm

save(help_data,file="./proc_files/help_data1")



