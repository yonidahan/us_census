
#gbm model
#grid search in parallel

gbm_grid<-function(
        y=y,
        data=data,
        n_folds=10,
        shrink,
        ntrees,
        depth,
        n_minob,
        bag_frac){
             
        #Parallel backend
        n_cores<-detectCores()
        clust<-makeCluster(n_cores)
        registerDoParallel(clust)
        
        tune_grid<-expand.grid(shrink=shrink,
                               depth=depth,
                               n_minob=n_minob,
                               bag_frac=bag_frac)
        
        best<-data.frame()
        
        #Each core does a k-cross-validation task
        for(j in 0:(nrow(tune_grid)/n_cores-1)){
                
                tune_grid_j<-tune_grid[(n_cores*j+1):(n_cores*j+n_cores),]
                
                results<-
                        foreach(gridID=c(1:nrow(tune_grid_j)),
                                .packages=c("gbm","caret","rARPACK",
                                            "Metrics","Matrix"),
                                .combine=rbind,.multicombine=T)%dopar%{
                                        
                                        set.seed(1308)
                                        folds<-createFolds(y,n_folds)
                                        shrink<-tune_grid_j[gridID,"shrink"]
                                        depth<-tune_grid_j[gridID,"depth"]
                                        n_minob<-tune_grid_j[gridID,"n_minob"]
                                        bag_frac<-tune_grid_j[gridID,"bag_frac"]
                                        
                                        
                                        #Cross-validation
                                        
                                        misclassif<-0
                                        
                                        for(i in 1:n_folds){
                                                
                                                #Get the folds
                                                train_cv<-data[-folds[[i]],]
                                                test_cv<-data[folds[[i]],]
                                                y_train_cv<-y[-folds[[i]]]
                                                y_test_cv<-y[folds[[i]]]
                                                
                                                
                                                #Train generalized boosting
                                                model_cv<-gbm.fit(x=train_cv,
                                                                  y=as.integer(y_train_cv)-1,
                                                                  distribution="bernoulli",
                                                                  interaction.depth=depth,
                                                                  shrinkage=shrink,
                                                                  n.minobsinnode=n_minob,
                                                                  bag.fraction=bag_frac)
                                                
                                                #Predictions
                                                preds_cv<-predict(model_cv,
                                                                  newdata=test_cv,
                                                                  n.trees=ntrees)
                                                preds_cv[preds_cv<0.5]<-0
                                                preds_cv[preds_cv>=0.5]<-1
                                                
                                                #Get metrics
                                                misclassif<-sum(y_test_cv!=preds_cv)/nrow(train_cv)
                                                
                                        }
                                        return(c(
                                                "misclassif"=misclassif/n_folds,
                                                "shrinkage"=shrink,
                                                "depth"=depth,
                                                "nminob"=n_minob,
                                                "bag_frac"=bag_frac
                                                
                                        ))
                                }
                on.exit(stopCluster(clust))
                
                #Get best results
                results<-data.frame(results,row.names=NULL)
                best<-rbind(best,results[order(results$misclassif,decreasing=F),][1,])                
        }
        best
}

