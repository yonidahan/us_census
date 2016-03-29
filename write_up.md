---
title: "US Census Data Write-up"
author: "Jonathan DAHAN"
date: "Monday, March 28, 2016"
output: html_document
---

##Exploratory Data Analysis   

***   

This part aims to provide basic insights on the variables: their distributions, extreme and missing values and relations with the income class.   
The scripts needed to perform the following are in my [github repository](https://github.com/yonidahan/us_census).   


***

###Missing values   
At a first glance, we can see that most of the missing values in the learn and test data sets are accounted by four variables: "migration_code_change_in_msa", "migration_code_change_in_reg", "migration_code_move_within_reg", "migration_prev_res_in_sunbelt". They each contain almost 50% of missing values. I chose to remove these variables.   
Additionally, an another group of features contain a small proportion of missing values: "hispanic_origin", "state_of_previous_residence", "country_of_birth_father", "country_of_birth_mother", "country_of_birth_self" (no more than 1.72%). For the purpose of this analysis and considering their small proportion of missingness, I didn't make use of a missing data imputation method and just removed the missing entries.   



```r
load("./proc_files/data")   
nas<-apply(data,2,FUN=function(x)sum(is.na(x))*100/nrow(data))
nas<-nas[nas!=0]
(nas<-data.frame(var=names(nas),nas=nas))
```

```
##                                                           var        nas
## hispanic_origin                               hispanic_origin  0.4273519
## state_of_previous_residence       state_of_previous_residence  0.3468266
## migration_code_change_in_msa     migration_code_change_in_msa 49.9998329
## migration_code_change_in_reg     migration_code_change_in_reg 49.9998329
## migration_code_move_within_reg migration_code_move_within_reg 49.9998329
## migration_prev_res_in_sunbelt   migration_prev_res_in_sunbelt 49.9998329
## country_of_birth_father               country_of_birth_father  3.3887432
## country_of_birth_mother               country_of_birth_mother  3.0709858
## country_of_birth_self                   country_of_birth_self  1.7231067
```

```r
par(mar=c(3,10,5,4))
barplot(nas$nas[order(nas$nas,decreasing=T)],names.arg=nas$var,
        horiz=T,las=1,col="blue",cex.main=0.8,
        cex.names=0.6,xlim=c(0,100),main="Missing values (%)")
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

***   



###Collinearity   

The variable "detailed_household_and_family_stat" is a more detailed version of "detailed_household_summary_in_household". I chose to keep only the latter, which is the simplest form of both.   
"detailed_industry_recode" and "detailed_occupation_recode" have also been discarded for the same reason.   

***   


###Distributions   
At this stage, the data has the following structure:   

```r
data.frame(
        "factor"=length(sapply(data,class)[sapply(data,class)=="factor"]),
        "integer"=length(sapply(data,class)[sapply(data,class)!="factor"])
        )
```

```
##   factor integer
## 1     33       8
```
One can note the **unbalanced** aspect of the income (only 6% of the people in the high-income class). Extreme values are observed in "capital_gains", "dividends_from_stocks and "weeks_worked_in_year". The large majority of people has neither capital gains nor dividends from stocks (96% and 98%). A left skewness is observed in "dividends_from_stocks" (90% earns no dividends).   
Also, we can observe some conflicting aspects in the data (characteristic of administrative datasets): some people have a net capital income (dividends+capital gains - capital losses) greater than 50k$ and belong to the low-income class; and many capital_gains of $99,999 seems unreasonable.   
The variables that appear the most related with the income class are "age" (high-income group people is older than low-income group), "class_of_worker" (high-income people turn out to be in either the private or the self-employed category), "education" (people who hold a degree tend to have high incomes), "marital_status" (being married leads to higher incomes),"major_industry_code" (banking and finance industries tend to have higher incomes), "major_occupation_code" (executives have higher incomes), "race" (a slight higher income is observed for white people) and sex (males tend to have higher incomes).   




```r
load("./proc_files/data1")
load("./proc_files/help_data1")
library(ggplot2)

n_train <- help_data$n_train
for (var in help_data$fact_var) {
    barplot(table(data[1:n_train, var])[order(table(data[1:n_train, var]), decreasing = T)]/n_train, 
        main = paste("Barplot of", var), horiz = F, col = "blue", ylim = c(0, 
            1), las = 2, cex.main = 0.85, cex.names = 0.5)
}
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-2.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-3.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-4.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-5.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-6.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-7.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-8.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-9.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-10.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-11.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-12.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-13.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-14.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-15.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-16.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-17.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-18.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-19.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-20.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-21.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-22.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-23.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-24.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-25.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-26.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-27.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-28.png) 

```r
for (var in help_data$contin_var) {
    hist(data[1:n_train, var], main = paste("Histogram of", var), col = "blue", 
        freq = F, xlab = var, cex.main = 0.85, cex.lab = 0.85)
}
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-29.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-30.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-31.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-32.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-33.png) ![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-34.png) 

```r
ggplot(data[1:n_train, ], aes(x = class_of_worker, fill = target)) + geom_bar(position = "dodge") + 
    ggtitle("Income by Class of Worker") + ylab("count") + xlab("Class of worker") + 
    theme(axis.text.x = element_text(angle = 90))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-35.png) 

```r
ggplot(data[1:n_train, ], aes(x = age, fill = target)) + geom_bar(position = "dodge") + 
    ggtitle("Income by age") + ylab("count") + xlab("Age")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-36.png) 

```r
ggplot(data[1:n_train, ], aes(x = education, fill = target)) + geom_bar(position = "dodge") + 
    ggtitle("Income by Education") + ylab("count") + xlab("Education") + theme(axis.text.x = element_text(angle = 90))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-37.png) 

```r
ggplot(data[1:n_train, ], aes(x = marital_stat, fill = target)) + geom_bar(position = "dodge") + 
    ggtitle("Income by marital status") + ylab("count") + xlab("Marital status") + 
    theme(axis.text.x = element_text(angle = 90))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-38.png) 

```r
ggplot(data[1:n_train, ], aes(x = sex, fill = target)) + geom_bar(position = "dodge") + 
    ggtitle("Income by sex") + ylab("count") + xlab("Sex") + theme(axis.text.x = element_text(angle = 0))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-39.png) 

```r
ggplot(data[1:n_train, ], aes(x = major_industry_code, fill = target)) + geom_bar(position = "dodge") + 
    ggtitle("Income by Industry") + ylab("count") + xlab("Industry") + theme(axis.text.x = element_text(angle = 90))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-40.png) 

```r
ggplot(data[1:n_train, ], aes(x = major_occupation_code, fill = target)) + geom_bar(position = "dodge") + 
    ggtitle("Income by Occupation") + ylab("count") + xlab("Occupation") + theme(axis.text.x = element_text(angle = 90))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-41.png) 

```r
ggplot(data[1:n_train, ], aes(x = race, fill = target)) + geom_bar(position = "dodge") + 
    ggtitle("Income by Race") + ylab("count") + xlab("Race") + theme(axis.text.x = element_text(angle = 90))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-42.png) 

***   

##Models   

***   


In order to model the income variable, the following reasoning will be applied:   
- learn a subset of important variables using random forest    
- compare two classification models with the latter subset (eliminate noise and redundancy): random forest and generalized boosted regression model.   

In order to handle this unbalanced dataset, a rigorous model would use an oversampling technique such as Synthetic Minority Over-sampling Technique (NV Chawla - 2002), but it's out of the scope of this evaluation.   


***   


###Feature selection  
Random forests can assess variable importance and out-of-bag error internally, it can be used to obtain a relevant feature subset.   



```r
library(randomForest)
load("./proc_files/data1")
load("./proc_files/help_data1")

data$target<-as.factor(as.integer(data$target)-1)
rf1<-randomForest(target~.,data=data[1:help_data$n_train,],importance=T,ntree=100)   
varImpPlot(rf1,main="Variable importance",type=2)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

The most important features (shown by the Gini decrease in node purity) seem to be **"education"**, **"major_industry_code"**, **"age"**, **"major_industry_code"** (as pointed out in exploratory data analysis), **"dividends_from_stocks"** and **"capital_gains"**. A similar but unbiased way (the measure is biased towards categorical variables with lots of levels) to obtain all relevant features would be to use the Boruta algorithm.   




```r
load("./proc_files/help_data1")
library(caret)
confusionMatrix(help_data$preds_rf1,data$target[1:help_data$n_train],positive="1")
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction      0      1
##          0 187243  11060
##          1     55   1165
##                                           
##                Accuracy : 0.9443          
##                  95% CI : (0.9433, 0.9453)
##     No Information Rate : 0.9387          
##     P-Value [Acc > NIR] : < 2.2e-16       
##                                           
##                   Kappa : NA              
##  Mcnemar's Test P-Value : < 2.2e-16       
##                                           
##             Sensitivity : 0.095297        
##             Specificity : 0.999706        
##          Pos Pred Value : 0.954918        
##          Neg Pred Value : 0.944227        
##              Prevalence : 0.061271        
##          Detection Rate : 0.005839        
##    Detection Prevalence : 0.006115        
##       Balanced Accuracy : 0.547501        
##                                           
##        'Positive' Class : 1               
## 
```

```r
#F1-Score
r<-confusionMatrix(help_data$preds_rf1,data$target[1:help_data$n_train],positive="1")$byClass[1]
p<-confusionMatrix(help_data$preds_rf1,data$target[1:help_data$n_train],positive="1")$byClass[3]
paste("F1-score:",(2*p*r)/(p+r))
```

```
## [1] "F1-score: 0.173298624023801"
```
The accuracy is high (accuracy paradox). However, the confusion matrix, the large amount of false negatives (low recall) and the low F1-score show the difficulty of this model at predicting the income class.   

***   


###Random forest with the subset of relevant features   
Now we can use the subset of features obtained in the last step to try again a random forest:   

```r
feat_sub<-c("education","major_occupation_code","age","major_industry_code",
            "dividends_from_stocks","capital_gains","target")
rf2<-randomForest(target~.,data=data[1:help_data$n_train,feat_sub],importance=T,ntree=100)
confusionMatrix(help_data$preds_rf2,data$target[1:help_data$n_train],positive="1")
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction      0      1
##          0 186666   5798
##          1    632   6427
##                                          
##                Accuracy : 0.9678         
##                  95% CI : (0.967, 0.9685)
##     No Information Rate : 0.9387         
##     P-Value [Acc > NIR] : < 2.2e-16      
##                                          
##                   Kappa : NA             
##  Mcnemar's Test P-Value : < 2.2e-16      
##                                          
##             Sensitivity : 0.52573        
##             Specificity : 0.99663        
##          Pos Pred Value : 0.91047        
##          Neg Pred Value : 0.96987        
##              Prevalence : 0.06127        
##          Detection Rate : 0.03221        
##    Detection Prevalence : 0.03538        
##       Balanced Accuracy : 0.76118        
##                                          
##        'Positive' Class : 1              
## 
```

```r
#F1-Score
r<-confusionMatrix(help_data$preds_rf2,data$target[1:help_data$n_train],positive="1")$byClass[1]
p<-confusionMatrix(help_data$preds_rf2,data$target[1:help_data$n_train],positive="1")$byClass[3]
paste("F1-score:",(2*p*r)/(p+r))
```

```
## [1] "F1-score: 0.666562953744037"
```
The model performs better, as shown by the increases of recall and harmonic mean recall/precision.  

***   


###Generalized Boosting Regression Model   
Following the same reasoning as before, a generalized boosting regression model has been tried on both whole set and subset of features. Due to caret package inefficiency on my machine, I wrote my own parallelized routine (gbm_grid) for finding the best parameters using a grid search with cross-validation.   
The classifier using the subset of features performs better, but the recall (0.23) and F1-score (0.29) are lower than the ones obtained using random forest with the feature subset.   

***   

   
##Test   

***   

Random forest appears to be the best model on the training set. But the following shows its lower performance on the independent test dataset. 


```r
test_preds<-predict(rf2,newdata=data[-c(1:help_data$n_train),feat_sub],
                    type="response")
confusionMatrix(test_preds,data$target[-c(1:help_data$n_train)],positive="1")
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction     0     1
##          0 79003  3278
##          1   853  1899
##                                         
##                Accuracy : 0.9514        
##                  95% CI : (0.95, 0.9529)
##     No Information Rate : 0.9391        
##     P-Value [Acc > NIR] : < 2.2e-16     
##                                         
##                   Kappa : NA            
##  Mcnemar's Test P-Value : < 2.2e-16     
##                                         
##             Sensitivity : 0.36681       
##             Specificity : 0.98932       
##          Pos Pred Value : 0.69004       
##          Neg Pred Value : 0.96016       
##              Prevalence : 0.06088       
##          Detection Rate : 0.02233       
##    Detection Prevalence : 0.03236       
##       Balanced Accuracy : 0.67807       
##                                         
##        'Positive' Class : 1             
## 
```

```r
r<-confusionMatrix(test_preds,data$target[-c(1:help_data$n_train)],positive="1")$byClass[1]
p<-confusionMatrix(test_preds,data$target[-c(1:help_data$n_train)],positive="1")$byClass[3]
paste("F1-score:",(2*p*r)/(p+r))
```

```
## [1] "F1-score: 0.47900113507378"
```


***   


##Notes    

***   

The most difficult steps have been for me to understand the data set and its variables (everything was not clearly explicited in the metadata file) and to find a way of how to get a first quick-and-dirty model. Performing all these steps in some hours has been challenging but interesting.   


As noted in the instructions, I spent several hours on the data set (not days). If I had to build a pure model, I would focus on these points:   

- careful preprocessing and feature engineering   
- try generating synthetic samples (using *Synthetic Minority Over-sampling Technique* for instance)   
- try **penalized logistic regression** (with L1 penalization)   
- diagnose bias Vs variance using learning curves   
- try other ensembling models such as extreme gradient boosting   
- improve tuning parameters process   
- try model ensembling with the best models obtained   
- try other perspectives, like **anomaly detection**   
