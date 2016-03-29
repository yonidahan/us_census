
#preproc2

load("./proc_files/data")
load("./proc_files/help_data")

#Variables with missing values
nas<-apply(data,2,FUN=function(x)sum(is.na(x))/nrow(data))
nas<-nas[nas!=0]
nas<-data.frame(var=names(nas),nas=nas)

fact_var<-help_data$fact_var[!c(help_data$fact_var%in%as.character(filter(nas,nas>0.2)$var))]

#Removing missing data
data<-data[,c(names(data)%in%c(help_data$contin_var,fact_var))]# 50% missing variables
data<-na.omit(data)#Rows with missing entries

#Remove collinear variables
data<-select(data,-detailed_household_and_family_stat,-detailed_industry_recode,
             -detailed_occupation_recode)

help_data$fact_var<-fact_var[!c(fact_var%in%c("detailed_household_and_family_stat"))]
help_data$contin_var<-help_data$contin_var[!c(help_data$contin_var%in%c("detailed_industry_recode",
                                                              "detailed_occupation_recode"))]

#Save
if(!exists("./proc_files/data1")){save(data,file="./proc_files/data1")}
if(!exists("./proc_files/help_data1")){save(help_data,file="./proc_files/help_data1")}

