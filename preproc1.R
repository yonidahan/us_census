
#preproc1

setwd("C:/Users/Sarah/Desktop/Data Science/Projects/us_census")
lapply(c("rARPACK","doParallel","caret","dplyr","ROCR","gbm",
         "randomForest"),require,character.only=T)

#Loading into R
train<-read.csv("./us_census_full/census_income_learn.csv"
                ,header=F,stringsAsFactors=F)
test<-read.csv("./us_census_full/census_income_test.csv",
               header=F,stringsAsFactors=F)
meta<-readLines("./us_census_full/census_income_metadata.txt")

#Stack the data
data<-rbind(train,test)

#Parsing variables' names from metadata file
get_col_names<-function(x){
        x<-unlist(strsplit(x,":"))[1]
        x<-gsub(" ","_",x)
        x<-gsub("\\'","",x)## ' -->""
        x
}
colnames(data)<-unlist(lapply(meta[-c(1:142)],get_col_names))
colnames(data)<-gsub("-","_",colnames(data))
colnames(data)<-c(colnames(data)[-25],"target")

#Remove instance weight
data<-select(data,-instance_weight)

#Get factor and continuous variables' names
fact_var<-names(data)[sapply(data,is.character)]
contin_var<-setdiff(names(data),fact_var)

#Some continuous variables are in fact categorical
new_fact<-sapply(data[,contin_var],FUN=function(x)length(unique(x)))
new_fact<-names(new_fact[new_fact<20])
fact_var<-c(fact_var,new_fact)
contin_var<-contin_var[!c(contin_var%in%new_fact)]

#Re-format some factor levels in order to avoid typing issues
# " High school graduate"--> "High school graduate"
# "Masters degree(MA MS MEng MEd MSW MBA)" --> "Masters degree"
data[,c(fact_var)]<-apply(data[,fact_var],2,
                          FUN=function(x)gsub("(^[[:space:]]*)([[:alpha:]]*)","\\2",x))
data$education<-gsub("([Masters|Doctorate|Prof school|Bachelors])(degree)([[:print:]]+)",
                     "\\1\\2",data$education)

#NA values
data[data=="NA"]<-NA
data[data=="?"]<-NA

#Convert character variables to factor
data[,fact_var]<-as.data.frame(lapply(data[,fact_var],factor))

#Save objects
if(!dir.exists("./proc_files")){dir.create("./proc_files")}

help_data<-list(fact_var=fact_var,contin_var=contin_var,n_train=nrow(train))
if(!exists("./proc_files/help_data")){save(help_data,file="./proc_files/help_data")}
if(!exists("./proc_files/data")){save(data,file="./proc_files/data")}
