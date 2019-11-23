library(dplyr)
library(randomForest)
library(ggplot2)
library(tree)
library(cvTools)

setwd("/media/dell/D_PROJECTS/Edvancer_Class/Project2_R")
lg_train = read.csv("store_train.csv",stringsAsFactors = FALSE)
lg_test = read.csv("store_test.csv", stringsAsFactors = FALSE)
head(lg_test)
lg_train$data = "train"
lg_test$store = NA
lg_test$data = "test"
data = rbind(lg_train, lg_test)

librry(dplyr)
glimpse(data)
data$storecode = substr(data$storecode,1,5)

CreateDummies=function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub("\\/","_",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

data = CreateDummies(data, "store_Type", 0)
data = CreateDummies(data, "state_alpha", 100)
data = CreateDummies(data, "storecode", 0)

null_values = lapply(data,function(x) sum(is.na(x)))
null_values[null_values > 0]

for(col in names(data)){
  
  if(sum(is.na(data[,col]))>0 & !(col %in% c("data","store"))){
    
    data[is.na(data[,col]),col]=mean(data[,col],na.rm=T)
  }
  
}

data$store = as.factor(data$store)

rg_train=data %>% filter(data=='train') %>% select(-data)
rg_test=data %>% filter(data=='test') %>% select (-data,-store)

set.seed(2)
s=sample(1:nrow(rg_train),0.8*nrow(rg_train))
rg_train1=rg_train[s,]
rg_train2=rg_train[-s,]
rg.tree=tree(store~.-Id -countyname -Areaname -countytownname ,data=rg_train1)

## Performance on validation set

val.score=predict(rg.tree,newdata = rg_train2,type='vector')[,2]
pROC::roc(rg_train2$store,val.score)$auc

## ----
library(car)

for_vif=lm(store~.-countyname -Areaname -countytownname,data=rg_train1)

sort(vif(for_vif),decreasing = T)


log_fit=glm(store~.-Id -countyname -Areaname -countytownname, data=rg_train1, family = "binomial")
summary(log_fit)

log_fit=step(log_fit)
# this might take 5-6 minutes to finish 
formula(log_fit)

log_fit=glm(store ~ state_alpha_CT + storecode_NCNTY,
            data=rg_train1,family='binomial')
summary(log_fit)

# from here we can drop vars one by one which had higher p-value
# code given below is result of multiple iterations

#### performance of score model on validation data
library(pROC)

val.score=predict(log_fit,newdata = rg_train2,type='response')

auc(roc(rg_train2$score,val.score))

train.score=predict(rg.tree,newdata=rg_train2,type='vector')[,2]
real=rg_train2$store

cutoffs=seq(0.001,0.999,0.001)

cutoff_data=data.frame(cutoff=99,Sn=99,Sp=99,KS=99,F5=99,F.1=99,M=99)

for(cutoff in cutoffs){
  
  predicted=as.numeric(train.score>cutoff)
  
  TP=sum(real==1 & predicted==1)
  TN=sum(real==0 & predicted==0)
  FP=sum(real==0 & predicted==1)
  FN=sum(real==1 & predicted==0)
  
  P=TP+FN
  N=TN+FP
  
  Sn=TP/P
  Sp=TN/N
  precision=TP/(TP+FP)
  recall=Sn
  
  KS=(TP/P)-(FP/N)
  F5=(26*precision*recall)/((25*precision)+recall)
  F.1=(1.01*precision*recall)/((.01*precision)+recall)
  
  M=(4*FP+FN)/(5*(P+N))
  
  cutoff_data=rbind(cutoff_data,c(cutoff,Sn,Sp,KS,F5,F.1,M))
}

cutoff_data=cutoff_data[-1,]

my_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)]

my_cutoff

## random forest 

subset_paras=function(full_list_para,n=10){
  
  all_comb=expand.grid(full_list_para)
  
  s=sample(1:nrow(all_comb),n)
  
  subset_para=all_comb[s,]
  
  return(subset_para)
}



param=list(mtry=c(5,10,15,20,25,35),
           ntree=c(50,100,200,500,700),
           maxnodes=c(5,10,15,20,30,50,100),
           nodesize=c(1,2,5,10)
)

mycost_auc=function(y,yhat){
  roccurve=pROC::roc(y,yhat)
  score=pROC::auc(roccurve)
  return(score)
}

num_trials=50
my_params=subset_paras(param,num_trials)
my_params

myauc=0

## Cvtuning
## This code will take couple hours to finish
## Dont execute in the class
for(i in 1:num_trials){
  #print(paste('starting iteration :',i))
  # uncomment the line above to keep track of progress
  params=my_params[i,]
  
  k=cvTuning(randomForest,store~.-Id -countyname -Areaname -countytownname, data=rg_train1,
             tuning =params,
             folds = cvFolds(nrow(rg_train1), K=10, type ="random"),
             cost =mycost_auc, seed =2,
             predictArgs = list(type="prob")
  )
  score.this=k$cv[,2]
  
  if(score.this>myauc){
    print(params)
    # uncomment the line above to keep track of progress
    myauc=score.this
    print(myauc)
    # uncomment the line above to keep track of progress
    best_params=params
  }
  
  #print('DONE')
  # uncomment the line above to keep track of progress
}


ld.rf.final=randomForest(store~.-Id -countyname -Areaname -countytownname,
                         mtry=best_params$mtry,
                         ntree=best_params$ntree,
                         maxnodes=best_params$maxnodes,
                         nodesize=best_params$nodesize,
                         data=rg_train)

test.pred=predict(ld.rf.final,newdata = rg_test)
predictions = data.frame(test.pred)
names(predictions) = "store"
write.csv(predictions,"Srinivas_Kakarla1_P2_part2.csv",row.names = F)

real = rg_train2$store
TP=sum(real==1 & test.pred==1)
TN=sum(real==0 & test.pred==0)
FP=sum(real==0 & test.pred==1)
FN=sum(real==1 & test.pred==0)

P=TP+FN
N=TN+FP





names(data_test)
head(data)
sum_sales_supermarket_1 = sum(data[data$store_Type == "Supermarket Type1" & data$Areaname == "Kennebec County, ME",
                                   2:6])
data[data$store_Type == "Supermarket Type1" & data$Areaname == "Kennebec County, ME",]
length(table(data$Areaname))
unique(data$Areaname)
shapiro.test(sample(data$sales5))
data = data %>% 
  mutate("sales" = sales0 + sales1 + sales2 + sales3 + sales4)
boxplot(data$sales)
(boxplot(data$sales, plot=FALSE))
max_seller = data %>% 
  select("store_Type","sales") %>% 
  group_by(store_Type) %>% 
  summarise(store_price = var(sales, na.rm = TRUE))
max(max_seller$store_price)
length(table(data$state_alpha))
summary(data$sales)
IQR(data$sales)
length(data[data$sales >= (3422 - 1.5 * (1546.75)) & data$sales <= (4969 + 1.5 * (1546.75)),])
table(data$store_Type)
