setwd("/media/dell/D_PROJECTS/Edvancer_Class/DataScienceWithR/Data")

ld_train=read.csv("loan_data_train.csv",stringsAsFactors = F)

ld_test= read.csv("loan_data_test.csv",stringsAsFactors = F)

ld_test$Interest.Rate=NA

ld_train$data='train'
ld_test$data='test'

ld_all=rbind(ld_train,ld_test)

# drop amount funded by investor

ld_all$Amount.Funded.By.Investors=NULL

library(dplyr)

glimpse(ld_all)

ld_all=ld_all %>%
  mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)) ,
         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)) ,
         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines) , 
         Amount.Requested=as.numeric(Amount.Requested) ,
         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
  )

glimpse(ld_all)

ld_all=ld_all %>% 
  mutate(ll_36=as.numeric(Loan.Length=="36 months")) %>% 
  select(-Loan.Length)

round(tapply(ld_all$Interest.Rate,ld_all$Loan.Purpose,mean,na.rm=T))

# 10
# 11
# 12
# 13
# 14

ld_all=ld_all %>% 
  mutate(lp_10=as.numeric(Loan.Purpose=='educational'),
         lp_11=as.numeric(Loan.Purpose %in% c("major_purchase","medical","car")),
         lp_12=as.numeric(Loan.Purpose %in% c("vacation","wedding","home_improvement")),
         lp_13=as.numeric(Loan.Purpose %in% c("other","small_business","credit_card")),
         lp_14=as.numeric(Loan.Purpose %in% c("debt_consolidation","house","moving"))) %>% 
  select(-Loan.Purpose)

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
    
    data[,name]=as.numeric(data[,var]==cat)
  }
  
  data[,var]=NULL
  return(data)
}

glimpse(ld_all)

ld_all=CreateDummies(ld_all ,"State",100)
ld_all=CreateDummies(ld_all,"Home.Ownership",100)

library(tidyr)

ld_all=ld_all %>% 
  separate(FICO.Range,into=c("f1","f2"),sep="-") %>% 
  mutate(f1=as.numeric(f1),
         f2=as.numeric(f2),
         fico=0.5*(f1+f2)) %>% 
  select(-f1,-f2)

ld_all=CreateDummies(ld_all,"Employment.Length",100)

## NA values

lapply(ld_all,function(x) sum(is.na(x)))

ld_all=ld_all[!(is.na(ld_all$ID)),]

for(col in names(ld_all)){
  
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","Interest.Rate"))){
    
    ld_all[is.na(ld_all[,col]),col]=mean(ld_all[,col],na.rm=T)
  }
  
}

## separate train and test

ld_train=ld_all %>% filter(data=='train') %>% select(-data)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Interest.Rate)

##

set.seed(2)
s=sample(1:nrow(ld_train),0.7*nrow(ld_train))
ld_train1=ld_train[s,]
ld_train2=ld_train[-s,]

fit=lm(Interest.Rate~.-ID,data=ld_train1)

library(car)
# we'll take vif cutoff as 5

sort(vif(fit),decreasing = T)

fit=lm(Interest.Rate~.-ID-lp_14,data=ld_train1)

sort(vif(fit),decreasing = T)

# p-value take the cutoff .05

summary(fit)

fit=step(fit)

## AIC score 

summary(fit)

formula(fit)

fit=lm(Interest.Rate ~ Amount.Requested + Monthly.Income + Open.CREDIT.Lines + 
         Inquiries.in.the.Last.6.Months + ll_36  + State_TX + 
         Home.Ownership_MORTGAGE + fico + Employment.Length_3years,
       data=ld_train1)

summary(fit)

###

val.pred=predict(fit,newdata=ld_train2)

errors=ld_train2$Interest.Rate-val.pred

errors**2 %>% mean() %>% sqrt()

### model for predcition on the entire data

fit.final=fit=lm(Interest.Rate ~ .-ID,
                 data=ld_train)

fit.final=step(fit.final)

summary(fit.final)

test.pred=predict(fit.final,newdata=ld_test)

write.csv(test.pred,"submision1.csv",row.names = F)

### 

plot(fit.final,1) # residual vs fitted values => non-linearity in the data exists or not

plot(fit.final,2) # errors are normal or not

plot(fit.final,3) # variance is constant or not

plot(fit.final,4) # outliers in the data if cook's distance >1

#### 

output=summary(fit.final)
names(output)

output$coefficients[,4]


##
d1=data.frame(x=1:4,y=c("a","a","b","b"))
d2=data.frame(x=10:13,y=c("a","a","b","c"))

d3=rbind(d1,d2)
d3


