# 1.

setwd('/Users/MadamPC/Dropbox/March onwards/Python Data Science/Data')
getwd()

pdl=read.csv("paydayloan_collections.csv",sep=",",header = T,stringsAsFactors = F)
head(pdl)

glimpse(pdl)

# data preparation
pdl=pdl %>%
  mutate(payment=ifelse(payment=="Success",1,0))

pdl=pdl %>%
  na.omit()

for(i in 1:31){
  if(class(pdl[,i])=="character")
  {
    pdl[,i]=as.factor(pdl[,i])
  }
  
}

pdl$payment=as.factor(pdl$payment)

set.seed(2)
s=sample(1:nrow(pdl),0.7*nrow(pdl))
pdl_train=pdl[s,]
pdl_test=pdl[-s,]

glimpse(pdl_train)

library(tree)
library(ISLR)
tree.pdl= tree(payment~.,data=pdl_train,na.action=na.exclude)
summary(tree.pdl)

tree.pred.test=predict(tree.pdl,newdata=pdl_test,type="class")
table(tree.pred.test,pdl_test$payment)


library(randomForest)
fit_rf=randomForest(payment~.,data=pdl_train,na.action=na.exclude)
fit_rf

forest.pred.test=predict(fit_rf,newdata=pdl_test)
table(pdl_test$payment,forest.pred.test)

importance(fit_rf)
varImpPlot(fit_rf)


# 2.
setwd('/Users/MadamPC/Dropbox/March onwards/PDS V2/Projects/P3')
getwd()

bd=read.csv("base_data.csv",sep=",",header = T,stringsAsFactors = F)
head(bd)

glimpse(bd)

bd=bd %>%
  na.omit()

for(i in 1:12){
  if(class(bd[,i])=="character")
  {
    bd[,i]=as.factor(bd[,i])
  }
  
}

set.seed(2)
s=sample(1:nrow(bd),0.7*nrow(bd))
bd_train=bd[s,]
bd_test=bd[-s,]

glimpse(bd_train)

library(tree)
library(ISLR)
tree.bd= tree(Counterfeit_Sales~.-Medicine_ID,data=bd_train,na.action=na.exclude)
summary(tree.bd)

sum((bd_test$Counterfeit_Sales-predict(tree.bd,newdata=bd_test))**2) %>%
  sqrt()

library(randomForest)
fit_rf=randomForest(Counterfeit_Sales~.-Medicine_ID,data=bd_train,na.action=na.exclude)
fit_rf

sum((bd_test$Counterfeit_Sales-predict(fit_rf,newdata=bd_test))**2) %>%
  sqrt()

importance(fit_rf)
varImpPlot(fit_rf)



