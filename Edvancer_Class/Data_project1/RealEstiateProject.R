setwd("/media/dell/D_PROJECTS/Edvancer_Class/Data_project1")

house_train=read.csv("housing_train.csv",stringsAsFactors = F)
house_test= read.csv("housing_test.csv",stringsAsFactors = F)


house_test$Price=NA

house_train$data='train'
house_test$data='test'

house_all=rbind(house_train,house_test)


View(house_train)

# QA in Edvancer
# var(house_train$Price)

# tapply(house_train$Price,house_train$Type,mean,na.rm=T)
# unique(house_train$Postcode)  

# max1=tapply(house_train$Price,house_train$CouncilArea,var,na.rm=T)
# max(max1)
#Ans - Stonnington (994218615783)


CreateDummies=function(data,var,freq_cutoff=0)
{
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categorise=names(t)[-1]
  for (cat in categorise) {
    name=paste(var,cat,sep="-")
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

library(dplyr)
glimpse(house_all)

### NA 

# count NA
lapply(house_all,function(x) sum(is.na(x)))

# Remoe NA from Data

#house_all=house_all[!(is.na(house_all$Bed)),]
for(col in names(house_all)){
  
  if(sum(is.na(house_all[,col]))>0 & !(col %in% c("data","Price"))){
    
    house_all[is.na(house_all[,col]),col]=mean(house_all[,col],na.rm=T)
  }
  
}

#sort(table(house_all$Bedroom2))
#sort(table(house_all$SellerG))

sort(table(house_all$Rooms))
house_all=CreateDummies(house_all,"Rooms",500)

glimpse(house_all)

#house_all=CreateDummies(house_all,"SellerG",400)
#sort(table(house_all$SellerG))

sort(table(house_all$Type))
house_all=CreateDummies(house_all,"Type",1000)

house_all=house_all %>% 
  mutate(ca_1=as.numeric(CouncilArea %in% c("Bayside","Boroondara","Stonnington","Whitehorse")),
         ca_2=as.numeric(CouncilArea %in% c("Manningham","Yarra")),
         ca_3=as.numeric(CouncilArea %in% c("Port Phillip","Monash","Glen Eira",""))) %>% 
  select(-CouncilArea)

library(dplyr)
glimpse(house_all)


round(tapply(house_all$Price,house_all$Suburb,mean,na.rm=T))

house_all= house_all %>%
  mutate(Suburb_1= as.numeric(Suburb %in% c( "Albert Park","Malvern","Balwyn","Balwyn North",
                                             "Brighton","Camberwell","Ivanhoe East","Kew",
                                             "Middle Park","Toorak")),
         Suburb_2=as.numeric(Suburb %in% c("Eaglemont","Glen Iris","Hampton","Kew East",
                                           "Mont Albert","Princes Hill"))) %>%
  select(-Suburb)

glimpse(house_all)    

ld_train=house_all %>% filter(data=='train') %>% select(-data)
ld_test=house_all %>% filter(data=='test') %>% select(-data,-Price)

View(house_train)
set.seed(2)
library(caTools)
s= sample.split(house_train,SplitRatio = 0.7)
ld_train1=house_train[s,]   #70% data
ld_train2=house_train[-s,]  # 30% data
View(ld_train1)

#fit=lm(Price~. -Bathroom -Bedroom2 -Landsize -Distance -Postcode -Address -Distance -Car -BuildingArea -CouncilArea ,data = train_data1)
#420600

fit=lm(Price~.,data=ld_train1)

#402500

# Bedrooms and bathrooms 

summary(fit)
step(fit)
#formula(fit)

library(car)
sort(vif(fit),decreasing =T )

#________________ testing ______________

val.pred=predict(fit,newdata = ld_train2)
errors=ld_train2$Price-val.pred
errors**2 %>% mean() %>% sqrt()
# RMSE = 411272.8

###############################################
### model for predcition on the entire data

final_fit=fit=lm(Price~.-Method-Landsize-Distance
                 -Postcode-Address-Car-BuildingArea,data = house_train)

final_fit=step(final_fit)
summary(final_fit)
test.pred1=predict(final_fit,newdata = house_test)
write.csv(test.pred1,"SrinivasKakarla1_P1_Part1.csv",row.names = F)

