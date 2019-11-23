setwd("/media/dell/D_PROJECTS/Edvancer_Class/Data_project1")
train=read.csv("housing_train.csv",stringsAsFactors = F)
test=read.csv("housing_test.csv",stringsAsFactors = F)
View(train)
View(test)
test$Price=NA
train$data="train"
test$data="test"
ld_al=rbind(train,test)
View(ld_al)
sort(table(ld_al$Suburb))
library(dplyr)
glimpse(ld_al)
sort(table(ld_al$Rooms)) 


creatdummies=function(data,var,freq_cutoff=0)
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

glimpse(ld_al)


lapply(ld_al,function(x) sum(is.na(x)))

for (col in names(ld_al)) {
  
  if(sum(is.na(ld_al[,col]))>0 & !(col %in% c("data","Price"))){
    ld_al[is.na(ld_al[,col]),col]=round(mean(ld_al[ld_al$data=="train",col],na.rm = T))
  }
  
}


View(ld_al)


sort(table(ld_al$Suburb))
round(tapply(ld_al$Price,ld_al$Suburb,mean,na.rm=T))
ld_al= ld_al %>% 
  mutate(Suburb_1= as.numeric(Suburb %in% c( "Albert Park","Malvern","Balwyn","Balwyn North",
                                             "Brighton","Camberwell","Ivanhoe East","Kew",
                                             "Middle Park","Toorak")),
         Suburb_2=as.numeric(Suburb %in% c("Eaglemont","Glen Iris","Hampton","Kew East",
                                           "Mont Albert","Princes Hill"))) %>% 
  select(-Suburb)

sort(table(ld_al$Rooms))
ld_al=creatdummies(ld_al,"Rooms",500)

sort(table(ld_al$Type))
ld_al=creatdummies(ld_al,"Type",1000)

sort(table(ld_al$Method))
ld_al=creatdummies(ld_al,"Method",1000)
glimpse(ld_al)


sort(table(ld_al$SellerG))
round(tapply(ld_al$Price,ld_al$SellerG,mean,na.rm=T))
ld_al= ld_al %>% 
  mutate( SellerG_1=as.numeric(SellerG %in% c("Weast","Blue","Assisi","Sotheby's","Red",
                                              "Kelly","R&H","Abercromby's","Fletchers/One","Hooper","One")),
          SellerG_2=as.numeric(SellerG %in% c("O'Donoghues","Kay","Nick","Marshall","Morrison","Ascend",
                                              "Castran","Walsh","RT"))) %>% 
  select(-SellerG)

sort(table(ld_al$Distance))
b=round(tapply(ld_al$Price,ld_al$Distance,mean,na.rm=T))
sort(b)

ld_al=ld_al %>% 
  mutate(Distance_1= as.numeric(Distance %in% c(7.8,11.8,2.1,5.5,8.5,6.2,1.5,12.7,4.6,9.2)),
         Distance_2=as.numeric(Distance %in%  c(3.2,7.5,3.7,6.3,11,10.7,9.7,10.3,13.7)),
         Distance_3 = as.numeric(Distance %in% c(5.6,7.4,4.1,9))) %>% select(-Distance)

sort(table(ld_al$Postcode))
a=round(tapply(ld_al$Price,ld_al$Postcode,mean,na.rm=T))
sort(a)
ld_al=ld_al %>% 
  mutate(Postcode_1 = as.numeric(Postcode %in%  c(3054,3123,3188,3127,3143,3102,3146,
                                                      3187,3142,3124,3104,3101,3186)),
         Postcode_2 = as.numeric(Postcode %in% c(3206,3144,3103,3126))) %>% select(-Postcode)

glimpse(ld_al)

lapply(ld_al,function(x) sum(is.na(x)))

sort(table(ld_al$Bedroom2))
ld_al=creatdummies(ld_al,"Bedroom2",440)
glimpse(ld_al)

sort(table(ld_al$Bathroom))
ld_al=creatdummies(ld_al,"Bathroom",400)
glimpse(ld_al)

sort(table(ld_al$Car))
ld_al=creatdummies(ld_al,"Car",500)
glimpse(ld_al)

sort(table(ld_al$BuildingArea))
ld_al=creatdummies(ld_al,"BuildingArea",60)


sort(table(ld_al$YearBuilt))
ld_al= ld_al %>% 
  mutate(YearBuilt_1= as.numeric(YearBuilt %in% c(1975,2012,1980,1940,1890,
                                                  1910,2000,1920,1930,1900,1950,1960,1970)),
         YearBuilt_2 = as.numeric(YearBuilt == 1961)) %>%
  select(-YearBuilt)

glimpse(ld_al)

sort(table(ld_al$CouncilArea))

round(tapply(ld_al$Price,ld_al$CouncilArea, mean,na.rm=T))

ld_al=ld_al %>% 
  mutate(ca_1=as.numeric(CouncilArea %in% c("Bayside","Boroondara","Stonnington","Whitehorse")),
         ca_2=as.numeric(CouncilArea %in% c("Manningham","Yarra")),
         ca_3=as.numeric(CouncilArea %in% c("Port Phillip","Monash","Glen Eira",""))) %>% 
  select(-CouncilArea)

glimpse(ld_al)

#sort(table(ld_al$Address))

ld_al$Address=NULL
glimpse(ld_al)

ld_train= ld_al %>% filter(data== "train") %>% select(-data)
ld_test= ld_al %>% filter(data=="test") %>% select(-data,-Price)

set.seed(2)

library(caTools)
s= sample.split(ld_train,SplitRatio = 0.7)
train1=ld_train[s,]
train2=ld_train[-s,]

fit=lm(Price~.-Landsize,data = train1)

library(car)

vif(fit)
sort(vif(fit),decreasing = T)

fit=lm(Price~.-Landsize-Rooms_2-Bathroom_1,data = train1)
summary(fit)

fit=lm(Price~.-Landsize-Rooms_2-Bathroom_1-Method_PI-Suburb_2,data = train1)
summary(fit)

#________________ testing

prediction=predict(fit,newdata = train2)
errors=train2$Price-prediction
errors**2 %>% mean() %>% sqrt()
#########

final_fit=fit=lm(Price~.-Landsize,data = ld_train)
final_fit=step(final_fit)
summary(final_fit)
test.pred=predict(final_fit,newdata = ld_test)
write.csv(test.pred,"submision2.csv",row.names = F)





