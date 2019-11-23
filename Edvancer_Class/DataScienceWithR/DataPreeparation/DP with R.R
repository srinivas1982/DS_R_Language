## ----For detailed comments on code snippets , please refer to reading material----------------------------------------------------------


## setwd(" Here/Goes/Path/To/Your/Data/Folder/")
## windows users will need to replace "\" with "/" or "\\" 

getwd()

setwd("/media/dell/D_PROJECTS/Edvancer_Class/DataScienceWithR/Data/Data")

getwd()

bd=read.csv("bank-full.csv",sep=";",stringsAsFactors = F)
str(bd)


# xlsx : openxlsx , readxl : read.xlsx , read_xls
# sas7bdat : read.sas7bdat


## ------------------------------------------------------------------------
for(i in 1:ncol(mtcars)){
  
  print(mean(mtcars[,i]))

  }


## -----------------------------------------------------------

lapply( mtcars , mean )



mydata=data.frame(v1=c(1,2,3,4,NA,5,6,7),
                  v2=sample(1:10,8))
mydata


lapply(mydata, mean,na.rm=T )


## --------------------------------------------------------------
## # Before running these codes , you'll have to set your working directory to the folder "namesbystate".
## # You will find this folder inside "Data" folder which you downloaded from LMS
getwd()

setwd("/Users/lalitsachan/Dropbox/0.0 Data/namesbystate")


file_names=list.files(pattern="*.TXT")
file_names

files=lapply( file_names , read.csv ,header=F,stringsAsFactors=F)

# rbind(files[[1]],files[[2]],files[[3]],....)

file=do.call(rbind,files)


names(file)=c("state","gender","year","name","count")


## ----

## ------------------------------------------------------------------------

apply(mtcars,1,mean)
apply(mtcars,2,mean)

# before applying the function apply converts the data frame to a matrix
# all the values in a matrix should be of same type

## ------------------------------------------------------------------------


tapply( mtcars$mpg , mtcars$am, mean)

tapply(mtcars$mpg, mtcars[,c("am","vs")],mean)


## ----
library(dplyr)
library(hflights)


hflights
## ------------------------------------------------------------------------
flights=tbl_df(hflights)
flights

## ------------------------------------------------------------------------
#note: you can use comma or ampersand to represent AND condition

d1= filter(flights, Month==1 & DayofMonth==1)

# use pipe for OR condition

d2= filter(flights, UniqueCarrier=="AA" | UniqueCarrier=="UA" )

d3= filter(flights, UniqueCarrier %in% c("AA", "UA"))

# select 

d4=select(flights, DepTime, ArrTime, FlightNum)

d5=select(flights, -DepTime, -ArrTime, -FlightNum)



## ------------------------------------------------------------------------
d6= select(flights, Year:DepTime,
           contains("Taxi"), 
           contains("Delay"))



## ------------------------------------------------------------------------
# nesting method to select UniqueCarrier and DepDelay columns 
# and filter for delays over 60 minutes


filter(select(flights, UniqueCarrier, DepDelay), DepDelay > 60)


## ------------------------------------------------------------------------
x=sample(10,6)
x
sum(sin(log(x)))

x %>% 
  log() %>%
  sin() %>% 
  sum()

# ctrl + shift + M : %>% 

## ------------------------------------------------------------------------
# chaining method
d6=flights %>% 
  select(UniqueCarrier, DepDelay,contains("Time")) %>% 
  filter(DepDelay > 60) %>% 
  select(contains("Dep")) %>% 
  filter(DepDelay>100)
 

## ------------------------------------------------------------------------

d7=flights %>%
  select(UniqueCarrier, DepDelay) %>%
  arrange(UniqueCarrier,desc(DepDelay))

## ------------------------------------------------------------------------

# dplyr approach 
d8=flights %>%
  select(Distance, AirTime,DepDelay,TaxiIn,TaxiOut) %>%
  mutate(speed=Distance/AirTime,
         total_taxi=TaxiIn+TaxiOut) %>% 
  select(-TaxiIn,-TaxiOut) %>% 
  mutate(time_diff=AirTime-total_taxi,
         time_diff=time_diff/60) %>% 
  arrange(time_diff) %>% 
  mutate(DepDelay_high=as.numeric(DepDelay>60))


## ------------------------------------------------------------------------
# create a table grouped by Dest, and then summarise each group by taking the mean of ArrDelay

flights %>%
  group_by(Dest,Month) %>% 
  summarise(avg_delay = mean(ArrDelay,na.rm=T))

## ------------------------------------------------------------------------


flights %>%
  group_by(Month, DayofMonth) %>%
  summarise(flight_count = n()) %>% 
  arrange(desc(flight_count))


# n() gives count of observations

# rewrite more simply with the 'tally' function
flights %>%
  group_by(Month, DayofMonth) %>%
  tally(sort=TRUE) 



flights %>%
  group_by(Dest) %>%
  summarise(flight_count = n(),
            plane_count = n_distinct(TailNum))


# for each destination, show the number of cancelled and not 
# cancelled flights

flights %>% 
  select(Dest,Cancelled) %>% 
  group_by(Dest,Cancelled) %>% 
  summarise(f_count=n())


flights %>% 
  select(Dest,Cancelled) %>% 
  group_by(Dest) %>% 
  summarise(f_count=n(),
            cancelled_f=sum(Cancelled)) %>% 
  mutate(non_c_f=f_count-cancelled_f)



select(flights,Dest,Cancelled)

d9=flights %>%
  mutate(Cancelled=ifelse(Cancelled==1,"A","B")) %>% 
  group_by(Dest) %>%
  summarize(c1=sum(Cancelled=="A"),
            total=n(),
            c2=total-c1,
            c3_weather=sum(CancellationCode=="B")) %>%
  select(-total)



# for each month, calculate the number of flights and the 
# change from the previous month

flights %>%
  group_by(Month) %>%
  summarise(flight_count = n()) %>%
  mutate(lagged_col=lag(flight_count,1),
    change = flight_count - lagged_col)

# filter , select , arrange , mutate , group_by+summarise
# tbl_df , n(): number of obs , n_distinct(col): number of distinct obs
# lag : lagged columns


# for each dest , every month 
# find out which month has highest count of flights , 
# second highest count of flights and least count of flight
# post the solution on qa forum



# dplyr approach: better formatting, and adapts to your screen width
str(flights)
glimpse(flights)

View(mtcars)

mtcars1=mtcars[1:15,]
mtcars2=mtcars[16:32,]
mtcars3=mtcars[c(1,3,13,17),]


s=sample(1:32,10)

mtcars4=mtcars[s,]
mtcars5=mtcars[-s,]

# make two datasets from mtcars , d1 ,d2 . 
# d1 contains 20% random rows , d2 contains rest of the 80% rows



## ------------------------------------------------------------------------
set.seed(1)
k=nrow(mtcars)
s=sample(1:k,0.7*k)


# we are using set.seed for our random sample to be reproducible

## ------------------------------------------------------------------------
mtcars_sample=mtcars[s,]

## ------------------------------------------------------------------------
mtcars_remaining=mtcars[-s,]

## ------------------------------------------------------------------------
set.seed(1)
s=sample(1:nrow(mtcars),100,replace = TRUE)
mtcars_bootstrapped=mtcars[s,]


## tidyr and lubridate
## ----
library(tidyr)

wide= data.frame(
  name = c("Wilbur", "Petunia", "Gregory"),
  a = c(67, 80, 64),
  b = c(56, 90, 50),
  c=c(10,20,30),
  d=c(0,1,2),
  e=c(-3,5,6)
)
wide

wide %>% 
  gather(drug,measurement,a:e)



## ----
set.seed(10)
wide= data.frame(
  id = 1:4,
  trt = rep(c('control', 'treatment'), each = 2),
  work.T1 = runif(4),
  home.T1 = runif(4),
  work.T2 = runif(4),
  home.T2 = runif(4)
)

wide

## ------------------------------------------------------------------------
long = wide %>%
  gather(key,time,work.T1:home.T2)
long

## ------------------------------------------------------------------------
long1=long %>% 
  separate(key,into=c("key1","key2"),sep="\\.")

long1

## ------------------------------------------------------------------------
step1= long1 %>%
  unite(untkey,key1,key2,sep="_")

step1
step2=step1 %>%
  spread(untkey,time)
step2

## ----
library(lubridate)

ymd("20110604")
mdy("06-04-2011")
dmy("04/06/2011")

class(dmy("04/06/2011"))


#dplyr, ggplot2, tidyr , ggvis, lubridate : Hadley Wickham

### POSIXct 

## ------------------------------------------------------------------------


arrive = ymd_hms("2011-06-04 12:00:00", tz = "Pacific/Auckland")

leave = ymd_hms("2011-08-10 14:00:00", tz = "Pacific/Auckland")


## please find out documentation for list of time zones , what all options i can give to tz

## ------------------------------------------------------------------------
second(arrive)

second(arrive) = 25
arrive

second(arrive) = 0
arrive

wday(arrive)

wday(arrive, label = TRUE)

quarter(arrive)

day(arrive)

month(arrive)

## ------------------------------------------------------------------------
meeting = ymd_hms("2011-07-01 09:00:00", tz = "Pacific/Auckland")

## ------------------------------------------------------------------------
with_tz(meeting, "Asia/Kolkata")

## ------------------------------------------------------------------------
leap_year(2011)

ymd(20110101) + dyears(1)

ymd(20110101) + years(1)

## ------------------------------------------------------------------------
 
leap_year(2012)

ymd(20110101) + dyears(12) #adding exactly 365 days 

ymd(20110101) + years(12) # this explicitly add one year

ymd(20120101) + months(3)

## Find out how to take difference of two dates 
## how to get the output of difference in 
## different units : minutes , days, months


## ------------------------------------------------------------------------
# d= two digit date
# y = two digit year
# b = abbreviated month name
# Y= 4 digit year
# B = complete month name
# p = for  am pm
# m = month in numbers
# %H:%M = time is in 24 hrs format
# %I:%M = time is in 12 hr format , this needs to be accompanied by p

parse_date_time("01-12-Jan","%d-%y-%b")

z=parse_date_time("2012-01-January 10:05 PM","%Y-%d-%B %I:%M %p")

# extracting date time in specfic format from POSIXt object 

format(z,"%Y-%b")





#Function can be used seamlessely for vectors as well

x = c("09-01-01", "09-01-02", "09-01-03")
parse_date_time(x, "ymd")
parse_date_time(x, "%y%m%d")
parse_date_time(x, "%y %m %d")

parse_date_time("6-12-99","%d-%m-%y")

#######

setwd("/Users/lalitsachan/Dropbox/March onwards/CBAP with R/Data/")
ci=read.csv("census_income.csv",stringsAsFactors = F)


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
  
  data[,name]=as.numeric(data[,var]==cat)
}

data[,var]=NULL
return(data)
}

ci=CreateDummies(ci,'marital.status')
ci=CreateDummies(ci,'native.country',100)

##### Regular Expressions #######

# 1 : we know how to match by providing exact letters or digit

myvec=c("a7bc123xyz","1define456","789sth","379tut26")
gsub("123","MATCH",myvec)

# substitute all the strings having digits
# \\d stands for a digit in regular expression

gsub("\\d","MATCH",myvec)


gsub("\\d\\d\\d","MATCH",myvec)


gsub("\\d\\d9","MATCH",myvec)


## \\w : means any character including numbers except special charactaers


# 2 : . means any single character, for symbol . use \\. 

myvec=c("ab@c","123#","$qwe.123","...")

gsub(".","MATCH",myvec)

gsub("\\.","MATCH",myvec)

#exercise : substitute first two members only with a single MATCH

myvec=c("896.","?Q+.","abc1")

gsub("...\\.","Match",myvec)

# 3 : replacing multiple characters at once

myvec=c("<#abc","#abc","abc%")


gsub("[<#%]","MATCH",myvec)


# 4 : excluding a pattern with metacharacter ^

myvec=c("<abc","#1abc","abc%")


gsub("[^abc]","MATCH",myvec)


# 5 : character ranges with -

myvec=c("<ab:c","#def","zghi%","a:z")

gsub("[^a-z]","MATCH",myvec)


gsub("[a-z]","MATCH",myvec)
gsub("[^a-z]","MATCH",myvec)



# 6 : more compact way for repetition

myvec=c("abc123xyz","define456","789sth","379tut6","3798tut6")

gsub("\\d\\d\\d","MATCH",myvec)

gsub("\\d{3}","MATCH",myvec)

gsub("\\d{3,}","MATCH",myvec)

myvec=c("abc123xy1234z12345","def4567ine456","789sth","379tut6")

gsub("\\d{3,5}","MATCH",myvec)


gsub("\\d{1,}","MATCH",myvec)


# 7 : Kleene Plus and Kleene Star

# * means it matches zero or more character
# + means it matches atleast one or more character

# people names
people = c("rori", "emmilia", "matteo", "mehmemt", "filipe", "anna", "tyler",
           "rasmus", "mt jacob", "youna", "flora", "adi mmt")

# match m zero or more times, and t
gsub("m*t","MATCH",people)


# match m atleast one or more times
gsub("m+t","MATCH",people)



# 8 : metacharacter or with ?

myvec=c("ac","abc","ab?c","12ac")

gsub("abc","MATCH",myvec)

gsub("ab?c","MATCH",myvec)

gsub("ab?\\??c","MATCH",myvec)

# 9 : \\s stands for any white space : " " , \\t , \\n

# 10 : ^ start and $ end
myvec=c("file_record_transcript.pdf","file_07241999.pdf",
        "fileabcpdf","file.pdf","fileabc.pdf","testfile_fake.pdf.tmp",
        "file_record_transcript.pdff")

gsub("^file.+\\.pdf$","MATCH",myvec)


# 11 : groups with ()

myvec=c("ac","abc","aQAc","12ac")

gsub("ab?Q?A?c","MATCH",myvec)

gsub("ab?QA?c","MATCH",myvec)

gsub("ab?(QA)?c","MATCH",myvec)

### discussion on grep and grepl

x=c("lalit","lalit sachan","ankit sharma","lalti kumar")

grep("lalit",x)

grep("lalit",x,value = T)

grep("janardhan",x)

grepl("lalit",x)

grepl("janardhan",x)


