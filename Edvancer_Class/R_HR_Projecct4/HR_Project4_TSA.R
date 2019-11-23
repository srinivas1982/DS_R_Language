
#setwd("/Users/lalitsachan/Desktop/LT & HP/TSA/")
setwd("/media/dell/D_PROJECTS/Edvancer_Class/R_HR_Projecct4")
#Its similar to creating or setting library . In case of R there is only one library you can say.
#[at a time]

#Its similar to creating or setting library . In case of R there is only one library you can say.
#[at a time]


hr=read.csv("hr_train.csv")
library(dplyr)

glimpse(hr)
View(hr)
# converting to a time series data format
hr_train=ts(hr,start=c(1813))

plot(hr)

# this timse series like having either trend or seasonality, meaning 
# we dont need parameters beta and gamma

rainforecast=HoltWinters(raints,beta=F,gamma=F)


#this model is basically S{t+1}= alpha*y{t} + (1-alpha)*S{t} . Where S are
#forecasts and y are observed values. this can be written as 
#S{t+1}=S{t}+alpha*E(t) .Here E{t} is error in the previous forecast

rainforecast
plot(rainforecast)
names(rainforecast)  
rainforecast$fitted

r2=HoltWinters(raints,alpha=0.2,beta=F,gamma=F)


library(forecast)

r2forecast= forecast:::forecast.HoltWinters(rainforecast,h=12)

plot(r2forecast)


#making forecasts
library(forecast)

rainfuture=forecast:::forecast.HoltWinters(rainforecast,h=8)

rainfuture
plot(rainfuture)

#The in-sample forecast errors are stored in the named element -residuals
#of the list variable returned by fore- cast.HoltWinters(). 
#If the predictive model cannot be improved upon, there should be 
#no correlations between forecast errors for successive predictions. 
#In other words, if there are correlations between forecast errors for 
#successive predictions, it is likely that the simple exponential 
#smoothing forecasts could be improved upon by another 
#forecasting technique.

# to figure that out we'd use auto correlation function to generate plots
plot(rainfuture$residuals)

hist(rainfuture$residuals)

acf(na.omit(rainfuture$residuals,lag.max=20))

# only level ,no trend and no sonality for above model

qqnorm(rainfuture$residuals)


#If you have a time series that can be described using an additive model with 
#increasing or decreasing trend and no seasonality, you can use Holts exponential
# smoothing to make short-term forecasts.
setwd("/media/dell/D_PROJECTS/Edvancer_Class/DataScienceWithR/Data/Data")

skirts=read.csv("skirts.csv")
skirts=ts(skirts,start=c(1866))
plot(skirts)


#as you can see this time series has trend and a simple "level" forecast will not be enough.
skirtforecast=HoltWinters(skirts,gamma=F)

plot(skirtforecast)

#model here is 
#S{t} = alpha*y{t} + (1-alpha)*(S{t-1}+b{t-1})
#b{t} = beta*(S{t}-S{t-1}) + (1-beta)*b{t-1}
#one periof forecast F{t}=S{t}+b{t}
#m period forecast F{t+m}=S{t}+m*b{t}

skirtforecast
skirtfuture=forecast.HoltWinters(skirtforecast,h=19)

plot(skirtfuture)

qqnorm(skirtfuture$residual)

acf(skirtfuture$residuals,lag.max=30)

#box-jung test


#Also we are not discussing in this session about variance of the residuals
#those who are interested can read up on ARCH/GARCH models. Here our
#assumption is that error variance does not change with time 

souvenir=read.csv("souvenir.csv")

souvenirts <- ts(souvenir, frequency=12, start=c(1987,1))


plot(souvenirts)
souvenirts=log(souvenirts)

plot(souvenirts)


souvenirforecast=HoltWinters(souvenirts)

souvenirforecast

plot(souvenirforecast)

#we have already seen the complete model being estimated here in the 
#presentation
#The value of beta is 0.00, indicating that the estimate of 
#the slope b of the trend component is not updated over the time series, 
#and instead is set equal to its initial value. 
#This makes good intuitive sense, as the level changes quite a bit over 
#the time series, but the slope b of the trend component remains roughly 
#the same. 

plot(souvenirforecast)
souvenirfuture=forecast:::forecast.HoltWinters(souvenirforecast,h=48)

plot(souvenirfuture)
acf(na.omit(souvenirfuture$residuals,lag.max=30))


qqnorm(souvenirfuture$residuals)
plot(souvenirfuture$residuals)


#ARIMA models have 3 parameters  and is generally written as ARIMA(p,d,q)
library(forecast)
auto.arima(souvenirts)

#ARIMA(2,0,0)(0,1,1)[12]

#p=2 ,d=0, q=0 | P=0 , D=1 , Q=1 | m=12

arimafit=arima(souvenirts,order=c(2,0,0),seasonal=c(0,1,1))

arimafuture=forecast.Arima(arimafit,h=48)
plot(arimafuture)

acf(na.omit(arimafuture$residuals,lag.max=20))
hist(arimafuture$residuals)
qqnorm(arimafuture$residuals)
