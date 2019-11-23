#setwd("/Users/MadamPC/dropbox/March onwards/CBAP with R/")
setwd("/media/dell/9941-19EE/Edvancer_Class/Data/Data")
getwd()

rain=read.csv("rain.csv)

air=read.csv("international-airline-passengers.csv")


air=read.csv("international-airline-passengers.csv")

library(dplyr)
air= air %>%
  na.omit()

# 1. plot the time series
pass=ts(air$International.airline.passengers..monthly.totals.in.thousands..Jan.49...Dec.60,start=c(1949,1),frequency=12)
plot(pass)

# 2.
plot(decompose(pass))
#The plot shows the time series has trend, seasonality and non-stationarity

#test for stationarity
library(tseries)
adf.test(pass, alternative ="stationary", k=12)

#From the above p-value, we concluded that the time series is non-stationary. 
#Now, We have to transfer the time series from non-stationary
#state to a stationary state in which it's statistical properties (mean) do NOT 
#change with time.We will use first-order differencing 
#for such transformation. Differencing a series can remove trends.

# 3.
library(forecast)
auto.arima(pass)

arimafit=arima(pass,order=c(0,1,1),seasonal=c(0,1,0))
arimafit

# 4.
#prediction intervals of 95% confidence level for each prediction
arimafuture=forecast.Arima(arimafit,h=24,level=95)
plot(arimafuture)

acf(arimafuture$residuals,lag.max=40)
pacf(arimafuture$residuals,lag.max=40)

shapiro.test(arimafuture$residuals)
Box.test(arimafuture$residuals,lag = 3)

# 6.
adf.test(arimafuture$residuals, alternative ="stationary")
#From the above p-value, we can conclude that the residuals of our ARIMA prediction model is stationary.

auto.arima(log(pass))

arimafit=arima(log(pass),order=c(0,1,1),seasonal=c(0,1,1))
arimafuture=forecast.Arima(arimafit,h=24,level=95)
plot(arimafuture)
 
acf(arimafuture$residuals,lag.max=40)
pacf(arimafuture$residuals,lag.max=40)
Box.test(arimafuture$residuals,lag = 3)
 
