library(forecast)
setwd("G:\\R Language\\session 9")
data<-read.csv("Sales.csv")
head(data)
##change column names
names(data)[c(1:2)]<- c("date","sales")
head(data)

#### converting into timeseries data
data<-ts(data[ ,2], start=c(2003,1), frequency = 12)

## plotting the data to have a look at it
plot(data)
##by observing the graph this not an stationary graph so make it stationary

## Differencing the data to get rid off the trend
plot(diff(data))

## Log transforming to get rid off the uneven variance
plot(log10(data))

## Combining both log and differencing to get rid off trend and uneven variance
plot(diff(log10(data)))

install.packages("fUnitRoots")


## Augmented Dickey-Fuller (ADF) Test
library(fUnitRoots)
adfTest(data)
adfTest(diff(data))
##P-value is less than the printed p-value

## running ARIMA model
require(forecast)
ARIMAfit <- auto.arima(log10(data), approximation=TRUE,trace=TRUE)
summary(ARIMAfit)


## predicting the future values
pred <- predict(ARIMAfit, n.ahead = 36)
pred


## showing values
10^(pred$pred)


## plotting the forecasted values
plot(forecast(ARIMAfit,h=36))


