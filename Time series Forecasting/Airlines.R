library(forecast)
library(tseries)
library(ggplot2)
library(GGally)
library(trend) 
library(AER)

df<-read.csv('C:\\Users\\Shivani Bhavsar\\OneDrive\\Documents\\ExcelR Assignment files\\DS\\A18 Forecast\\Airlines.csv')
df
class(df)
time<-ts(df$Passengers,start=c(1995,1),frequency=12)
time

#to check seasonal, treand, observed, random components.
d<-decompose(time)
plot(d)
class(time)
plot(time)

#Mannkendall test for detection of trend 
mk.test(time)

# H0 : No trend vs H1 : Trend is present 
#Conclusion: Here we  Reject Ho p-value 0.05 > 0.000000 i.e. The trend is present in the given series.So now we take difference. we proceed for fitting the ARIMA models. For, that we plot the ACF and PACF graph of original Yt.


#auto- correlation function(ACF)
acf(time)

#partial-auto-correlation function (PACF)
pacf(time)

#augmented dickey-fuller test
adf.test(time)

#H0 : not stationary , H1 : stationarity present
#Conclusion: Here we Reject Ho i.e. The data follow Stationarity in the given series.

#Differnce (library (AER)) 
ndiffs(WWWusage)
ndiffs(diff(time,alpha = 0.05,
       type = c("trend")))


#Akaike-information criteria (AIC)
timemodel<-auto.arima(time,ic='aic',trace=TRUE)
timemodel

#arima(0,0,0),(0,1,0)(ARmodel, difference, MA)
summary(timemodel)
acf(ts(timemodel$residuals))
pacf(ts(timemodel$residuals))
ftime<-forecast(timemodel,h=12,levale=c(95))
ftime
plot(ftime)
summary(ftime)