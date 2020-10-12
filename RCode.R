# loading the data
data <- read.csv("C:/Users/DANISH/Desktop/smba_project2/Etherum.csv")
str(data) # all the data are factors
#eliminating comma
library(gsubfn)
data$Volume = gsub(',','',data$Volume)
data$Market.Cap = gsub(',','',data$Market.Cap)
#converting into numeric
data$Open=as.numeric(data$Open)
data$High=as.numeric(data$High)
data$Low=as.numeric(data$Low)
data$Close=as.numeric(data$Close)
data$Volume=as.numeric(data$Volume)
data$Market.Cap=as.numeric(data$Market.Cap)
str(data)

# sorting and converting
data$Date=as.Date(data$Date, "%d-%b-%y")
data=data[order(data$Date),]
summary(data)
#splitting the data
test=data[1301:1345,]
data=data[1:1300,]
#converting dataset into time series objects
library(xts)
data_t = xts(data[, -1], order.by=as.POSIXct(data$Date))
class(data_t)

#Stationarity Check
#removing non-stationarity
plot(data_t$Close)
plot(log(data_t$Close))
plot(diff(log(data_t$Close)))

#augmented Dickey Fuller Test
library(tseries)
adf.test((data_t$Close), alternative="stationary",k= )
adf.test(na.omit(diff(data_t$Close)), alternative="stationary", k = 365)
adf.test(log(data_t$Close), alternative="stationary", k=365)
adf.test(na.omit(diff(log(data_t$Close))), alternative="stationary", k=365)
plot(diff(log(data_t$Close)))

#data visualization

library(forecast)
library(fpp2)
library(ggplot2)
autoplot(data_t$Close)
ts_data<- ts(data,start = c(2015, 8,7), frequency = 365)
ts_test<-ts(test)
autoplot(data_t[,1:4])
autoplot(data_t[,1:4], facets = TRUE)

#smoothing
autoplot(data_t[,1:4], facets = TRUE) +
geom_smooth()
autoplot(data_t[,1:4]) +
geom_smooth()

#lag plots and acf plots and white noise test
ggAcf(data_t)

#Ljung Test for serial autocorrelation
#null Hyp: there is no serial corelationupto lag 10
Box.test(data_t$Close,lag=10,type="Ljung-Box") #we reject the null

#Model builing
#simple forecasting method
library(tseries)
autoplot(ts_data[,5]) +
autolayer(meanf(ts_data[,5], h=45),
 series="Mean", PI=FALSE) +
autolayer(naive(ts_data[,5], h=45),
 series="Naïve", PI=FALSE) +
autolayer(snaive(ts_data[,5], h=45),
 series="Seasonal naïve", PI=FALSE) +
ggtitle("Forecasts for 45 days closing price") +
xlab("Year") + ylab("closing price") +
 guides(colour=guide_legend(title="Forecast"))
 
#residual check
checkresiduals(naive(diff(log(ts_data[,5]))))

#reject the null hypo that autocorelation comes from white noise no aucor
#exponential moving average
fc <- ses(ts_data[,5], h=45)
round(accuracy(fc),2)
autoplot(fc)+
autolayer(fitted(fc), series="Fitted") +
ylab("Closing price") + xlab("Year")
fc$model # alpha value=0.6175 and lo=375.39
accuracy(ts(fc$mean),ts_test[,5])

# using pipe
#residual vs one step forecast error
cbind('Residuals' = residuals(fc),
 'Forecast errors' = residuals(fc,type='response')) %>%
autoplot(facet=TRUE) + xlab("Year") + ylab("")
checkresiduals(fc)

#AR model
library(vars)
require(strucchange)
lgcls<- lag(data_t$Close, start = c(2015,8,7))
lgcls[1]=500
data_qlr<-cbind(data_t,lgcls)

model <- lm(data_qlr$Close~data_qlr$Close.1)

ar1=ar(na.omit(diff(log(data_t$Close)),aic=TRUE))

myqlr<- qlr.test(model,from=c(2015,8), to=c(2018,3))
qlr
plot(myqlr,alpha=0.05)
ar1=ar(diff(log(trclosing,aic=TRUE)))
ar1

plot(forecast(ar1,h=45))
pred1=forecast(ar1,h=45)
accuracy(forecast(ar1))

#test accuracy
accuracy(pred1,test$Close)
forecast(ar1,h=45)%>%autoplot()
cbind('Residuals' = residuals(ar1),
 'Forecast errors' = residuals(ar1,type='response')) %>%
autoplot(facet=TRUE) + xlab("Year") + ylab("")

a1=arima(trclosing,order=c(1,0,0))
a2=arima(trclosing,order=c(2,0,0))
a3=arima(trclosing,order=c(3,0,0))
a4=arima(trclosing,order=c(4,0,0))
a5=arima(trclosing,order=c(5,0,0))
a6=arima(trclosing,order=c(6,0,0))

autoarima= auto.arima(trclosing, seasonal = FALSE)
cbind('Residuals' = residuals(autoarima),
 'Forecast errors' = residuals(autoarima,type='response')) %>%
autoplot(facet=TRUE) + xlab("Year") + ylab("")
accuracy(forecast(autoarima, h=45),test$Close)
forecast(autoarima,h=45)%>%autoplot()
library(lmtest)
library("dynlm", lib.loc="~/R/win-library/3.5")
grangertest(Close ~ High, order = 2, data = train)
adl1=dynlm(trclosing ~ L(train$High,1) + L(trclosing,1) + L(trclosing,2) + L(trclosing,3)+ L(trclosing,4)+ L(trclosing,5) +
L(trclosing,6))
coeftest(adl1)
accuracy(adl1)
accuracy(forecast(adl1, h=45),test$Close)
cbind('Residuals' = residuals(adl1),
 'Forecast errors' = residuals(adl1,type='response')) %>%
autoplot(facet=TRUE) + xlab("Year") + ylab("")
accuracy(forecast(adl1, h=45),test$Close)
accuracy(adl1)
