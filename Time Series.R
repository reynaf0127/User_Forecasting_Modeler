data<-read.csv('./data.csv')
data$date<-as.Date(data$date,"%m/%d/%Y")
summary(data)

#Load packages
library(scales)
library(rlang)
library(vctrs)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(forecast)
library(tseries)
library(lmtest)
library(zoo)
library(ggpubr)
library(sitools)
library(xlsx)

tempdata<-data %>%
  filter(!is.na(current)&date>=as.Date('2020-04-10'))
#Explore data
newout<-boxplot(tempdata$new)$out
returnout<-boxplot(tempdata$returning)$out
new<-tempdata[-which(tempdata$new %in% newout),] %>% arrange(date)
return<-tempdata[-which(tempdata$returning %in% returnout),]  %>% arrange(date)

ts_new<-ts(new$new,start=1,frequency=1)
tsdisplay(ts_new)
#Stationary test
adf.test(ts_new) #stationary

#Auto model
auto_arima_new<-auto.arima(ts_new,trace = T)
auto_arima_new
#Manual selected model
fitARIMA_new <- arima(ts_new, order=c(3,2,3),method="ML")
fitARIMA_new
#Predict based on model
predict_new<-forecast(arima(ts_new, order=c(3,2,3),method="ML"),h = 90)
plot(predict_new)
#Residual and model fit check
residuals_new<-residuals(fitARIMA_new)
plot(residuals_new,type='l')
adf.test(residuals_new)
shapiro.test(residuals_new)
t.test(residuals_new,mu=0)

#Returning users
ts_return<-ts(return$returning,start=1,frequency = 1)
tsdisplay(ts_return)
adf.test(ts_return) #stationary

auto_arima_return<-auto.arima(ts_return,trace = T)
auto_arima_return
fitARIMA_return <- arima(ts_return, order=c(2,1,2),method="ML")
fitARIMA_return
predict_return<-forecast(arima(ts_return, order=c(2,1,2),method="ML"),h = 90)
plot(predict_return)
residuals_return<-residuals(fitARIMA_return)
plot(residuals_return,type='l')
adf.test(residuals_return)
shapiro.test(residuals_return)
t.test(residuals_return,mu=0)


new<-cbind(new,seq(from=1,to=nrow(new)))
colnames(new)[26]<-"row_num"
head(new)
summary(lm(new~log(row_num),data=new))

return<-cbind(return,seq(from=1,to=nrow(return)))
colnames(return)[26]<-"row_num"
summary(lm(returning~log(row_num),data=return))