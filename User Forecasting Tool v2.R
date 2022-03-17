#User Forecasting Tool

#Connect Vertica
library(RJDBC)
vDriver <- JDBC(driverClass="com.vertica.jdbc.Driver", classPath="C:/Program Files/DbVisualizer/jdbc/vertica/vertica.jar")
vertica <- dbConnect(vDriver, "jdbc:vertica://lfdev-dal:5433/Kingsisle_DEV", "sfeng", "pwd_sfeng")
db <- dbGetQuery(vertica, '
WITH attribution as (
                 select a.account_id,
                 a.country,
                 date(d.account_creation_date) as account_creation_date,
                 c.group_name
                 from Core_DB.CS_AccountCore_DIM as a
                 join Core_DB.MKT_Campaign_DIM as b on a.url_campaign_id=b.mkt_campaign_id
                 join Core_DB.MKT_CampaignGroup_DIM as c on b.mkt_group_id=c.mkt_group_id
                 join WZ_Core_DB.GCS_AccountGame_DIM as d on a.account_id=d.account_id 
                 where d.account_creation_date is not null and a.account_id != 0 and a.ki_employee_flag=0 and b.game_id=1 and d.account_id!=0
                 and a.account_id not in (select account_id from eramirez.V_W101SyntheticAccounts)
)

SELECT play_date,COUNT(DISTINCT ga.account_id) as dau
FROM WZ_Core_DB.MQT_GCS_GameActivityDaily AS ga
JOIN attribution AS a ON a.account_id = ga.account_id
WHERE year(play_date) >= 2017
GROUP BY 1
                 ')

#Tidyverse data
library(dplyr)
library(ggplot2)
ggplot(db,aes(x=play_date,y=dau))+
  geom_point()
library(lubridate)
db$play_date<-as.Date(db$play_date)
summary(db)
data<-db%>%
  arrange(play_date) %>%
  mutate(year=year(play_date),month=month(play_date),wday=wday(play_date,week_start = 1))%>%
  filter(year<=2019)
summary(data)
#Data visualization
year_average<-data%>%
  group_by(year)%>%
  summarise(mean=mean(dau))

ggplot(year_average,aes(x=year,y=mean))+
  geom_line()+
  scale_y_log10()
#Which one is normal distributed?
library('car')
qqnorm(year_average$mean)
shapiro.test(year_average$mean)
shapiro.test(log(year_average$mean)) #P value is higher, so log model is more linear

ggplot(data,aes(x=month,y=dau))+
  geom_col() #Factor

ggplot(data,aes(x=wday,y=dau))+
  geom_col() #Factor
data$month<-as.factor(data$month)
data$wday<-as.factor(data$wday)

ggplot(data,aes(x=play_date,y=dau))+
  geom_line()+
  scale_y_log10()
#Log DAU
data_log<-data%>%
  mutate(log_dau=log(dau))

#Separate train and test data
set.seed(127)
assignment <- sample(0:1, size= nrow(data_log), prob = c(0.75,0.25), replace = TRUE)
train <-data_log[assignment == 0, ]
test <- data_log[assignment == 1, ]
t.test(train$log_dau,test$log_dau) #P value shows the diff between train and test is not significant

#Linear model
require("MASS")
require("rpart")
require("mgcv")
#LM model
model1 <- lm(log_dau ~ year+month+wday, data = train)
summary(model1)
#Stepwise method to select the variables
model2 <- stepAIC(model1, trace = 0)
summary(model2)
#VIF to check multicollinearity among variables
vif(model2)


#Model fit
#mse, standard error and r square
require("caret")
require("lattice")
d1=model2$fitted-train$log_dau
#standard error
print(sqrt(mean(d1^2)))
#r square
print(1-(sum((d1)^2)/sum((train$log_dau-mean(train$log_dau))^2)))
shapiro.test(model2$residuals)
qqnorm(model2$residuals)
t.test(model2$residuals,mu=0)
plot(model2)

#Cross validation
predict(model2, newdata = test, type = "response") -> test_prob
plot(test_prob,type="l",col="red")
lines(test$log_dau,col="blue")
legend("bottomleft",legend=c("Pred", "Act"),
       col=c("red", "blue"),lty=1:4, cex=1)
t.test(test_prob,test$log_dau)
wilcox.test(test_prob,test$log_dau)
sqrt(mean((test_prob-test$log_dau)^2,na.rm=T))
write.csv(coef(model2),"C:/Users/sfeng/Documents/Analysis/User Analysis/user forecasting result.csv")

#Predict 2020
data_all<-db%>%
  arrange(play_date) %>%
  mutate(log_dau=log(dau),year=year(play_date),month=month(play_date),wday=wday(play_date,week_start = 1))
  #%>%filter(play_date>'2020-03-09')
data_all$month<-as.factor(data_all$month)
data_all$wday<-as.factor(data_all$wday)

ggplot(data_all,aes(x=play_date,y=log_dau))+
  geom_line()
predict(model2, newdata = data_all, type = "response") -> prob_all
plot(prob_all,type="l",col="red")
lines(data_all$log_dau,col="blue")
legend(1000,90000,legend=c("Pred", "Act"),
       col=c("red", "blue"),lty=1:4, cex=1)
residual_all<-data_all$log_dau-prob_all
residual_all
plot(residual_all,type='l')

#Time series residual
ts<-ts(residual_all,start=1,frequency = 1)
plot(ts,type='l')
require("forecast")
library('tseries')
acf(ts,lag.max = 50) #Shows lag1 to lag9 are highly related to today's data
pacf(ts) #Lag 1
tsdisplay(ts)
#Stationary test
adf.test(ts) #stationary
auto_arima<-auto.arima(ts,trace = T) #ARIMA(5,1,5) is best model Moving Average and Differencing
auto_arima

fitARIMA <- arima(ts, order=c(30,1,30),method="ML")
library(lmtest)
require("zoo")
coeftest(fitARIMA) 
fitARIMA

predict<-forecast(arima(ts, order=c(30,1,30),method="ML"),h = 30)
plot(predict)
plot(ts)

##Check ts model residuals
residuals_fitARIMA<-residuals(fitARIMA)
plot(residuals_fitARIMA,type='l')
adf.test(residuals_fitARIMA) #Error term is stationary 
shapiro.test(residuals_fitARIMA)
qqnorm(residuals_fitARIMA)
t.test(residuals_fitARIMA,mu=0) #Error term is equal to 0
##Output result
write.csv(data_2020,"C:/Users/sfeng/Documents/Analysis/User Analysis/user forecasting data 2020.csv")
write.csv(predict_2020,"C:/Users/sfeng/Documents/Analysis/User Analysis/user forecasting data predict 2020.csv")


##Connect R with Tableau
install.packages("Rserve")
library(Rserve)
Rserve()

dbDisconnect(vertica)
