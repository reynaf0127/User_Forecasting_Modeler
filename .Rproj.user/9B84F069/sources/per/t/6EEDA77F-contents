library(RJDBC)

#Connect to Vertica and Import Data
vDriver <- JDBC(driverClass="com.vertica.jdbc.Driver", classPath="C:/Program Files/DbVisualizer/jdbc/vertica/vertica.jar")
vertica <- dbConnect(vDriver, "jdbc:vertica://lfdev-dal:5433/Kingsisle_DEV", "sfeng", "pwd_sfeng")

data <- dbGetQuery(vertica,"select * from sfeng.V_W101_all_kpi_2018")

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

summary(data)

data$date<-as.Date(data$date)

#Tidyverse data
ggplot(data,aes(x=date,y=current+new+returning))+
  geom_line()+
  ggtitle("Acutual DAU", subtitle="From 1/1/2020") + xlab("Date") + ylab("DAU")

data_1<-data%>%
    mutate(total_revenue=rev_crowns+rev_ingame+rev_ppdcard+rev_monthly+rev_prepaid+rev_semi+rev_annual+rev_ad) %>%
    mutate(arpu=total_revenue/(current+new+returning)) %>%
    arrange(date)

#Group data
arpu_a<-data_1%>%
  filter(date<='2020-03-08')%>%
  arrange(date)
arpu_b<-data_1%>%
  filter(date>='2020-03-09',date<='2020-06-13',!is.na(arpu))%>%
  arrange(date)

#Current all data
non_na_data<-data_1%>%
  filter(date<='2020-06-13')%>%
  arrange(date)
current_date<-max(non_na_data$date)

#Explore ARPU
ggplot(data_1,aes(x=date,y=arpu))+
  geom_line()
#Test if two period ARPU are significantly different
t.test(arpu_a$arpu,arpu_b$arpu)

#Investigate July DAU
arpu_c<-data_1%>%
  filter(date>='2020-07-01',!is.na(arpu),current_r<1)%>%
  arrange(date)
t.test(arpu_b$current_r,arpu_c$current_r)
t.test(arpu_b$new_r,arpu_c$new_r)
t.test(arpu_b$returning_r,arpu_c$returning_r)
cr_july<-mean(arpu_c$current_r)
nr_july<-mean(arpu_c$new_r)

cr_july
nr_july

ggplot(non_na_data,aes(y=arpu))+
  geom_boxplot()+
  ylim(c(0,6))
arpu_noout<-boxplot(non_na_data$arpu, outline=FALSE)
arpu_noout$out

#Explore Retention Rate
#Plot Retention
retention_data<-non_na_data %>%
    filter(new_r<1)
ggplot()+
  geom_line(aes(x=date,y=current_r),color='sky blue',size=1,data=retention_data)+
  geom_line(aes(x=date,y=new_r),color='orange',size=1,data=retention_data)+
  geom_line(aes(x=date,y=returning_r),color='yellow',size=1,data=retention_data)+
  ggtitle("Acutal Retention Rate") + xlab("Date") + ylab("Retention Rate")+
  geom_text(aes(x=as.Date('2020-05-10'), y = 0.9, label = "Current Retention"))+
  geom_text(aes(x=as.Date('2020-05-10'), y = 0.6, label = "Returning Retention"))+
  geom_text(aes(x=as.Date('2020-05-10'), y = 0.45, label = "New Retention"))

#Prepared Social Distancing to current date data
ggplot()+
  geom_line(aes(x=date,y=new),color='sky blue',size=1,data=arpu_b)
ggplot()+
  geom_line(aes(x=date,y=returning),color='sky blue',size=1,data=arpu_b)

ts_new<-ts(arpu_b$new,start=1,frequency=1)
tsdisplay(ts_new)
#Stationary test
adf.test(ts_new) #stationary

#Auto model
auto_arima_new<-auto.arima(ts_new,trace = T)
auto_arima_new
#Manual selected model
fitARIMA_new <- arima(ts_new, order=c(1,2,1),method="ML")
fitARIMA_new
#Predict based on model
predict_new<-forecast(arima(ts_new, order=c(1,2,1),method="ML"),h = 90)
plot(predict_new)
#Residual and model fit check
residuals_new<-residuals(fitARIMA_new)
plot(residuals_new,type='l')
adf.test(residuals_new)
shapiro.test(residuals_new)
t.test(residuals_new,mu=0)

#Returning users
ts_return<-ts(arpu_b$returning,start=1,frequency = 1)
tsdisplay(ts_return)
adf.test(ts_return) #stationary

auto_arima_return<-auto.arima(ts_return,trace = T)
auto_arima_return
fitARIMA_return <- arima(ts_return, order=c(0,2,1),method="ML")
fitARIMA_return
predict_return<-forecast(arima(ts_return, order=c(0,2,1),method="ML"),h = 90)
plot(predict_return)
residuals_return<-residuals(fitARIMA_return)
plot(residuals_return,type='l')
adf.test(residuals_return)
shapiro.test(residuals_return)
t.test(residuals_return,mu=0)

non_na_data_noout<-non_na_data%>%
  filter(!(arpu %in% arpu_noout$out))%>%
  arrange(date)

ts_arpu<-ts(non_na_data_noout$arpu,start=1,frequency=1)
tsdisplay(ts_arpu)
#Stationary test
adf.test(ts_arpu) #stationary

auto_arima_arpu<-auto.arima(ts_arpu,trace = T,ic='aic',seasonal = T)
auto_arima_arpu
fitARIMA_arpu <- arima(ts_arpu, order=c(0,1,2),method="ML")
fitARIMA_arpu
predict_arpu<-forecast(arima(ts_arpu, order=c(0,1,2),method="ML"),h = 180)
plot(predict_arpu,ylim=c(-5,5))
residuals_arpu<-residuals(fitARIMA_arpu)
plot(residuals_arpu,type='l')
adf.test(residuals_arpu)
shapiro.test(residuals_arpu)
t.test(residuals_arpu,mu=0)

#Padding ARPU data
#Low 2020 projection=$1.83 (mean(arpu_a$arpu))
#Current average $2.02 mean(arpu_b$arpu). Padding ARPU from $2.02 to $1.83
n<-length(data_1$date)-length(non_na_data$date)
arpu_padding<-seq(mean(arpu_b$arpu),mean(non_na_data$arpu),length.out=n)

#High Retention
arpu_b_retention_data<-non_na_data %>%
    filter(new_r<1)%>%
    arrange(date)
cr_1<-mean(arpu_b_retention_data$current_r)
nr_1<-mean(arpu_b_retention_data$new_r)
rr_1<-mean(arpu_b_retention_data$returning_r)
cr_1
nr_1
rr_1

#Confidence interval
cr_upper<-mean(arpu_b_retention_data$current_r)+qnorm(0.95)*(sd(arpu_b_retention_data$current_r)/sqrt(nrow(arpu_b_retention_data)))
cr_lower<-mean(arpu_a$current_r)-qnorm(0.95)*(sd(arpu_a$current_r)/sqrt(nrow(arpu_a)))
nr_upper<-mean(arpu_b_retention_data$new_r)+qnorm(0.95)*(sd(arpu_b_retention_data$new_r)/sqrt(nrow(arpu_b_retention_data)))
nr_lower<-mean(arpu_a$new_r)-qnorm(0.95)*(sd(arpu_a$new_r)/sqrt(nrow(arpu_a)))
rr_upper<-mean(arpu_b_retention_data$returning_r)+qnorm(0.95)*(sd(arpu_b_retention_data$returning_r)/sqrt(nrow(arpu_b_retention_data)))
rr_lower<-mean(arpu_a$returning_r)-qnorm(0.95)*(sd(arpu_a$returning_r)/sqrt(nrow(arpu_a)))

cr_upper
nr_upper
rr_upper

#Benchmark Retenion
cr<-mean(arpu_a$current_r)
nr<-mean(arpu_a$new_r)
rr<-mean(arpu_a$returning_r)
cr
nr
rr

result_1<-data_1 %>%
  arrange(date)%>%
  mutate(current_new_1=ifelse(date<=current_date,current,lag(current)*cr+lag(new)*nr+lag(returning)*rr), #Based on actual data and safe retention rate
         new_new_1=ifelse(date<=current_date,new,2*lag(new,1)-lag(new,2)), #Based on actual data and ts trend
         return_new_1=ifelse(date<=current_date,returning,2*lag(returning,1)-lag(returning,2))
        ) %>%
  mutate(new_error=lag(new)-lag(new_new_1), #ts trend error term
         return_error=lag(returning)-lag(return_new_1)
        ) %>%
  mutate(new_ts=new_new_1, #Full ts trend
         return_ts=return_new_1,
         current_new_2=ifelse(date<=current_date,current,lag(current)*cr_1+lag(new)*nr_1+lag(returning)*rr_1),#Smooth trend
         current_new_3=ifelse(date<=current_date,current,ifelse(date>current_date & date<='2020-06-30',lag(current)*cr_upper+lag(new)*nr_upper+lag(returning)*rr_upper,lag(current)*cr_july+lag(new)*nr_july+lag(returning)*rr_upper)), #Smooth upper limit
         current_new_4=ifelse(date<=current_date,current,lag(current)*cr_lower+lag(new)*nr_lower+lag(returning)*rr_lower) #Smooth lower limit
        )

temp<-data_1 %>% 
   filter(date<='2020-07-01')
start_row_july<-length(temp$date)

end_row<-nrow(result_1)
start_row<-length(non_na_data$date)+1
start_row
end_row
start_row_july

for (i in start_row:end_row)
    {
    result_1[i,32]=result_1[i-1,32]*cr+result_1[i-1,8]*nr+result_1[i-1,12]*rr #safe retention rate, new_linear, and returning_log
    result_1[i,37]=result_1[i-1,37]-20.74 #Overwrite new ts 
    result_1[i,38]=result_1[i-1,38]-24.547 #Overwrite returning ts
    result_2<-result_1
}

result_3<-result_2 %>%
  mutate(new_ts_new=ifelse(new_ts-new_linear<=0,new_linear,new_ts),
         return_ts_new=ifelse(return_ts-return_log<=0,return_log,return_ts)
        )

for (i in start_row:end_row)
    {
        if(i<start_row_july){
            result_3[i,39]=result_3[i-1,39]*cr_1+result_3[i-1,42]*nr_1+result_3[i-1,43]*rr_1
            result_3[i,40]=result_3[i-1,40]*cr_upper+result_3[i-1,42]*nr_upper+result_3[i-1,43]*rr_upper
            result_3[i,41]=result_3[i-1,41]*cr_lower+result_3[i-1,42]*nr_lower+result_3[i-1,43]*rr_lower
        } else {
            result_3[i,39]=result_3[i-1,39]*cr_1+result_3[i-1,42]*nr_1+result_3[i-1,43]*rr_1
            result_3[i,40]=result_3[i-1,40]*cr_july+result_3[i-1,42]*nr_july+result_3[i-1,43]*rr_upper
            result_3[i,41]=result_3[i-1,41]*cr_lower+result_3[i-1,42]*nr_lower+result_3[i-1,43]*rr_lower
        }
    result_4=result_3
}
head(result_4)

#Padding ARPU for the future months
for (i in start_row:end_row)
    {
    result_4[i,31]=arpu_padding[i-start_row+1]
    result_5<-result_4
}

result_6<-result_5 %>%
  mutate(dau_user_model=new_linear+return_log+current_log,
         dau_social_distancing=ifelse(date<=current_date,new,new_linear)+ifelse(date<=current_date,returning,return_log)+current_new_1,
         dau_social_distancing_smooth=ifelse(date<=current_date,new,new_ts_new)+ifelse(date<=current_date,returning,return_ts_new)+current_new_2,
         dau_social_distancing_upper=ifelse(date<=current_date,new,new_ts_new)+ifelse(date<=current_date,returning,return_ts_new)+current_new_3,
         dau_social_distancing_lower=ifelse(date<=current_date,new,new_ts_new)+ifelse(date<=current_date,returning,return_ts_new)+current_new_4
        )%>%
  mutate(revenue_user_model=dau_user_model*mean(arpu_a$arpu),
         revenue_social_distancing=ifelse(date<=current_date,total_revenue,dau_social_distancing*mean(arpu_a$arpu)),
         revenue_social_distancing_smooth=ifelse(date<=current_date,total_revenue,dau_social_distancing_smooth*arpu),
         revenue_social_distancing_upper=ifelse(date<=current_date,total_revenue,ifelse(month(date)<=11,dau_social_distancing_upper*mean(arpu_b$arpu),dau_social_distancing_upper*2.422258)),
         revenue_social_distancing_lower=ifelse(date<=current_date,total_revenue,dau_social_distancing_lower*mean(arpu_a$arpu))
         )

write.xlsx(result_6,"C:/Users/sfeng/Documents/Analysis/R Project/Social Distancing Decay/Investigate for July_3.xlsx")

highlight_dau<- result_6 %>% 
              select(date, dau_user_model, dau_social_distancing,dau_social_distancing_smooth,dau_social_distancing_upper,dau_social_distancing_lower) %>%
              filter(date==current_date) %>%
              pivot_longer(., cols = c(dau_user_model,dau_social_distancing,dau_social_distancing_smooth,dau_social_distancing_upper,dau_social_distancing_lower), names_to = "model", values_to = "forecasting_dau")
highlight_dau

highlight_revenue<- result_6 %>% 
              select(date, revenue_user_model, revenue_social_distancing,revenue_social_distancing_smooth,revenue_social_distancing_upper,revenue_social_distancing_lower) %>%
              filter(date==current_date) %>%
              pivot_longer(., cols = c(revenue_user_model,revenue_social_distancing,revenue_social_distancing_smooth,revenue_social_distancing_upper,revenue_social_distancing_lower), names_to = "model", values_to = "forecasting_revenue")
highlight_revenue

#Plotting
chart_dau<-result_6 %>% 
  select(date, dau_user_model, dau_social_distancing,dau_social_distancing_smooth,dau_social_distancing_upper,dau_social_distancing_lower) %>%
  pivot_longer(., cols = c(dau_user_model,dau_social_distancing,dau_social_distancing_smooth,dau_social_distancing_upper,dau_social_distancing_lower), names_to = "model", values_to = "forecasting_dau")

ggplot() +
  geom_line(aes(x = date, y = forecasting_dau, group=model,color=model),size=1,data=chart_dau)+
  geom_point(aes(x = date, y = forecasting_dau, group=model),data=highlight_dau)+
  theme(legend.position="bottom",legend.text=element_text(size=7),plot.margin=margin(2,.8,8,.2, "cm"),plot.caption=element_text(color = "black", face = "italic", size=8))+
  ggtitle("Decay of DAU after Social Distancing", subtitle="From 1/1/2020 to 12/31/2020") + xlab("Date") + ylab("DAU")+
  labs(color="Models",caption='Green and red lines are showing actual DAU before 5/31 then showing decay of DAU by two different forecasting methods')+
  scale_y_continuous(labels=f2si)+
  scale_color_brewer(palette = "Set2",name="Models", labels = c("DAU by current variables (lower)", "DAU by normal variables", "DAU by current variables", "DAU by current variables(upper)", "DAU by forecasting model"))


chart_revenue<-result_6 %>%
  select(date, revenue_user_model, revenue_social_distancing,revenue_social_distancing_upper) %>%
  pivot_longer(., cols = c(revenue_user_model,revenue_social_distancing,revenue_social_distancing_upper), names_to = "model", values_to = "forecasting_revenue") %>%
  mutate(month_num=month(date)) %>%
  mutate(month=as.factor(month(date))) %>%
  group_by(month,month_num,model) %>%
  summarise(total_revenue=sum(forecasting_revenue)) %>%
  arrange(month_num)
chart_revenue

#Plotting
year_revenue<-result_6 %>%
  select(date, revenue_user_model, revenue_social_distancing,revenue_social_distancing_smooth,revenue_social_distancing_upper,revenue_social_distancing_lower) %>%
  pivot_longer(., cols = c(revenue_user_model,revenue_social_distancing,revenue_social_distancing_smooth,revenue_social_distancing_upper,revenue_social_distancing_lower), names_to = "model", values_to = "forecasting_revenue") %>%
  mutate(year=as.factor(year(date))) %>%
  group_by(year,model) %>% 
  summarise(total_revenue = sum(forecasting_revenue))
year_revenue

#Plotting
ggplot(data=chart_revenue,aes(x = month, y = total_revenue, fill=model)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme(legend.position="bottom",plot.margin=margin(2,.8,8,.8, "cm"),plot.caption=element_text(color = "black", face = "italic", size=7))+
  ggtitle("Decay of Revenue after Social Distancing", subtitle="From 1/1/2020 to 12/31/2020")+
  xlab("Month")+
  ylab("Revenue")+
  labs(color="Models",caption='Green and red lines are showing actual revenue before 5/31 then showing trend of revenue calculated by two forecasting DAU and normal ARPU')+
  scale_y_continuous(labels=dollar_format())+
  scale_x_discrete(breaks=c("1","2","3","4","5","6","7","8","9","10","11","12"),labels=c("Jan", "Feb", "Mar","Apr", "May", "Jun","Jul", "Aug", "Sep","Oct", "Nov", "Dec"))+
  scale_fill_brewer(palette = "Set2",name="Models", labels = c("Revenue by normal variables", "Revenue by current variables", "Forecasting Revenue"))

result_7<-result_6 %>%
  mutate(new_sd=ifelse(date<=current_date,new,new_ts_new),
         returning_sd=ifelse(date<=current_date,returning,return_ts_new))%>%
  select(date,dau_social_distancing,dau_social_distancing_smooth,revenue_social_distancing,revenue_social_distancing_smooth,revenue_social_distancing_upper,revenue_social_distancing_lower,
         new_sd,returning_sd,current_new_2)

dbWriteTable(vertica,"sfeng.WizardSocialDecayModeler",result_7,overwrite=TRUE)
