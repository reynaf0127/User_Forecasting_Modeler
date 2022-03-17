##User Forecasting Model
#Set train and test data
data_temp<-reactive({
  data_temp<-data%>%
    mutate(year=(year(date)-2015+1),
           month=as.factor(month(date)),wday=as.factor(wday(date)),
           new_log=log(new),current_log=log(current),returning_log=log(returning),
           dau_log=log(dau))
  return(data_temp)
})

train<-reactive({
  data_temp<-data_temp()
  data_before_current_year<-data_temp%>%filter(year(date)<=2019)
  return(data_before_current_year)
})
  
test<-reactive({
  data_temp<-data_temp()
  data_current_year<-data_temp%>%filter(year(date)>=input$current_year&year(date)<=input$current_end_year)
  return(data_current_year)
})

#(Method 1: Linear all by year, month and weekday)
model1<-reactive({
  train<-train()
  model<-lm(train[,input$column_model]~year+month*wday,data=train)
  stepAIC(model,trace=0)
})

summary1<-reactive({summary(model1())})

#Model Fit
#standar error
output$stdev_train1<-reactive({
  model1<-model1()
  train<-train()
  print(sqrt(mean(model1$fitted.values-train[,input$column_model])^2))
})
#r square
output$rsquar1<-reactive({
  summary1<-summary1()
  summary1$adj.r.squared
})
#t test p value to check residuals
output$residuals1<-reactive({
  model1<-model1()
  t<-t.test(model1$residuals,mu=0)
  t$p.value
})

#(Method 2: Log all by year, month and weekday)
model2<-reactive({
  train<-train()
  model<-lm(log(train[,input$column_model])~year+month*wday,data=train)
  stepAIC(model,trace=0)
})

summary2<-reactive({summary(model2())})

#Model Fit
#standar error
output$stdev_train2<-reactive({
  model2<-model2()
  train<-train()
  print(sqrt(mean(model2$fitted.values-log(train[,input$column_model]))^2))
})
#r square
output$rsquar2<-reactive({
  summary2<-summary2()
  summary2$adj.r.squared
})
#t test p value to check residuals
output$residuals2<-reactive({
  model2<-model2()
  t<-t.test(model2$residuals,mu=0)
  t$p.value
})

#plot
linear_model_plot<-reactive({
  test<-test()
  model1<-model1()
  model2<-model2()
  data_predict1<-predict(model1,newdata=test,type = "response")
  data_predict2<-predict(model2,newdata=test,type = "response")
  
  n1<-max(ifelse(!is.na(test[,input$column_model]),test[,input$column_model],exp(data_predict2)))
  plot(test[,input$column_model],type='l',col=wes_palette("FantasticFox1")[3],ylim=c(0,n1),lwd=2,ylab="KPI Value",xlab="Day")
  lines(data_predict1,col=wes_palette("Zissou1")[3],lwd=2)
  lines(exp(data_predict2),col=wes_palette("Royal1")[2],lwd=2)
  })

output$linear_model_plot<-renderPlot(linear_model_plot())

#A full data table with user forecasting model
full_data<-reactive({
  test<-test()
  train<-train()
  full_temp1<-test%>%
    mutate(dau_pre_linear=predict(stepAIC(lm(train[,26]~year+month*wday,data=train),trace=0),newdata=test,type="response"),
           current_pre_linear=predict(stepAIC(lm(train[,2]~year+month*wday,data=train),trace=0),newdata=test,type="response"),
           new_pre_linear=predict(stepAIC(lm(train[,3]~year+month*wday,data=train),trace=0),newdata=test,type="response"),
           return_pre_linear=predict(stepAIC(lm(train[,4]~year+month*wday,data=train),trace=0),newdata=test,type="response"),
           dau_pre_log=exp(predict(stepAIC(lm(log(train[,26])~year+month*wday,data=train),trace=0),newdata=test,type="response")),
           current_pre_log=exp(predict(stepAIC(lm(log(train[,2])~year+month*wday,data=train),trace=0),newdata=test,type="response")),
           new_pre_log=exp(predict(stepAIC(lm(log(train[,3])~year+month*wday,data=train),trace=0),newdata=test,type="response")),
           return_pre_log=exp(predict(stepAIC(lm(log(train[,4])~year+month*wday,data=train),trace=0),newdata=test,type="response")),
           current_r_log=exp(predict(stepAIC(lm(log(train[,5])~year+month*wday,data=train),trace=0),newdata=test,type="response")),
           new_r_log=exp(predict(stepAIC(lm(log(train[,6])~year+month*wday,data=train),trace=0),newdata=test,type="response")),
           return_r_log=exp(predict(stepAIC(lm(log(train[,7])~year+month*wday,data=train),trace=0),newdata=test,type="response")))
  full<-full_temp1%>%     
  mutate(return_pre_log=ifelse(date>=as.Date('2020-08-01')&date<=as.Date('2020-11-30'),return_pre_log+900,
                               ifelse(date>=as.Date('2020-12-01')&date<=as.Date('2021-01-15'),return_pre_log+800+500,
                                      ifelse(date>=as.Date('2021-01-16')&date<=as.Date('2021-02-28'),return_pre_log+700,
                                             ifelse(date>=as.Date('2021-03-01')&date<=as.Date('2021-04-30'),return_pre_log+600,
                                                    ifelse(date>=as.Date('2021-05-01')&date<=as.Date('2021-06-30'),return_pre_log+500,
                                                           ifelse(date>=as.Date('2021-07-01')&date<=as.Date('2021-09-30'),return_pre_log+400,
                                                                  ifelse(date>=as.Date('2021-10-01')&date<=as.Date('2021-11-30'),return_pre_log+300,
                                                                         ifelse(date>=as.Date('2021-12-01')&date<=as.Date('2021-12-31'),return_pre_log+200+500,return_pre_log)))))))))
  full$date<-as.Date(full$date)
  return(full)
})