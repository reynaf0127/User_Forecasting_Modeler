#Model Check
#A full data table with user forecasting model
full_hist<-reactive({
  train<-train()
  full_hist<-train%>%
    mutate(dau_pre_linear=predict(stepAIC(lm(train[,26]~year+month*wday,data=train),trace=0),newdata=train,type="response"),
           dau_pre_log=exp(predict(stepAIC(lm(log(train[,26])~year+month*wday,data=train),trace=0),newdata=train,type="response")))
  full_hist$date<-as.Date(full_hist$date)
  full_hist<-full_hist%>%
    dplyr::select(date,rev_total,arpu_total,dau,year,month,wday,dau_pre_linear,dau_pre_log)%>%
    dplyr::group_by(year,month)%>%
    mutate(avg_year_arpu=mean(arpu_total))%>%
    mutate(rev_total_pre_linear=dau_pre_linear*avg_year_arpu,
           rev_total_pre_log=dau_pre_log*avg_year_arpu)
  return(full_hist)
})



variance<-reactive({
  full_hist<-full_hist()
  full_temp<-full_hist%>%
    dplyr::group_by(year,month)%>%
    mutate(monthly_rev_actual=sum(rev_total),
           monthly_rev_linear=sum(rev_total_pre_linear),
           monthly_rev_log=sum(rev_total_pre_log),
           monthly_dau_actual=mean(dau),
           monthly_dau_linear=mean(dau_pre_linear),
           monthly_dau_log=mean(dau_pre_log))%>%
    dplyr::select(year,month,monthly_rev_actual,monthly_rev_linear,monthly_rev_log,
                  monthly_dau_actual,monthly_dau_linear,monthly_dau_log)%>%
    distinct()
  variance<-full_temp%>%
    mutate(rev_diff_linear=percent((monthly_rev_actual-monthly_rev_linear)/monthly_rev_actual),
           rev_diff_log=percent((monthly_rev_actual-monthly_rev_log)/monthly_rev_actual),
           dau_diff_linear=percent((monthly_dau_actual-monthly_dau_linear)/monthly_dau_actual),
           dau_diff_log=percent((monthly_dau_actual-monthly_dau_log)/monthly_dau_actual),
           year=as.integer(year-1+2015),
           monthly_rev_actual=dollar(monthly_rev_actual),
           monthly_rev_linear=dollar(monthly_rev_linear),
           monthly_rev_log=dollar(monthly_rev_log),
           monthly_dau_actual=format(monthly_dau_actual,big.mark=",",digits=0,scientific = FALSE),
           monthly_dau_linear=format(monthly_dau_linear,big.mark=",",digits=0,scientific = FALSE),
           monthly_dau_log=format(monthly_dau_log,big.mark=",",digits=0,scientific = FALSE)
           )%>%
    arrange(year,month)
  
})

output$variance<-renderTable(variance())