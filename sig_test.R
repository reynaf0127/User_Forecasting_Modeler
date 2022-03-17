##Significant test
output$sigtestdate_validation<-renderText({
  validate(
    need(input$sigtestdate[2]>input$sigtestdate[1],"End date is earlier than start date")
  )
""})

data_full<-reactive({
  data_full<-data%>%
    filter(date>=input$sigtestdate[1]&date<=input$sigtestdate[2])%>%
    mutate(year=(year(date)-2015+1),
           month=as.factor(month(date)),wday=as.factor(wday(date)),
           new_log=log(new),current_log=log(current),returning_log=log(returning),
           dau_log=log(dau))
  return(data_full)
})
output$correlation<-renderPlot({
  data_full_value<-data_full()
  corrplot(cor(data_full_value[,c(2:7,26,27,30:33)]),method="number")
    })
output$corr_validation<-renderText({
  data_full_value<-data_full()
  validate(
    need(length(unique(data_full_value$year))>1,"Warning: There is only one year in the data range. So correlation test can't use in year variable.")
  )
  ""})

##ANOVA test
aov_month<-reactive({
  data_full_value<-data_full()
  aov_month<-data_full_value%>%
    mutate(col=data_full_value[,input$column])%>%
    group_by(month)%>%
    mutate(avg=mean(col))%>%
    dplyr::select(avg,month)%>%distinct()
  ggplot(data=aov_month,aes(x=month,y=avg,label=format(avg,digits=0,big.mark=",",scientific = FALSE)))+geom_bar(stat='identity',fill=wes_palette("Rushmore1")[1])+
    scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan", "Feb", "Mar","Apr", "May", "Jun","Jul", "Aug", "Sep","Oct", "Nov", "Dec"))+
    ggtitle("Average by Month",subtitle=paste("From ",input$sigtestdate[1]," to ",input$sigtestdate[2],sep=''))+
    xlab("Month")+theme_minimal()+ylab(input$column)+
    geom_text(nudge_y=1)
})
output$aov_month<-renderPlot(aov_month())

aov_wday<-reactive({
  data_full_value<-data_full()
  aov_wday<-data_full_value%>%
    mutate(col=data_full_value[,input$column])%>%
    group_by(wday)%>%
    mutate(avg=mean(col))%>%
    dplyr::select(avg,wday)%>%distinct()
  ggplot(data=aov_wday,aes(x=wday,y=avg,label=format(avg,digits=0,big.mark=",",scientific = FALSE)))+geom_bar(stat='identity',fill=wes_palette("Cavalcanti1")[4])+
    scale_x_discrete(breaks=c(1,2,3,4,5,6,7),labels=c("Sun", "Mon", "Tue","Wed", "Thu", "Fri","Sat"))+
    ggtitle("Average by Weekday",subtitle=paste("From ",input$sigtestdate[1]," to ",input$sigtestdate[2],sep=''))+
    xlab("Weekday")+theme_minimal()+ylab(input$column)+
    geom_text(nudge_y=1)
})
output$aov_wday<-renderPlot(aov_wday())

##Significant change
cutoff_validation<-reactive({
  validate(
    need(input$cutoffdate>input$sigtestdate[1],"Error: Cutoff date must be greater than data range start date."),
    need(input$cutoffdate<input$sigtestdate[2],"Error: Cutoff date must be smaller than data range end date.")
  )})
output$cutoff_validation<-renderText(cutoff_validation())

ttestout <- reactive({
  vals_data<-data_full()
  group_a<-vals_data %>% filter(date<input$cutoffdate)
  group_b<-vals_data %>% filter(date>=input$cutoffdate)
  test_a<-group_a[,input$column]
  test_b<-group_b[,input$column]
  t1<-t.test(test_a,test_b)
  return(t1)
})

#Output tvalue
output$tvalue <- renderPrint({
  vals <- ttestout()
  if (is.null(vals)){return(NULL)}
  vals$statistic
})

#Output pvalue
output$pvalue <- renderPrint({
  vals <- ttestout()
  if (is.null(vals)){return(NULL)}
  vals$p.value
})

#Output mean
output$mean <- renderPrint({
  vals <- ttestout()
  if (is.null(vals)){return(NULL)}
  vals$estimate
})
