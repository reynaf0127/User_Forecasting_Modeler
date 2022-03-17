#Data Visualization
total_dau<-reactive({
  yeardata<-data%>%filter(year(date)==input$year_select)
    ggplot(data=yeardata,aes(x=date,y=dau))+
    geom_line(color=wes_palette("Zissou1", n = 1),size=0.8)+theme_minimal()+
    ggtitle("Total DAU",subtitle=paste("From ",min(yeardata$date)," to ",max(yeardata$date),sep=''))+
    xlab("Date")+
    ylab("DAU")
})
  
dau_by_type<-reactive({
  yeardata<-data%>%filter(year(date)==input$year_select)
  yeardata_long<-yeardata%>% 
    dplyr::select(date,current,new,returning) %>%
    pivot_longer(.,cols=c(current,new,returning),names_to="type", values_to="users")
  
  ggplot(data=yeardata_long,aes(x=date,y=users,color=type))+
    geom_line(size=0.8)+theme_minimal()+
    ggtitle("DAU by Type",subtitle=paste("From ",min(yeardata$date)," to ",max(yeardata$date),sep=''))+
    xlab("Date")+
    ylab("Number of players")+
    scale_color_manual(values=wes_palette("GrandBudapest2"))
})

dau_plot<-reactive({
  switch(input$dau_type_select,
         "Total DAU"=total_dau(),
         "DAU by Type"=dau_by_type())
})

output$dau_plot<-renderPlot(dau_plot())

##Revenue
daily_revenue<-reactive({
  revdata<-data%>%filter(year(date)==input$year_select)
  ggplot(data=revdata,aes(x=date,y=rev_total))+
    geom_line(color=wes_palette("Zissou1", n = 1),size=0.8)+theme_minimal()+
    ggtitle("Daily Revenue",subtitle=paste("From ",min(revdata$date)," to ",max(revdata$date),sep=''))+
    xlab("Date")+
    ylab("Revenue")+
    scale_y_continuous(labels=dollar_format())
})

monthly_revenue<-reactive({
  yeardata<-data%>%filter(year(date)==input$year_select)
  revdata<-data%>%filter(year(date)==input$year_select)%>%
    mutate(month=month(date))%>%
    group_by(month)%>%
    mutate(monthly_rev=sum(rev_total))%>%
    dplyr::select(month,monthly_rev)%>%distinct()
  ggplot(data=revdata,aes(x=factor(month),y=monthly_rev,label=dollar(monthly_rev)))+
    geom_bar(stat="identity",fill=wes_palette("Zissou1", n = 1))+theme_minimal()+
    ggtitle("Monthly Revenue",subtitle=paste("From ",min(yeardata$date)," to ",max(yeardata$date),sep=''))+
    xlab("Month")+
    scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9,10,11,12),labels=c("Jan", "Feb", "Mar","Apr", "May", "Jun","Jul", "Aug", "Sep","Oct", "Nov", "Dec"))+
    ylab("Revenue")+
    scale_y_continuous(labels=dollar_format())+
    geom_text(nudge_y=1)
})

arpu<-reactive({
  revdata<-data%>%filter(year(date)==input$year_select)
  ggplot(data=revdata,aes(x=date,y=arpu_total))+
    geom_line(color=wes_palette("Zissou1")[3],size=0.8)+theme_minimal()+
    ggtitle("ARPU",subtitle=paste("From ",min(revdata$date)," to ",max(revdata$date),sep=''))+
    xlab("Date")+
    ylab("ARPU")+
    scale_y_continuous(labels=dollar_format())
})

revenue_plot<-reactive({
  switch(input$revenue_type_select,
         "Total Monthly Revenue"=monthly_revenue(),
         "Total Daily Revenue"=daily_revenue(),
         "ARPU"=arpu())
})

output$revenue_plot<-renderPlot(revenue_plot())

##Retention
retention<-reactive({
  yeardata<-data%>%filter(year(date)==input$year_select)
  yeardata_long<-yeardata%>% 
    dplyr::select(date,current_r,new_r,returning_r) %>%
    pivot_longer(.,cols=c(current_r,new_r,returning_r),names_to="type", values_to="retention")
  
  ggplot(data=yeardata_long,aes(x=date,y=retention,color=type))+
    geom_line(size=0.8)+theme_minimal()+
    ggtitle("Retention by Type",subtitle=paste("From ",min(yeardata$date)," to ",max(yeardata$date),sep=''))+
    xlab("Date")+
    ylab("Retention Rate")+
    scale_y_continuous(labels=scales::percent)+
    scale_color_manual(values=wes_palette("GrandBudapest2"))
})

output$retention_plot<-renderPlot(retention())
