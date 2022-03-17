#Projected Revenue Page
data_rev1<-data%>%
  filter(!is.na(current)&date<=as.Date('2020-10-31'))%>%
  mutate(year_num=(year(date)-2011+1),year=year(date))%>%
  group_by(year)%>%
  mutate(year_arpu_crowns=mean(arpu_crowns),
         year_arpu_ingame=mean(arpu_ingame),
         year_arpu_ppdcard=mean(arpu_ppdcard),
         year_arpu_sub_ppt=mean(arpu_sub_ppt),
         year_arpu_total=mean(arpu_total))%>%
  dplyr::select(year_num,year,year_arpu_crowns,year_arpu_ingame,year_arpu_ppdcard,year_arpu_sub_ppt,year_arpu_total)%>%
  distinct()%>%
  arrange(year_num)

data_2011_2014<-data.frame(matrix(NA, nrow=4,ncol=7))
colnames(data_2011_2014)<-colnames(data_rev1)
data_2011_2014<-data_2011_2014 %>%
  mutate(year_num=seq(1,4),
         year=seq(2011,2014),
         year_arpu_crowns=c(0.317076911,0.388114492,0.402093696,0.426681361),
         year_arpu_ingame=c(0.005459004,0.031535388,0.038472077,0.044475633),
         year_arpu_ppdcard=c(0.226575311,0.243133525,0.255536659,0.219007386),
         year_arpu_sub_ppt=c(0.232145068,0.269854117,0.314182318,0.333954644),
         year_arpu_total=c(0.781256295,0.932637522,1.01028475,1.024119024))%>%
  arrange(year)

data_rev1<-rbind(as.data.frame(data_rev1),data_2011_2014)%>%
  arrange(year)

data_rev2<-data%>%
  mutate(year_num=(year(date)-2011+1),year=year(date))

data_rev3<-data_rev2%>%
  mutate(
    arpu_crowns_pre=predict(lm(year_arpu_crowns~year_num + I(year_num^2),data=data_rev1),newdata=data_rev2,type="response"),
    arpu_ingame_pre=predict(lm(year_arpu_ingame~year_num,data=data_rev1),newdata=data_rev2,type="response"),
    arpu_ppdcard_pre=predict(lm(year_arpu_ppdcard~year_num,data=data_rev1),newdata=data_rev2,type="response"),
    arpu_sub_ppt_pre=predict(lm(year_arpu_sub_ppt~year_num,data=data_rev1),newdata=data_rev2,type="response"))


data_rev4<-data_rev3 %>%
  group_by(year)%>%
  mutate(year_arpu_crowns=mean(arpu_crowns),
         year_arpu_ingame=mean(arpu_ingame),
         year_arpu_ppdcard=mean(arpu_ppdcard),
         year_arpu_sub_ppt=mean(arpu_sub_ppt),
         year_arpu_total=mean(arpu_total),
         year_arpu_crowns_pre=mean(arpu_crowns_pre),
         year_arpu_ingame_pre=mean(arpu_ingame_pre),
         year_arpu_ppdcard_pre=mean(arpu_ppdcard_pre),
         year_arpu_sub_ppt_pre=mean(arpu_sub_ppt_pre))%>%
  mutate(year_arpu_total_pre=year_arpu_crowns_pre+year_arpu_ingame_pre+year_arpu_ppdcard_pre+year_arpu_sub_ppt_pre)%>%
  dplyr::select(year,year_arpu_crowns,year_arpu_ingame,year_arpu_ppdcard,year_arpu_sub_ppt,year_arpu_total,
                year_arpu_crowns_pre,year_arpu_ingame_pre,year_arpu_ppdcard_pre,year_arpu_sub_ppt_pre,year_arpu_total_pre)%>%
  distinct()

#Start plotting
output$arpu_trend<-renderPlot({
  a<-as.numeric(input$arpu_type_forecasting)
  ggplot(data=data_rev4,aes(x=as.integer(year)))+
    geom_line(aes(y=data_rev4[[a]]),color=wes_palette("Zissou1")[1],size=2)+
    geom_line(aes(y=data_rev4[[a+5]]),color=wes_palette("Zissou1")[3],size=2)+
    theme_minimal()+
    ggtitle("ARPU Trend",subtitle=paste("From ",min(data_rev4$year)," to ",max(data_rev4$year),sep=''))+
    xlab("Year")+
    ylab("ARPU")+
    geom_label(aes(y=data_rev4[[a+5]],label=dollar(data_rev4[[a+5]])),nudge_x=0.35,size=4)+
    scale_y_continuous(labels=dollar_format())
})

#Revenue table: actual revenue from 2020, predict revenue
#Add option to input and manualy adjust total ARPU
arpu_adj<-read_excel('./arpu_adjustment.xlsx')

output$arpu_adj<-renderRHandsontable({
  rhandsontable(arpu_adj,rowHeaders = NULL) %>%
    hot_col('year_adj',format='0',readOnly = TRUE)%>%
    hot_col('arpu_total_adj',allowInvalid=TRUE,min=0,max=10)%>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

observeEvent(input$save_arpu_adj, {
  showNotification(paste("Save Successfully"),
                   closeButton=TRUE,
                   type="message")
})

observeEvent(input$save_arpu_adj, {
  x<-hot_to_r(isolate(input$arpu_adj))
  write_xlsx(x,'./arpu_adjustment.xlsx',col_names=TRUE)
})

data_rev5<-reactive({
  result1<-result1()
  data_rev5<-sqldf("
                   select r1.*,d3.year,d3.arpu_crowns_pre,d3.arpu_ingame_pre,d3.arpu_ppdcard_pre,d3.arpu_sub_ppt_pre,
                   arpu_crowns_pre+arpu_ingame_pre+arpu_ppdcard_pre+arpu_sub_ppt_pre as arpu_total_pre,
                   aa.arpu_total_adj as arpu_total_adjust
                   from result1 as r1
                   join data_rev3 as d3 on r1.date=d3.date
                   left join arpu_adj as aa on aa.year_adj=d3.year
                   ")
  data_rev5<-data_rev5 %>%
    mutate(forecasting_crowns=data_rev5[,ncol(data_rev5)-10]*data_rev5[,ncol(data_rev5)-5],
           forecasting_ingame=data_rev5[,ncol(data_rev5)-10]*data_rev5[,ncol(data_rev5)-4],
           forecasting_ppdcard=data_rev5[,ncol(data_rev5)-10]*data_rev5[,ncol(data_rev5)-3],
           forecasting_sub=data_rev5[,ncol(data_rev5)-10]*data_rev5[,ncol(data_rev5)-2],
           forecasting_total=data_rev5[,ncol(data_rev5)-10]*data_rev5[,ncol(data_rev5)-1],
           user_adjustment_total=data_rev5[,ncol(data_rev5)-10]*data_rev5[,ncol(data_rev5)],
           )
  return(data_rev5)
})

data_rev6<-reactive({
  data_rev5<-data_rev5()
  full_data<-full_data()
  data_rev6<-data_rev5 %>%
    dplyr::select(date,year,forecasting_crowns,forecasting_ingame,forecasting_ppdcard,forecasting_sub,forecasting_total,user_adjustment_total)
  data_rev6<-sqldf("
                   select d6.*,f.rev_crowns,f.rev_ingame,f.rev_ppdcard,f.rev_sub_ppt,f.rev_total
                   from data_rev6 as d6
                   join full_data as f on f.date=d6.date
                   "
  )
  return(data_rev6)
})

data_rev7<-reactive({
  data_rev6<-data_rev6()
  data_rev7<-data_rev6 %>%
    mutate(crowns_diff=(rev_crowns-forecasting_crowns)/rev_crowns,
           ingame_diff=(rev_ingame-forecasting_ingame)/rev_ingame,
           ppdcard_diff=(rev_ppdcard-forecasting_ppdcard)/rev_ppdcard,
           sub_diff=(rev_sub_ppt-forecasting_sub)/rev_sub_ppt,
           total_diff=(rev_total-forecasting_total)/rev_total,
           total_adjustment_diff=(rev_total-user_adjustment_total)/rev_total)%>%
    mutate(month=month(date))%>%
    group_by(year,month)%>%
    mutate(crowns_variance=percent(mean(crowns_diff)),
           ingame_variance=percent(mean(ingame_diff)),
           ppdcard_variance=percent(mean(ppdcard_diff)),
           sub_variance=percent(mean(sub_diff)),
           total_variance=percent(mean(total_diff)),
           total_adjustment_variance=percent(mean(total_adjustment_diff)))%>%
    dplyr::select(year,month,crowns_variance,ingame_variance,ppdcard_variance,sub_variance,total_variance,total_adjustment_variance)%>%
    distinct()%>%
    arrange(year,month)
  return(data_rev7)
})

output$project_revenue<-renderTable(data_rev7())


output$downloadRevenue <- downloadHandler(
  filename = function() {
    paste("Final Revenue Data", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(data_rev6(), file, row.names = FALSE)
  }
)

