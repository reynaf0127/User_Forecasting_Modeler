result1<-reactive({
  full<-full_data()
  x<-as.data.frame(model_number)
  n<-max(x$model_num)
  actual<-full%>%
    dplyr::select(date,current,new,returning,current_r,new_r,returning_r,dau,dau_pre_log,current_pre_log,new_pre_log,return_pre_log)
  
  model_data<-full%>%
    dplyr::mutate(dau_actual_new=as.numeric(ifelse(date<=as.Date('2020-06-13'),dau,NA)),
                  current_new=as.numeric(ifelse(date<=as.Date('2020-06-13'),current,NA)),
                  new_new=as.numeric(ifelse(date<=as.Date('2020-06-13'),new,NA)),
                  return_new=as.numeric(ifelse(date<=as.Date('2020-06-13'),returning,NA)))%>%
    dplyr::arrange(date)%>%
    dplyr::select(date,current,new,returning,dau,dau_pre_log,current_pre_log,new_pre_log,return_pre_log,dau_actual_new,current_new,new_new,return_new,current_r_log,new_r_log,return_r_log)
  
  y<-nrow(model_data%>%filter(!is.na(current_new)))+1
  z<-nrow(model_data)
  for (j in y:z) {
    model_data[j,12]<-model_data[j-1,12]-20.74
    model_data[j,13]<-model_data[j-1,13]-24.57
    model_data_temp1<-model_data
  }

  for (i in 1:n){
    retention<-full%>%
      dplyr::filter(date<=x[i,2]&date>=(x[1,2]-15)&current_r<1)#The most recent 15 days of retention
    cr<-mean(retention$current_r)
    nr<-mean(retention$new_r)
    rr<-mean(retention$returning_r)

    model_data_temp2<-model_data_temp1%>%
      dplyr::mutate(dau_actual_new=as.numeric(ifelse(date<=x[i,2],dau,NA)),
             current_new=as.numeric(ifelse(date<=x[i,2],current,NA)),
             new_new=as.numeric(ifelse(date<=x[i,2],new,ifelse(new_new>new_pre_log,new_new,new_pre_log))),
             return_new=as.numeric(ifelse(date<=x[i,2],returning,ifelse(return_new>return_pre_log,return_new,return_pre_log))))%>%
      dplyr::arrange(date)%>%
      dplyr::select(date,current,new,returning,dau,dau_pre_log,current_pre_log,new_pre_log,return_pre_log,dau_actual_new,current_new,new_new,return_new,current_r_log,new_r_log,return_r_log)

    y1<-nrow(model_data_temp2%>%filter(!is.na(current_new)))+1
    z1<-nrow(model_data_temp2)

        for (h in y1:z1) {
          # model_data_temp2[h,10]<-model_data_temp2[h-2,10]*model_data_temp2[h,13]+model_data_temp2[h-7,11]*nr+model_data_temp2[h-7,12]*model_data_temp2[h,15] #scenario 2 predictive retention
          model_data_temp2[h,11]<-model_data_temp2[h-2,11]*cr+model_data_temp2[h-7,12]*nr+model_data_temp2[h-7,13]*rr #scenario 3 most current retention
          model_data_temp2[h,10]<-model_data_temp2[h,11]+model_data_temp2[h,12]+model_data_temp2[h,13]
          model_data_final<-model_data_temp2
        }
        model_data_final<-model_data_final%>%
             dplyr::mutate(dau_actual_model=as.numeric(ifelse(date<input$vaccination,dau_actual_new,dau_pre_log)),
                           current_model=as.numeric(ifelse(date<input$vaccination,current_new,current_pre_log)),
                           new_model=as.numeric(ifelse(date<input$vaccination,new_new,new_pre_log)),
                           return_model=as.numeric(ifelse(date<input$vaccination,return_new,return_pre_log)))%>%
             dplyr::select(dau_actual_model,current_model,new_model,return_model)
    names(model_data_final)[1]<-paste("dau_actual_model",i,sep="")
    names(model_data_final)[2]<-paste("current_model",i,sep="")
    names(model_data_final)[3]<-paste("new_model",i,sep="")
    names(model_data_final)[4]<-paste("return_model",i,sep="")

    actual<-cbind(actual,model_data_final)
  }
  return(actual)
})

social_decaying_plot<-reactive({
  switch(input$model_column,
         "DAU"=result2(),
         "Current Users"=result3(),
         "New Users"=result4(),
         "Returning Users"=result5())
})

result2<-reactive({
  actual<-result1()
  m<-max(actual$dau)+1000
  x<-as.data.frame(model_number)
  n<-max(x$model_num)
  plot<-ggplot(data=actual,aes(x=date))+
    geom_line(aes(y=dau_pre_log),size=1,color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[2])
  for (k in 1:n){
    mdate<-x[k,2]
    plot<-plot+
      geom_line(aes_string(y=actual[,k+12+(k-1)*3]),size=1,
                color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[k+2])+
      geom_text(aes_string(x=as.Date('2020-11-30')-(k-1)*60, y=max(actual[,k+12+(k-1)*3])), label=paste("Model:",mdate),color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[k+2])
  }

  plot<-plot+
    geom_line(aes(y=dau),size=1,color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[1])+
    ggtitle("Social Decaying Models")+
    xlab("Date")+ylab("Number of Users")+ylim(0,m)+theme_light()
  return(plot)
})

result3<-reactive({
  actual<-result1()
  m<-max(actual$current)+500
  x<-as.data.frame(model_number)
  n<-max(x$model_num)
  plot<-ggplot(data=actual,aes(x=date))+
    geom_line(aes(y=current_pre_log),size=1,color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[2])
  for (k in 1:n){
    mdate<-x[k,2]
    plot<-plot+
      geom_line(aes_string(y=actual[,k+13+(k-1)*3]),size=1,color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[k+2])+
      geom_text(aes_string(x=as.Date('2020-11-30')-(k-1)*60, y=max(actual[,k+13+(k-1)*3])), label=paste("Model:",mdate),color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[k+2])
  }
  
  plot<-plot+
    geom_line(aes(y=current),size=1,color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[1])+
    ggtitle("Social Decaying Models")+
    xlab("Date")+ylab("Number of Users")+ylim(0,m)+theme_light()
  return(plot)
})


result4<-reactive({
  actual<-result1()
  m<-max(actual$new)+500
  x<-as.data.frame(model_number)
  n<-max(x$model_num)
  plot<-ggplot(data=actual,aes(x=date))+
    geom_line(aes(y=new_pre_log),size=1,color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[2])
  for (k in 1:n){
    mdate<-x[k,2]
    plot<-plot+
      geom_line(aes_string(y=actual[,k+14+(k-1)*3]),size=1,color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[k+2])+
      geom_text(aes_string(x=as.Date('2020-11-30')-(k-1)*60, y=max(actual[,k+14+(k-1)*3])), label=paste("Model:",mdate),color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[k+2])
  }
  
  plot<-plot+
    geom_line(aes(y=new),size=1,color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[1])+
    ggtitle("Social Decaying Models")+
    xlab("Date")+ylab("Number of Users")+ylim(0,m)+theme_light()
  return(plot)
})


result5<-reactive({
  actual<-result1()
  m<-max(actual$returning)+500
  x<-as.data.frame(model_number)
  n<-max(x$model_num)
  plot<-ggplot(data=actual,aes(x=date))+
    geom_line(aes(y=return_pre_log),size=1,color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[2])
  for (k in 1:n){
    mdate<-x[k,2]
    plot<-plot+
      geom_line(aes_string(y=actual[,k+15+(k-1)*3]),size=1,color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[k+2])+
      geom_text(aes_string(x=as.Date('2020-11-30')-(k-1)*60, y=max(actual[,k+15+(k-1)*3])), label=paste("Model:",mdate),color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[k+2])
  }
  
  plot<-plot+
    geom_line(aes(y=returning),size=1,color=wes_palette("Darjeeling2", max(x$model_num)+2, type = "continuous")[1])+
    ggtitle("Social Decaying Models")+
    xlab("Date")+ylab("Number of Users")+ylim(0,m)+theme_light()
  return(plot)
})

output$social_decaying<-renderPlot(social_decaying_plot())

# output$test_number<-renderText({
#   actual<-result1()
#   x<-as.data.frame(model_number)
#   n<-max(x$model_num)
#   return(n)
# })

output$downloadData <- downloadHandler(
  filename = function() {
    paste("Final DAU Data", ".csv", sep = "")
  },
  content = function(file) {
    write.csv(result1(), file, row.names = FALSE)
  }
)

