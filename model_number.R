model_number<-read_excel('./model number.xlsx')
model_number$model_date<-as.Date(model_number$model_date)

output$model_number<-renderRHandsontable({
  rhandsontable(model_number,rowHeaders = NULL) %>%
    hot_col('model_num',format='0')%>%
    hot_validate_numeric(col='model_num', min=1,allowInvalid=TRUE)%>%
    hot_col('model_date')
})

model_num_seq<-seq(1,10)

output$model_number_validation<-renderText({
  val<-hot_to_r(input$model_number)
  d<-data%>% filter(!is.na(current))
  md<-max(d$date)
  validate(
    need(all(val$model_num %in% model_num_seq),"Please check the model number"),
    need(length(unique(val$model_num))==length(val$model_num),"Model numbers are not repeatable"),
    need(md-max(val$model_date)>7,"Please enter a date at least one week ago to have accurate retention rate")
    )
  ""
})

observeEvent(input$save_model_number, {
  showNotification(paste("Save Successfully"),
                   closeButton=TRUE,
                   type="message")
})

observeEvent(input$save_model_number, {
  x<-hot_to_r(isolate(input$model_number))
  write_xlsx(x,'./model number.xlsx',col_names=TRUE)
})
