observeEvent(input$write, {
  source("/opt/ki_r/ki_db.R")
  vertica <- ki_initVertica("vertica")
  actual<-result1()
  # k<-input$model_number_db
  # k_dau<-k+12+(k-1)*3
  # k_c<-k+13+(k-1)*3
  # k_n<-k+14+(k-1)*3
  # k_r<-k+15+(k-1)*3
  # result<-actual %>%
  #   select(c(k_dau,k_c,k_n,k_r))
  dbWriteTable(vertica,"sfeng.WizardDecayingModel",actual,overwrite=TRUE)
})
