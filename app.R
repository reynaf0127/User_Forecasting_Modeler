source('./ui.R',local=TRUE)
server<-function(input,output,session){
  data<-read.csv('./data.csv')
  data$date<-as.Date(data$date,"%m/%d/%Y")
  source('./refresh.R',local=TRUE)
  source('./model_number.R',local=TRUE)
  source('./data_visual.R',local=TRUE)
  source('./sig_test.R',local=TRUE)
  source('./model.R',local=TRUE)
  source('./social_decaying.R',local=TRUE)
  source('./variance.R',local=TRUE)
  source('./project_revenue.R',local=TRUE)
  source('./overwrite_db.R',local=TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)

#Creat Temp file
tempdir()
# [1] "C:\Users\XYZ~1\AppData\Local\Temp\Rtmp86bEoJ\Rtxt32dcef24de2"
dir.create(tempdir())