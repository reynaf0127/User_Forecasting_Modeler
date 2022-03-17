#Load Packages
library(sqldf)
library(readxl)
library(writexl)
library(RSQLite)
library(scales)
library(rlang)
library(dplyr, warn.conflicts = FALSE)
library(lubridate)
library(ggplot2)
library(zoo)
library(ggpubr)
library(sitools)
library(tools)
library(tidyverse)
library(rhandsontable)
library(shiny)
library(DT)
library(reactable)
library(sodium)
library(shinythemes)
library(data.table)
library(shinycssloaders)
library(wesanderson)
library(corrplot)
library(ggsci)
library(MASS)

ui = fluidPage(
    theme = shinythemes::shinytheme("lumen"),
    titlePanel("DAU Monitoring Tool"),
             sidebarPanel(
               h5("Refresh Data"),
               actionButton("refresh", "Click to get current data",icon("refresh")),
               br(),
               helpText("This tool only include Wizard101 DAU and revenue. There is no Steam DAU, Steam revenue, advertising revenue, refund, and chargeback since they are counted separately in finance."),
               br(),
               helpText("Input model starting and ending year to see the result. Model is based on data from 2015 to 2019."),
               numericInput("current_year","Input model starting year:",value=2020),
               numericInput("current_end_year","Input model ending year:",value=year(Sys.Date())+1),
               br(),
               conditionalPanel(condition = "$('li.active a').first().html()==='Data Visualization'",
                                h5("Select a year"),
                                radioButtons("year_select","Year:",
                                             choices = list(2015,2016,2017,2018,2019,2020,2021,2022,2023,2024,2025,2026),
                                             selected = year(Sys.Date()))),
               conditionalPanel(condition = "$('li.active a').first().html()==='Social Distance Decaying Model'",
                                dateInput("vaccination", "Input a vaccination date:", value = "2021-06-01")),
               width = 3,
               style = "font-size:70%"
             ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Data Visualization",
                          h5("DAU/Revenue"),
                          p("Click Refresh to get current year data and a sample of most current data will be shown here."),
                          tableOutput("preview")%>% withSpinner(type=4,color="#0dc5c1",size=1),
                          radioButtons("kpi_select","Select DAU or Revenue:",
                                       choices = list("DAU","Revenue","Retention"),
                                       selected = "DAU"),
                          conditionalPanel(condition="input.kpi_select=='DAU'",
                                           radioButtons("dau_type_select","Select User Type:",
                                                        choices = list("Total DAU","DAU by Type"),
                                                        selected = "Total DAU"),
                                           plotOutput("dau_plot")%>% withSpinner(type=4,color="#0dc5c1",size=1)),
                          
                          conditionalPanel(condition="input.kpi_select=='Revenue'",
                                           radioButtons("revenue_type_select","Select Revenue Type:",
                                                        choices = list("Total Monthly Revenue","Total Daily Revenue","ARPU"),
                                                        selected = "Total Monthly Revenue"),
                                           plotOutput("revenue_plot")%>% withSpinner(type=4,color="#0dc5c1",size=1)),
                          
                          conditionalPanel(condition="input.kpi_select=='Retention'",
                                           plotOutput("retention_plot")%>% withSpinner(type=4,color="#0dc5c1",size=1))
                          ),
                 tabPanel("Significant Test",
                          dateRangeInput("sigtestdate","Update Test Range",start=as.Date("2015-01-01"),end=as.Date("2019-12-31"),min=as.Date("2015-01-01"),
                                         format="yyyy-mm-dd",separator = " to "),
                          textOutput("sigtestdate_validation"),
                          h5("Correlation test among numeric variables"),
                          plotOutput("correlation")%>% withSpinner(type=4,color="#0dc5c1",size=1),
                          textOutput("corr_validation"),
                          br(),
                          br(),
                          h5("Select a variabl to have a statistical test"),
                          selectInput("column", "Select a KPI variable:", 
                                      choices=c("DAU"="dau",
                                                "Current Users"="current",
                                                "New Users"="new",
                                                "Returning Users"="returning"), 
                                      selected="dau"),
                          h5("ANOVA test among categorical variables"),
                          plotOutput("aov_month")%>% withSpinner(type=4,color="#0dc5c1",size=1),
                          plotOutput("aov_wday")%>% withSpinner(type=4,color="#0dc5c1",size=1),
                          br(),
                          h5("Significant change among retention rate"),
                          p("Select a cutoff date to set the whole data range into two groups"),
                          textOutput("cutoff_validation"),
                          dateInput("cutoffdate", "Cutoff Date:", value = "2019-07-01"),
                          p("The observed sample statistics were:"),
                          textOutput('mean'),
                          p("The observed t test statistic :"),
                          textOutput('tvalue'),
                          p("A low P value (normally<=0.05) suggests that there is siginificant difference between two groups"),
                          textOutput('pvalue')
                          ),
                 tabPanel("User Forecasting Model",
                          selectInput("column_model", "Select a KPI variable:", 
                                      choices=c("DAU"="dau",
                                                "Current Users"="current",
                                                "New Users"="new",
                                                "Returning Users"="returning",
                                                "Current User Retention"="current_r",
                                                "New User Retention"="new_r",
                                                "Returning User Retention"="returning_r"), 
                                      selected="dau"),
                          radioButtons("model_method","Select a regression method:",
                                       choices=list("Model1: Simple linear function",
                                                    "Model2: Log linear function to smooth the year trend"
                                                 ),
                                       selected="Model1: Simple linear function"
                                       ),
                          conditionalPanel(condition="input.model_method=='Model1: Simple linear function'",
                                       h5("Model1: Simple linear function"),
                                       p("Standard error with train data:"),
                                       textOutput("stdev_train1"),
                                       p("R square:"),
                                       textOutput("rsquar1"),
                                       p("Residual zero test:"),
                                       textOutput("residuals1")),
                          conditionalPanel(condition="input.model_method=='Model2: Log linear function to smooth the year trend'",
                                       h5("Model2: Log linear function to smooth the year trend"),
                                       p("Standard error with train data:"),
                                       textOutput("stdev_train2"),
                                       p("R square:"),
                                       textOutput("rsquar2"),
                                       p("Residual zero test:"),
                                       textOutput("residuals2"),
                                       p("Cross test if mean is equal with test data:")),
                          h5("User Forecasting Models"),
                          plotOutput("linear_model_plot")
                          ),
                 tabPanel("Model Variance",
                          h5("User Forecasting Model Variance"),
                          tableOutput("variance")
                          ),
                 tabPanel("Social Distance Decaying Model",
                          h5("Social Distance Model with Retention Data"),
                          p("After Covid 19, new users and returning users will decrease gradually and retention rate will have change. This tool can be used as a real-time monitor of DAU by most recent data"),
                          p("Use the significant test tool to test the difference among two date frame. And input average retention rate"),
                          rHandsontableOutput("model_number"),
                          helpText("Please save this models table first then refresh the page to see new model output below."),
                          textOutput("model_number_validation"),
                          br(),
                          actionButton("save_model_number", "Save Models",icon("save")),
                          br(),
                          h5("Social Decaying Models"),
                          radioButtons("model_column","Select a DAU type to see its models:",
                                       choices=list("DAU",
                                                    "Current Users",
                                                    "New Users",
                                                    "Returning Users"
                                       ),
                                       selected="DAU"),
                          plotOutput("social_decaying")%>% withSpinner(type=4,color="#0dc5c1",size=1),
                          # textOutput("test_number"),
                          downloadButton("downloadData", "Download Table")
                          # br(),
                          # h5("Write Result to DB"),
                          # numericInput("model_number_db","Choose a model to write to Vertica",4,min=1,step=1),
                          # actionButton("write", "Click to write data to Vertica",icon("cloud"))
                          ),
                 tabPanel("Projected Revenue and Variance",
                          h5("ARPU Trend"),
                          helpText("This regression is based on polynomial on crowns arpu and linear on other items arpu"),
                          radioButtons("arpu_type_forecasting","Select a product type to see its arpu:",
                                       choices=list("Crowns"=2,
                                                    "Subscription"=5,
                                                    "In Game Bundle"=3,
                                                    "Prepaid Cards"=4,
                                                    "Total"=6),
                                       selected=6
                                       ),
                          plotOutput("arpu_trend")%>%withSpinner(type=4,color="#0dc5c1",size=1),
                          br(),
                          h5("Projected Revenue and Variance Check from 2020"),
                          helpText("Users can adjust total ARPU in this table based on above ARPU model numbers. Click save after editing"),
                          rHandsontableOutput("arpu_adj"),
                          helpText("Please save this table first then refresh the page to see new revenue output below."),
                          actionButton("save_arpu_adj", "Save Adjusted ARPU",icon("save")),
                          br(),
                          helpText("This is based on forecasting ARPU from model, user-input projected ARPU and most recent variables forecasting model"),
                          tableOutput("project_revenue")%>%withSpinner(type=4,color="#0dc5c1",size=1),
                          downloadButton("downloadRevenue", "Download Project Daily Revenue Data")
                          )
                 ),
               width = 9,
               style = "font-size:85%"
    )
    )

