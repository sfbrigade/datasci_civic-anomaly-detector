## Hazmat Incident Anomaly Detection App (R + Shiny + Leaflet) for the U.S. Dept. of Transportation
## By Jude Calvillo (Data Science Working Group @ Code for San Francisco)
##
## Status: Oct. 22, 2016
## ---------------------
## v2.0 - Complete / Beta
## 
## - ALL DONE. :) - Hazmat-related news results integrated as HTML + javascript and map interactivity
##   now bug-free.
## - Next steps: 
##     > Update dataset and work with Dan @ DoT to get some kind of live/daily data feed. 
##     > Try to refine MS Cognitive Services API call for consistently more relevant news search results.
##     > Try to create and employ a custom 'month picker' Shiny input object, as Shiny Apps does not offer 
##        a month picker widget or useful dateInput option. Some useful links...
##          >> jQuery Month Picker: http://jsfiddle.net/kidsysco/JeZap/
##          >> How to Build Custom Shiny Input Objects: http://shiny.rstudio.com/articles/building-inputs.html
##     > Attempt to generalize this for MANY temporal-spatial civic anomaly detection purposes!!
## 
## ----------------------------------
##

library(shiny)
library(leaflet)
library(lubridate)
includeScript("www/MonthPicker.js")

shinyUI(fluidPage(theme = "style2.css",
                  # Customizing background.
                  list(tags$head(tags$style("body {background-image: url('http://www.sovereignmarket.com/app-assets/tunnel-background_dot9.jpg');
                  color: #333333; center repeat-x;}"))),
fluidRow(
    div(h1(" "), align="center"),
    div(
        h1(img(src="dot-logo2s.png", align="absmiddle"),
           "U.S. Dept. of Transportation: Hazmat Incident Anomaly Detector"), align="center"),
    div(h1(" "), align="center")
    ),
sidebarPanel(
    style = "background:transparent;border-color:#ffffff;box-shadow: 10px 10px 10px #cccccc;",
    p("This app helps DoT executives identify which states exhibited monthly hazmat incident totals that
      were -truly- anomalous to their respective norms, after accounting for seasonality and trend. Please 
      follow the two steps below to get started."),
    h2(style="color:#108aa0;font-weight:bold;", "Step 1: Select Month of Concern"),
    
    ## Date selection: Shiny Apps widget approach, for now (sub-optimal; have to select a specific date).
    ## Will someday attempt to create a custom month picker Shiny input object (see notes above).
    dateInput('selectdate',
              label = "",
              
              ## Default val and date range = Current month - 1 month
              value = as.Date(cut(Sys.Date(), "month")),
              min = Sys.Date() - 1825, max = as.Date(cut(Sys.Date(), "month")) - months(1),
              
              format = paste0("mm/", "01", "/yy"),
              startview = 'year'),
    
    h2(" "),
    h4(icon("truck", lib="font-awesome"),"Anomalous States"),
    tableOutput("anonSTATES"),
    p(" "),
    p(" "),
    p(" "),
    h2(style="color:#108aa0;font-weight:bold;", "Step 2: Select Anomolous State >>"),
    p("Select from the red-colored states to display some news and historical incident count context for that state."),
    # p(style="font-weight:bold;","Step 2: Select State for Context >"),
    h2(" ")
    ),
mainPanel(
    h3(style="color:#000000;", icon("map-o", lib="font-awesome"),"Anomalous States Heatmap"),
    leafletOutput("theMAP"),
    h2(" "),
    column(6, uiOutput("plotHEAD"),
            plotOutput("plotANOM")),
    column(6, uiOutput("newsHEAD"),
           h2(" "),
           h2(" "),
           dataTableOutput("newsCONTENT")
           ),
    column(12, h2(" ")),
    column(12, "Developed by", a("Jude Calvillo", href="http://linkd.in/vVlpXA"), "-", a("Data Science Working Group @ Code for San Francisco", 
                                                                                         href="http://datascience.codeforsanfrancisco.org"),
            p("For code and dev details, visit ", a("Github Repo @ Bayes Hack 2016", href="https://git.io/vPrtb")),
            h2(" "),
            h2(" "))
    )
  )
)
