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

## Load necessary libraries.
library(shiny)
library(AnomalyDetection)
library(lubridate)
library(dplyr)
library(ggplot2)
library(jsonlite)
library(httr)
# library(maps) # Loading states map as object, instead, to save a little load time)

## Load R objects for API key (MS Cognitive Services) and map polygons.
## Reformatting names of map polygons for easier reference/matching.
load(file = "data/api_key.rda")
load(file = "data/mapStates.rda")
mapStates$names <- sub(":main","", mapStates$names)
# mapStates <- map("state", fill = TRUE, plot = FALSE) # Used only if loading from library

## Get the state-by-state incident data
dat <- read.csv("data/hazmat_year_month.csv")

## States lookup table (to deal w/maps subsetting issue).
## Also, it's faster to create than to load, apparently.
states_lookup <- data.frame()
states_lookup[1:50,] <- NA
states_lookup$State <- state.name
states_lookup$State.Abb <- state.abb

## Factorize state
dat$State <- as.factor(dat$State)

# Currently, in order to make this all work, we have to filter out those territories not in the
# states_lookup dataset (e.g. filtering out Puerto Rico). We can work out a solution for getting those 
# later, if necessary.

## Get months and convert to Posix.
dat$Year.Month = as.POSIXct(strptime(dat$Year.Month, "%Y-%m-%d"))

## For later
dat2 <- data.frame(State = character(), Year.Month = as.POSIXlt(character()), Report.Number = integer())

## Create empty dataframe
res_df <- data.frame(timestamp = as.POSIXlt(character()), anoms = integer(), state = character())

## So as not to corrupt local testing
res_df2 <- data.frame()

## Pre-popped data frame, in line w/maps' "states" name ordering and proper shading in Leaflet
anom_map_states <- data.frame(State = character(), State.Abb = character(), Incidents = integer(),
                              Median = integer(), Low = integer(), High = integer())
anom_map_states[1:length(mapStates$names),] <- NA
anom_map_states$State <- mapStates$names
anom_map_states$State.Abb <- as.character(anom_map_states$State.Abb)
anom_map_states[anom_map_states$State %in% tolower(states_lookup$State), "State.Abb"] <- states_lookup[tolower(states_lookup$State) %in% 
                                                                                                           mapStates$names, "State.Abb"]
anom_map_states$Incidents <- 0


## Initiate Shiny Server instance.
shinyServer(
    function(input, output){
        
        ## React to the selected date/month by subsetting to chosen month - 5 years before, then running
        ## anomaly detection and filtering to this month. 
        the_anoms <- reactive({
            
            # par(mar=c(1,1,1,1))
            
            ## To format any date selected to the first of that month
            ## (for proper ref w/dataset) - Temp solution, until I can create a sort of 'month picker' for Shiny
            the_first <- input$selectdate
            day(the_first) <- 1
            
            ## Have to create 5 years back date as character because base won't take in
            yrs_back_5 <- as.Date(the_first) - years(5)
            dat2 <- dat[(dat$Year.Month >= as.character(yrs_back_5)) & (dat$Year.Month <= as.character(the_first)),]
            
            ## Run each state's data through anomaly detection
            for(i in 1:length(levels(dat2$State))){
                res <- AnomalyDetectionTs(dat2[dat2$State %in% levels(dat2$State)[i],c("Year.Month","Report.Number")], max_anoms=0.05,
                                          direction='pos', plot=T)
                per_state <- res$anom
                per_state$state <- rep(levels(dat2$State)[i], nrow(per_state)) # Quick and dirty solution here.
                res_df <- rbind(res_df, per_state)
            }
            names(res_df)[2:3] <- c("Incidents","State")
            res_df2 <- res_df
            
            ## Filter to selected month's states.
            res_df2 <- res_df2[res_df2$timestamp %in% as.character(the_first), c("State", "Incidents")]
            if(nrow(res_df2) > 0){
                sum_stats <- summarise(group_by(dat2[dat2$State %in% res_df2$State,], State), Median = median(Report.Number), 
                                       Low = min(Report.Number), High = max(Report.Number))
                res_df2 <- cbind(res_df2, sum_stats[,-1])
            }
            res_df2
            
        })
        
        
        ## Run selected state through anomaly detection and grab its individual plot (for selected month -> -5yrs)
        anom_plot <- reactive({
            
            if(is.null(input$theMAP_shape_click$id)){
                
                wait_message <- paste("Awaiting state selection...")
                
                ggplot() + 
                    annotate("text", x = -5:0, y = 0:5, size=8, label = "") + 
                    annotate("text", x = -2.5, y = 2.5, label = wait_message) +
                    
                    theme_bw() + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
                    xlab("Years (-5 ---> | Selected Year)") + ylab("Incidents")
                
            } else {
                
                ## Lookup state abbreviation
                selected_state <- states_lookup$State.Abb[tolower(states_lookup$State) %in% input$theMAP_shape_click$id]
                
                ## Reformat selected date to 1st of the month
                the_first <- input$selectdate
                day(the_first) <- 1
                
                ## Have to create 5 years back date as character because base won't take in
                yrs_back_5 <- as.Date(the_first) - years(5)
                dat2 <- dat[(dat$Year.Month >= as.character(yrs_back_5)) & (dat$Year.Month <= as.character(the_first)),]
                
                ## Run selected state's data through anomaly detection, to get plot.
                state_anom <- AnomalyDetectionTs(dat2[dat2$State %in% selected_state, c("Year.Month","Report.Number")], 
                                                max_anoms=0.05, direction='pos', plot=T)
                print(state_anom$plot)
            }
            
        })

        ## Get the news via API
        the_news <- reactive({
            
            if(is.null(input$theMAP_shape_click$id)){
                print("Awaiting state selection...")
                
            } else {
                
                ## Lookup state abbreviation
                state_name <- states_lookup$State[tolower(states_lookup$State) %in% input$theMAP_shape_click$id]
                
                ## Format state to account for no-spaces allowed in request URL
                state_name <- gsub(" ", "+", state_name)
                
                ## Test request URL
                req_url <- paste0("https://api.cognitive.microsoft.com/bing/v5.0/news/search?q=", "hazardous+materials+accident|incident+", state_name, "+", year(input$selectdate),
                                  "&count=100&offset=0&mkt=en-us&safeSearch=Moderate")
                #&category=Health")
                
                ## API REQUEST for hazmat news @ state + month
                req_content <- GET(url = req_url, add_headers("Ocp-Apim-Subscription-Key" = api_key, type = "basic"))
                raw_content <- content(req_content, type = "text", encoding = "UTF-8")
                news_content <- fromJSON(raw_content)par(mar=c(1,1,1,1))

                ## Convert to usable data frame
                news_df <- as.data.frame(news_content[[4]])
                news_df <- news_df[,c("name","description","url","datePublished","provider")]
                news_df$datePublished <- as.Date(news_df$datePublished)
                
                ## Filter down to news of user-selected month
                news_df$datePublished <- as.character(news_df$datePublished) # Because render table auto-converts dates to numeric
                news_df <- news_df[floor_date(as.Date(news_df$datePublished), unit = "month") 
                                   %in% floor_date(as.Date(input$selectdate), unit = "month"),]

                ## Create HTML links (for later use) by applying paste0 across a few columns
                news_df <- news_df[, c("name","datePublished","url")]
                names(news_df)[2] <- "Date"
                news_df <- cbind(news_df, "<a href='")
                news_df <- cbind(news_df, "' style='color:#108aa0;font-weight:bold'>")
                news_df <- cbind(news_df, "</a>")
                news_df <- news_df[, c(4,3,5,1,6,2)]
                news_df$Article <- apply(news_df[,-6], 1, paste0, collapse = "")
                news_df[, c("Article","Date")]

            }
        })
        
        ## Show anomolous states
        output$anonSTATES <- renderTable({the_anoms()}, include.rownames=FALSE)
        
        ## Show map
        output$theMAP <- renderLeaflet({
            
            ## Replace zeroed out states dataframe w/anomalous states' values where states match
            anom_map_states[anom_map_states$State.Abb %in% the_anoms()$State, -1] <- the_anoms()
            
            ## Polygons and layerIds from mapStates
            leaflet(data = mapStates) %>% addTiles() %>%
            addPolygons(fillColor = "#cc0000", fillOpacity = anom_map_states$Incidents * .01, 
                        layerId = mapStates$names, stroke = FALSE)
            
            })
        
        ## Render reactive anomaly plot heading UI
        output$plotHEAD <- renderUI({
            
            if(is.null(input$theMAP_shape_click$id)){
                print(h4(style="color:#000000;", icon("clock-o", lib="font-awesome"), "Incident Timeline for State (Month - 5yrs)"))
                
            } else {
                print(h4(style="color:#000000;", icon("clock-o", lib="font-awesome"), 
                         paste("Incident Timeline for", states_lookup$State[tolower(states_lookup$State) %in% input$theMAP_shape_click$id],
                               "(Month - 5yrs)")))
            }
            
        })
        
        ## Render timeline + anomalies plot for selected month
        output$plotANOM <- renderPlot({anom_plot()}, height = 280)
        
        ## Render reactive news heading UI (reacts to selected state name)
        output$newsHEAD <- renderUI({
            
            if(is.null(input$theMAP_shape_click$id)){
                
                print(h4(style="color:#000000;", icon("newspaper-o", lib="font-awesome"), "Hazmat News for Selected State @ Month"))
                
            } else {
                
                print(h4(style="color:#000000;", icon("newspaper-o", lib="font-awesome"), 
                         paste("Hazmat News for", states_lookup$State[tolower(states_lookup$State) %in% input$theMAP_shape_click$id], "@ Month")))
            }
            
        })
        
        ## Render reactive news content in HTML-enabled table
        output$newsCONTENT <- renderDataTable({

            if(is.null(input$theMAP_shape_click$id)){
                
                news_temp <- as.data.frame("Awaiting state selection")
                names(news_temp) <- "Article"
                print(news_temp)

            } else {

                the_news()
                
            }

        }, escape = F, options = list(paging = F, searching = F)) # disabled search/filtering and pagination, for now
        
  
        }
    )

