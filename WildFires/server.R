library(shiny)
library(shinydashboard)
library(DBI)
library(RSQLite)
library(tidyverse)
library(DT)
unzip('wildfires.zip')
#fires <- tbl(con, sql("select FIRE_NAME as fire_name, FIRE_YEAR as fire_year, NWCG_GENERAL_CAUSE as cause, CONT_DATE as date, FIRE_SIZE as size, FIRE_SIZE_CLASS as size_class, LATITUDE as latitude, LONGITUDE as longitude, STATE as state 
             #from Fires
                      #limit 500")) %>% collect()

#info for first tab


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    #for data tab
    data <- reactive({
      if (input$dataType == 'Fires Data'){
        con <- dbConnect(SQLite(), 'wildfires.sqlite')
        on.exit(dbDisconnect(con), add = TRUE)
        dbGetQuery(con, paste0("select FIRE_NAME as Name, FIRE_YEAR as Year, NWCG_GENERAL_CAUSE as Cause, CONT_DATE as Date, FIRE_SIZE as Size, FIRE_SIZE_CLASS as Class, LATITUDE as Latitude, LONGITUDE as Longitude, STATE as State 
               from Fires",
                             if (input$yearFilter != 'No Filter') paste0(" where Year = ", input$yearFilter),
                             " order by Date desc 
                             limit 5000"
      )) %>% collect()
    }})
    
    output$Data <- renderDataTable(datatable(data()))
    output$download <- downloadHandler(
      filename = function(){'data.csv'},
      content = function(fname){
        write.csv(data(), fname)
      }
    )
 
    
    # for exploration tab
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })



})
