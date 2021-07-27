library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

#info for first tab
combined <- read_csv('combined.csv')[,-1] 

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    #for data tab
    data <- reactive({
      data1 <- select(combined, State, Year, Month, input$cols)
      if (input$yearFilter != 'No Filter') {filter(data1, Year == input$yearFilter)
      } else { 
        data1
        }
        
    })
    
    output$Data <- renderDataTable(datatable(data(), options = list(scrollX = T)))
    output$download <- downloadHandler(
      filename = function(){'combined.csv'},
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
