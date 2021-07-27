library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)

#data
combined <- read_csv('combined.csv')[,-1] %>%
  mutate(Season = factor(ifelse(Month %in% c(3,4,5), "Spring", 
                                ifelse(Month %in% c(6,7,8), 'Summer', 
                                       ifelse(Month %in% c(9,10,11), 'Fall', 'Winter')))))

combined$Month <- factor(combined$Month)
levels(combined$Month) <- month.abb

# Define server logic 
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

    
    # for data exploration tab
      #create boxplot for wildfire distribution by season and month
      #allow for filtering
    
    combined_visualize <- reactive({
      if (input$state != 'No Filter') {filter(combined, State == input$state)
      } else { 
        combined
      }
    })
    
    output$boxPlot <- renderPlotly({
      if (input$group == 'Season'){
        plot_ly(combined_visualize(), x = ~Season, y = ~total_fires, alpha = 0.3, type = 'box')
      } else {
        plot_ly(combined_visualize(), x = ~Month, y = ~total_fires, alpha = 0.3, type = 'box')
      }
    })
    
      #create summaries for wildfires by season and month
    output$fireSummary <- renderTable(

      combined_visualize() %>% 
          group_by_at(.vars = input$group) %>%
          summarise(mean_num_fires = mean(total_fires), median_num_fires = median(total_fires), mean_fire_size = mean(total_acres_burned)/mean(total_fires), median_fire_size = median(total_acres_burned)/median(total_fires))
      
)


})
