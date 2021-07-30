library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(htmlwidgets)
library(caret)
library(gbm)
library(rpart.plot)

#data
combined <- read_csv('combined.csv')[,-1] %>%
  mutate(Season = factor(ifelse(Month %in% c(3,4,5), "Spring", 
                                ifelse(Month %in% c(6,7,8), 'Summer', 
                                       ifelse(Month %in% c(9,10,11), 'Fall', 'Winter')))))

combined$Month <- factor(combined$Month)
levels(combined$Month) <- month.abb
combined$drought_severity <- -combined$drought_severity #flip index so larger numbers indicate more severe droughts

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
    
    #display information for next chart
    output$info <- renderUI({
      text <- paste0('This chart shows the statistics for the number of wildfires in', ifelse(input$state != 'No Filter', paste(' the state of', input$state), paste(' the US')), ' broken down into ', tolower(input$group), 's.' )
      h3(text)
    })
    
    session_store <- reactiveValues()
    
    plotlyPlot <- function(){
      plot1 <- if (input$group == 'Season'){
        plot_ly(combined_visualize(), x = ~Season, y = ~total_fires, alpha = 0.3, type = 'box')
      } else {
        plot_ly(combined_visualize(), x = ~Month, y = ~total_fires, alpha = 0.3, type = 'box')
      }
    }
    
    output$boxPlot <- renderPlotly({
      plotlyPlot()
      session_store$plt <- ggplotly(plotlyPlot())
      session_store$plt
    })
    
    output$download_plotly_widget <- downloadHandler(
      filename = function() {"wildfireStats.html"},
      content = function(file) {
        # export plotly html widget as a temp file to download.
        saveWidget(as_widget(session_store$plt), file, selfcontained = TRUE)
      }
    )
    
      #create summaries for wildfires by season and month
    output$fireSummary <- renderTable(

      combined_visualize() %>% 
          group_by_at(.vars = input$group) %>%
          summarise(mean_num_fires = mean(total_fires), median_num_fires = median(total_fires), mean_fire_size = mean(total_acres_burned)/mean(total_fires), median_fire_size = median(total_acres_burned)/median(total_fires))
  )
    
 
    climate_visuals <- reactive({
      data <- if (input$season != 'No Filter') {filter(combined_visualize(), Season == input$season)
      } else { 
        combined_visualize()
      }
      data <- data %>%
        group_by(Year) %>%
        summarise(`Average Temperature` = mean(avg_temp), 
                  `Average High Temperature` = mean(max_temp),
                  `Average Drought Severity` = mean(drought_severity),
                  `Average Precipitation` = mean(avg_precip),
                  `Total Acres Burned` = sum(total_acres_burned),
                  `Total Fires` = sum(total_fires)) %>%
        pivot_longer(cols = `Average Temperature`:`Total Fires`) %>%
        filter(name == input$fireMetric | name == input$climateMetric)
    })
    
    getPlotTwo <- function(){
      plotTwo <- climate_visuals() %>%
        group_by(name) %>%
        mutate(scaled = scale(value)) %>%
        ggplot(aes(Year, scaled)) +
        geom_point(aes(col = name), size = 1.5) +
        geom_line(aes(col = name)) +
        geom_smooth(aes(col = name), size = 1.5) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5)) +
        labs(title = paste('Scaled Wildfires and Climate Data by Year', ifelse(input$state != 'No Filter', paste('in the state of', input$state), paste('in the US'))), y = 'Scaled Metric') +
        scale_color_brewer(name = 'Metric (Scaled)', palette = 'Dark2')
    }
    
    #create charts for climate comparison
    output$climateChart <- renderPlot({
      print(getPlotTwo())
    })
    
    #download plot two
    output$downloadPlotTwo <- downloadHandler(
      filename = function() {'wildfireClimateStats.png'},
      content = function(file) {
        png(file)
        print(getPlotTwo())
        dev.off()
      }
    )
    
    #numerical data for climate comparison
    output$climateSummary <- renderText({
      data <- climate_visuals() %>%
        pivot_wider(values_from = value)
      paste('There were', data[nrow(data),input$fireMetric], tolower(input$fireMetric), ifelse(input$state != 'No Filter', paste('in the state of', input$state), paste('in the US')), 'in the', ifelse(input$season != 'No Filter', paste(input$season, 'of'), 'year of'), '2018. The', tolower(input$climateMetric), 'was', round(data[nrow(data),input$climateMetric]), 'degrees. The chart above tracks your selected metrics by year from 1992 to 2018. The curve overlayed on top of the points is fitted using the LOESS method.')
    })
    
      #model fitting
        #split data and filter
    modeling <- eventReactive({input$run |
      input$predict},
        {modeling <- combined %>%
          filter(State == input$state2) %>%
          select(total_fires, input$predictors)
      })
    
    train_rows <- eventReactive({input$run |
      input$predict}, 
                                {train_rows <- sample(nrow(modeling()), input$trainProp*nrow(modeling()))
      })
    
    lm.fit <- reactive({
        lm.fit <- train(if (input$interactions == 'Yes') {
          total_fires ~ .*.
        } else {
          total_fires ~ .
        },
        data = modeling(), subset = train_rows(),
        method = 'lm',
        trControl = trainControl('cv', number = 5))
    })
    
      #display lm results when button pressed
      observeEvent(input$run,
        output$lm <- renderTable({
          withProgress(message = 'Fitting Linear Model', value = 0.2, {
          print(lm.fit()$results[2:4])
          })
      })
      )
      
      #create anova table when button pressed
      observeEvent(input$run,
        output$lmAnova <- renderTable(
          data.frame(
            anova(lm(if (isolate(input$interactions) == 'Yes') {
              total_fires ~ .*.
            } else {
              total_fires ~ .
            },
            data = modeling(), subset = train_rows()))
          ),
          rownames = T
        )
      )
      
      tree.fit <- reactive({
        tree.fit <- train(total_fires ~ ., data = modeling(), 
                         subset = train_rows(),
                         method = 'rpart', #tree method which tunes complexity parameter
                         trControl = trainControl('cv', number = 10))
      })
      #display tree results when button pressed
      observeEvent(input$run,
                   output$tree <- renderTable({
                     withProgress(message = 'Fitting Tree Model', value = 0.3, {
                     best <- which.min(tree.fit()$results[,2])
                     print(tree.fit()$results[best, 1:4])
                   })
                   })
      )
      
      #plot tree
      observeEvent(input$run,
                   output$treePlot <- renderPlot(
                     rpart.plot(tree.fit()$finalModel),
                     height = 250
                   )
      )
      
      #get random forest model
      rf.fit <- reactive({
        rf.fit <- train(total_fires ~ ., data = modeling(), 
                           subset = train_rows(),
                           method = 'rf',
                           trControl = trainControl('cv', number = 5),
                           tuneGrid = data.frame(mtry = seq(20, 40, by = 5)))
      })
      
      #display rf results when button pressed
      observeEvent(input$run,
                     output$rf <- renderTable({
                       withProgress(message = 'Fitting Random Forest Model', value = 0.5, {
                         best <- which.min(rf.fit()$results$RMSE)
                         print(rf.fit()$results[best,1:4])
                     })
                   })
      )
      
      #disply variable importance plot
      observeEvent(input$run,
                   output$impPlot <- renderPlot( 
                     ggplot(head(data.frame(variable = rownames(rf.fit()$finalModel$importance), importance = rf.fit()$finalModel$importance)), aes(reorder(variable, IncNodePurity), IncNodePurity)) +
                       geom_bar(stat = 'identity', fill = 'darkblue')  +
                       coord_flip() +
                       labs(x = 'Variable', y = 'Variable Importance'),
                     height = 250
                     )
      )
      
      #display test results
      observeEvent(input$compare,
                   output$testResults <- renderTable(
                    print(
                      cbind('Linear' = postResample(predict(lm.fit(), modeling()[-train_rows(),]), modeling()$total_fires[-train_rows()]),
                            'Tree' = postResample(predict(tree.fit(), modeling()[-train_rows(),]), modeling()$total_fires[-train_rows()]),
                            'Random Forest' = postResample(predict(rf.fit(), modeling()[-train_rows(),]), modeling()$total_fires[-train_rows()]))
                    ),
                    rownames = T
                   )
      )
      
      output$statePred <- renderText({
        print(paste0('For the state of ', input$state2, '. (Change in Model Fitting tab.)'))
      })
      
      #refit rf model using all predictors
      rf.fit.full <- reactive({
          rf.fit.full <- train(total_fires ~ avg_temp + drought_severity + avg_precip + Month + Year, data = filter(combined, State == input$state2), 
                          subset = train_rows(),
                          method = 'rf',
                          trControl = trainControl('cv', number = 5),
                          tuneGrid = data.frame(mtry = seq(30, 40, by = 5)))
        })
      
      observeEvent(input$predict,
                  output$getrfPred <- renderText({
                    withProgress(message = 'Getting Prediction', value = 0.5, {
                      print(paste0('The predicted number of fires is ', round(predict(rf.fit.full(), data.frame(avg_temp = isolate(input$avgTemp), drought_severity = isolate(input$drought), avg_precip = isolate(input$precip), Year = isolate(input$year), Month = isolate(input$month)))), '.'))
                    })
                  })
      )
      
})
    
