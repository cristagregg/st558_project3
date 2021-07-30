#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(htmlwidgets)
library(caret)
library(gbm)
library(rpart.plot)

combined <- read_csv('combined.csv')[,-1] 
cols <- names(combined)[4:9]

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = 'Wildfire dashboard'),
    dashboardSidebar(width = 140,
        sidebarMenu(
            menuItem("About", tabName = "About", icon = icon("About")),
            menuItem("Data", tabName = "Data", icon = icon("Data")),
            menuItem("Exploration", tabName = "Exploration", icon = icon("Exploration")),
            menuItem("Modeling", tabName = "Modeling", icon = icon("Modeling"))
        )
    ),
    dashboardBody(
        tabItems(
            #First tab - information on the project
            tabItem(tabName = 'About',
                    fluidPage(
                    # Application title
                        titlePanel("Wildfire Data"),
                        sidebarLayout(
                            sidebarPanel(
                                img(src='wildfire.png', height = 350, width = 270)
                            ),
                            
                            mainPanel(
                                h3('Understanding the Prevalence and Causes of Wildfires'),
                                h4('Background'),
                                p('Wildfires have become an extremely common and concerning problem across the United States, particularly across the Western states. Increasing drought and record heatwaves have triggered more and more fires that quickly become out of control. These fires have enormous impacts on human life, causing people to lose their homes and entire towns being destroyed.'),
                                h4('Purpose'),
                                p('The purpose of this project is to raise awareness on the impact and trends of wildfires and investigate how weather patterns affect the number and severity of wildfires. We will create a model to predict wildfires by climate data. We will use data provided by the US Department of Agriculture which provides information on wildfires from 1992 to 2018. This data will be summarized by state, year, and month, prior to using in this app due to the size of the original file. We will also use a climate dataset provided by the National Centers for Environmental Information. These data will be combined for this project. You can find more information about these original datasets via these links:'),
                                a(href='https://www.fs.usda.gov/rds/archive/Catalog/RDS-2013-0009.5', 'Wildfire data'),
                                br(),
                                a(href='https://www.ncdc.noaa.gov/cag/divisional/mapping/110/pcp/202106/1/value', 'Climate data'),
                                br(), 
                                h4('About the App'),
                                p('In the Data tab, you will be able to investigate and download the combined dataset. The Data Exploration tab will allow you to make some charts and summaries on historical data. Finally, the modelling page will allow you to investigate different predictive models and predict the number of fires for a chosen month.')
                            )
                        )
                    
                    )),
            
            #Second tab - data
            tabItem(tabName = 'Data',
                    fluidPage(
                        titlePanel("Data"),
                        h4('This tab contains the combined wildfire and climate dataset.'),
                        sidebarLayout(
                            sidebarPanel(
                              
                              
                              checkboxGroupInput('cols', 'Select columns you wish to view', choices = cols, selected = cols),
                              
                              selectInput('yearFilter', 'Filter by Year', choices = list('No Filter', Year = 1992:2018)),                
                            
                            downloadButton('download', "Download data")),
                            
                            mainPanel(
                              dataTableOutput('Data')
                              )
                        )
                    )),
            tabItem(tabName = 'Exploration',
                    fluidPage(
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                h2("Historical Wildfire Data"),
                                selectInput('mainGraphChoice', h3('Select the type of graph you wish to view'), 
                                            choices = list('Wildfires by Season', 'Wildfire and Climate data')),
                                selectInput('state', 'Filter by State',
                                            choices = list('No Filter', 
                                                           State = state.abb)),
                                
                                #dynamic UI 1
                                conditionalPanel(condition = "input.mainGraphChoice == 'Wildfires by Season'",
                                    selectInput("group", "Group by:",
                                            choices = list('Season', 
                                                           'Month')),
                                    downloadButton('download_plotly_widget', "Download Plot")
                            ),
                            conditionalPanel(condition = "input.mainGraphChoice == 'Wildfire and Climate data'",
                                    selectInput('season', 'Filter by Season',
                                            choices = list('No Filter',
                                                           'Spring',
                                                           'Summer',
                                                           'Fall',
                                                           'Winter')),
                                    selectInput('climateMetric', 'Select the Climate Metric',
                                            choices = list('Average Temperature',
                                                           'Average High Temperature', 
                                                           'Average Drought Severity',
                                                           'Average Precipitation')),
                                    selectInput('fireMetric', 'Select the Wildfire Metric',
                                                choices = list('Total Fires', 
                                                               'Total Acres Burned')),
                                    
                                    downloadButton('downloadPlotTwo', "Download Plot")
                                    )
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                                conditionalPanel(condition = "input.mainGraphChoice == 'Wildfires by Season'",
                                            uiOutput('info'), #dynamic UI 2    
                                            plotlyOutput("boxPlot"),
                                            tableOutput('fireSummary')
                            ),
                                conditionalPanel(condition = "input.mainGraphChoice == 'Wildfire and Climate data'",
                                             plotOutput("climateChart"),
                                             br(),
                                             textOutput('climateSummary')
                            )
                            
                            )
                        )
                    )),
            tabItem(tabName = 'Modeling',
                        fluidPage(
                            tabsetPanel(
                                tabPanel('Modeling Info',
                                         h2('Information on Model Types'),
                                         p('In the next tab we will compare the fit of three different types of models. We are attempting to predict either the total number of fires for each month over the timeframe we have data for.'),
                                         h3('Linear Regression'),
                                         p('Linear regression is one of the most common methods for modeling. It looks at a set of predictors and estimates what will happen to the response if one of the predictors or a combination of predictors change. The model is chosen by minimizing the squares of the distances between the estimated value and the actual value in the testing set. The basic Simple Linear Regression Model is shown below:'),
                                         withMathJax(),
                                         '$$Y_i = \\beta_0 + \\beta_1x_i + E_i$$',
                                         p('This model can be extended to include multiple predictors, interactions, and polynomials. This is known as Multiple Linear Regression. This model is beneficial because it is highly interpretable; it shows us the effect of each individual predictor as well as interactions. We can see if the change in the response goes up or down and in what quantity. However, this model has limited flexibility, and it relies on several statistical assumptions that could be violated.'),
                                         h3('Regression Tree'),
                                         p('Regression Trees are an intuitive method for fitting predictive models. They minimize the Residual Sum of Squares by creating splits in the predictor space, and then using the mean of the observations as the prediction. Using trees allow you to visualize the model in a way that it easy to understand. However, their usefulness is limited because it takes a "greedy" approach, selecting the most important variable first, which increases the variance of the model. This can be managed somewhat by pruning the tree back after fitting, using cross-validation.'),
                                         h3('Random Forests'),
                                         p('Random Forests is a tree based method which optimizes the development process. It is a variation on Baggin, which creates many bootstrapped trees and averages across the fitted trees. The random forest method uses a different subset of predictors for each tree. By reducing the number of predictors considered in each tree, we are able to reduce the correlation between trees to improve our results. The number of predictors used is chosen by cross-validation. The benefits of using this method are that it decreases the variance of our tree fit, giving better predictions. However, we lose interpretability, and are only able to look at variable importance measures.')
                                         ),
                                tabPanel('Model Fitting',
                                         fluidPage(
                                             fluidRow(
                                                 column(width = 2,
                                                        checkboxGroupInput('predictors', h4('Select the Predictors'),
                                                                           choices = list('avg_temp', 
                                                                                          'drought_severity',
                                                                                          'avg_precip',
                                                                                          'Year',
                                                                                          'Month'),
                                                                           selected = 'avg_temp'),
                                                        ),
                                                 column(width = 3, offset = 1,
                                                        selectInput('interactions', h4('Add interactions to Linear Model?'),
                                                             choices = list('No',
                                                                            'Yes')),
                                                        actionButton('run', '1. Fit and View Training Cross-Validation Results'),
                                                        conditionalPanel(condition = "input.run",
                                                            actionButton('compare', '2. View Test Set Results')
                                                        )),
                                                 column(width = 4, offset = 1, 
                                                        selectInput('state2', 'Select State to Model On',
                                                                    choices = state.abb, selected = 'CA'),
                                                        sliderInput('trainProp', 'Select proportion of data used for training',
                                                                    min = 0.3, max = 0.9, value = 0.8, step = 0.05)
                                                        )
                                             ),
                                             conditionalPanel(condition = "input.compare",
                                                              fluidPage(
                                                                  fluidRow(column(width = 8, offset = 3,
                                                                                  h3('Test Set Results for Selected Model'))),
                                                                  fluidRow(column(width = 4, offset = 3,
                                                                                tableOutput('testResults')
                                                              )))
                                             ),
                                             conditionalPanel(condition = "input.run",
                                                 splitLayout(
                                                     fluidPage(h3('Linear Regression'),
                                                               h4('Cross Validation Results'),
                                                               tableOutput('lm'),
                                                               h4('ANOVA output for selected model'),
                                                               tableOutput('lmAnova')),
                                                     fluidPage(h3('Regression Tree'),
                                                               h4('Cross Validation Results'),
                                                               tableOutput('tree'),
                                                               h4('Regression Tree Plot'),
                                                               plotOutput('treePlot')),
                                                     fluidPage(h3('Random Forests'),
                                                               h4('Cross Validation Results'),
                                                               tableOutput('rf'),
                                                               tableOutput('rf2'),
                                                               h4('Variable Importance Plot'),
                                                               plotOutput('impPlot'))
                                                )
                                             )
                                             
                                         )),
                                tabPanel('Predictions',
                                         fluidPage(
                                             sidebarLayout(
                                                 sidebarPanel(
                                                     h4('Select your inputs'),
                                                     selectInput('month', 'Month', 
                                                                 choices = month.abb),
                                                     sliderInput('avgTemp', 'Average Temperature', value = 75,
                                                                 min = 0, max = 130),
                                                     sliderInput('drought', 'Drought Severity', value = 0,
                                                                 min = -10, max = 10, step = 0.5),
                                                     sliderInput('precip', 'Average Precipitation', value = 5,
                                                                 min = 0, max = 18, step = 0.5),
                                                     sliderInput('year', 'Year', value = 2010,
                                                                 min = 1992, max = 2018, sep = ''),
                                                     actionButton('predict', h4(strong('Predict')))
                                                 ),
                                                 mainPanel(
                                                     h3('Make a prediction using the', strong('Random Forest'), 'model with all 5 predictors.'),
                                                     h4(textOutput('statePred')),
                                                     br(),
                                                     h3(textOutput('getrfPred'))
                                                 )
                                             )
                                         )
                                         
                                        )
                            )
                        )
                    )
                    
                    
                    
                    ))
        )
    )



