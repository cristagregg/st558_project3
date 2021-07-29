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
                                img(src='wildfire.png', height = 350, width = 240)
                            ),
                            
                            mainPanel(
                                h3('Understanding the Prevalence and Causes of Wildfires'),
                                h4('Background'),
                                p('Wildfires have become an extremely common and concerning problem across the United States, particularly across the Western states. Increasing drought and record heatwaves have triggered more and more fires that quickly become out of control. These fires have enormous impacts on human life, causing people to lose their homes and entires towns being destroyed.'),
                                br(),
                                h4('Purpose'),
                                p('The purpose of this project is to raise awareness on the impact of wildfires and understand their causes. We will also investigate how weather patterns affect the number and severity of wildfires, and create a model to predict wildfires. We will use data provided by the US Department of Agriculture which provides information on wildfires from 1992 to 2018. This data will be summarized by state, year, and month, prior to using due to the size of the file. We will also use a climate dataset provided by the National Centers for Environmental Information. These data will be combined for this project. You can find more information about these original datasets via these links:'),
                                a(href='https://www.fs.usda.gov/rds/archive/Catalog/RDS-2013-0009.5', 'Wildfire data'),
                                br(),
                                a(href='https://www.ncdc.noaa.gov/cag/divisional/mapping/110/pcp/202106/1/value', 'Climate data'),
                                br(), br(),
                                h4('About the App'),
                                p('In the Data tab, you will be able to investigate the combined dataset. The Data Exploration tab will allow you to make some charts and summaries. Finally, the modelling page will allow you to investigate different models used for prediction.')
                            )
                        )
                    
                    )),
            
            #Second tab - data
            tabItem(tabName = 'Data',
                    fluidPage(
                        titlePanel("Data"),
                        
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
                                    downloadButton('download_plotly_widget', "Download data")
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
                                    
                                    downloadButton('downloadPlotTwo', "Download data")
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
                                         p('In the next tab we will compare the fit of three different types of models. We are attempting to predict either the total number of fires or the total acres burned for each month over the timeframe we have data for.'),
                                         h3('Linear Regression'),
                                         p('Linear regression is one of the most common methods for modeling. It looks at a set of predictors and estimates what will happen to the response if one of the predictors or a combination of predictors change. The model is chosen by minimizing the squares of the distances between the estimated value and the actual value in the testing set. The basic Simple Linear Regression Model is shown below:'),
                                         withMathJax(),
                                         '$$Y_i = \\beta_0 + \\beta_1x_i + E_i$$',
                                         p('This model is beneficial because it is highly interpretable, as it shows us the effect of each individual predictor as well as interactions. We can see if the change in the response goes up or down and in what quantity. However, this model has limited flexibility, and it relies on several statistical assumptions that could be violated.'),
                                         h3('k-Nearest Neighbors'),
                                         p('k-Nearest Neighbors is a much more flexible modeling method than linear regression, as it can take any shape it needs to for prediction. This method looks at the k number (chosen by cross-validation) of neighbors to an observation we wish to classify and predicts the observation based on the majority vote (for classification) or average of the responses (for regression). This model is beneficial because it can fit very flexible models. However, we lose interpretability of the model, and we also must be careful, as the KNN method is very susceptible to the ', a(href = 'https://builtin.com/data-science/curse-dimensionality', 'Curse of Dimensionality.')),
                                         h3('Boosting'),
                                         p('Boosting is another method that is very flexible. It is a variation on a tree-based method where the final model is developed through an iterative combination of weaker models where each iteration builds upon the last. This model learns slowly, and predictions are updated as the trees are grown. This both method tends to process good results, but the model itself is not as interpretable as linear regression. This can be offset somewhat by investigating the variable importance.')
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
                                                     fluidPage(h3('k-Nearest Neighbors'),
                                                               h4('Cross Validation Results'),
                                                               tableOutput('knn'),
                                                               h4('RMSE by Number of Neighbors'),
                                                               plotOutput('knnPlot')),
                                                     fluidPage(h3('Boosting'),
                                                               h4('Cross Validation Results'),
                                                               tableOutput('boost'),
                                                               tableOutput('boost2'),
                                                               h4('Variable Importance Plot'),
                                                               plotOutput('impPlot'))
                                                )
                                             )
                                             
                                         )),
                                tabPanel('Predictions')
                            )
                        )
                    )
                    
                    
                    
                    ))
        )
    )



