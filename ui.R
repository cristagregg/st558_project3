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

combined <- read_csv('combined.csv')[,-1] 
cols <- names(combined)[4:9]

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = 'Wildfire dashboard'),
    dashboardSidebar(
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
                        
                        # Application title
                        titlePanel("Historical Wildfire Data"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
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
            tabItem(tabName = 'Modeling')
                    
                    
                    
                    ))
        )
    )



