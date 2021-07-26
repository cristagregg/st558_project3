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
library(DT)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    dashboardHeader(title = 'Wildfire dashboard'),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About", tabName = "About", icon = icon("About")),
            menuItem("Data", tabName = "Data", icon = icon("Data")),
            menuItem("Exploration", tabName = "Exploration", icon = icon("Exploration")),
            menuItem("Modelling", tabName = "Modelling", icon = icon("Modelling"))
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
                                p('The purpose of this project is to raise awareness on the impact of wildfires and understand their causes. We will also investigate how weather patterns affect the number and severity of wildfires, and create a model to predict wildfires. We will use data provided by the US Department of Agriculture which provides information on wildfires from 1992 to 2018. We will also use a climate dataset provided by the Climate Data Store by Copernicus. You can find more information about these datasets via these links:'),
                                a(href='https://www.fs.usda.gov/rds/archive/Catalog/RDS-2013-0009.5', 'Wildfire data'),
                                br(),
                                a(href='https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-single-levels-monthly-means?tab=overview', 'Climate data'),
                                br(), br(),
                                h4('About the App'),
                                p('In the Data tab, you will be able to investigate the raw data. The Data Exploration tab will allow you to make some charts and summaries. Finally, the modelling page will allow you to investigate different models used for prediction.')
                            )
                        )
                    
                    )),
            
            #Second tab - data
            tabItem(tabName = 'Data',
                    fluidPage(
                        titlePanel("Data"),
                        
                        sidebarLayout(
                            sidebarPanel(
                              selectInput('dataType',
                                          label = 'Select the dataset you wish to view',
                                          choices = list('Fires Data', 'Climate Data')),
                              downloadButton('download', "Download data"),
                              #dynamic UI 1
                              conditionalPanel(condition = 'input.dataType == "Fires Data"', 
                              selectInput('yearFilter', 'Filter by Year', choices = list('No Filter', Year = 1992:2018)))                
                            ),
                            
                            mainPanel(
                              dataTableOutput('Data')
                              )
                        )
                    )),
            tabItem(tabName = 'Exploration',
                    fluidPage(
                        
                        # Application title
                        titlePanel("Old Faithful Geyser Data"),
                        
                        # Sidebar with a slider input for number of bins 
                        sidebarLayout(
                            sidebarPanel(
                                sliderInput("bins",
                                            "Number of bins:",
                                            min = 1,
                                            max = 50,
                                            value = 30)
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                                plotOutput("distPlot")
                            )
                        )
                    )),
            tabItem(tabName = 'Modelling')
                    
                    
                    
                    ))
        )
    )



