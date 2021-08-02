# st558_project3
This project creates a Shiny app to analyze wildfire and climate data. You will be able to view and download raw data, view plots, and fit predictive models.

To run this dashboard from your computer, enter the following code into your R Studio:

#install and load required packages  
install.packages(c('shiny', 'shinydashboard', 'tidyverse', 'DT', 'plotly', 'htmlwidgets', 'caret', 'gbm', 'rpart.plot'))  
library(shiny)  
library(shinydashboard)  
library(tidyverse)  
library(DT)  
library(plotly)  
library(htmlwidgets)  
library(caret)  
library(gbm)  
library(rpart.plot)  

#run app  
shiny::runGitHub('st558_project3', 'cristagregg', ref = 'main')
