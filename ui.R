
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(RColorBrewer)
library(shiny)
library(leaflet)
require(dplyr)
require(ggplot2)
# The goal of this project is to create a map of the study region where you can zoom in and see counts
# The counts can vary based on a box you select.

# Input output with 3 choices: Species, Year and summary type
shinyUI(bootstrapPage(
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = 'species',
                   label = "Species to explore",
                   choices = c('Western Sandpipers' = 'wesa', 'Falcons'= 'falc'),
                   selected = 'wesa'),
    
      checkboxGroupInput(inputId = 'yrs', 
                         label = 'Years',
                         choices = c('2013','2014','2015'),
                         selected = c('2013','2014','2015')),
      
      checkboxGroupInput(inputId = 'age', 
                         label = 'Month',
                         choices = c('July','August'),
                         selected = 'July'),
      
      # This outputs the dynamic UI component
      wellPanel(uiOutput("ui")),
      
      checkboxInput(inputId = 'advopt', label = "Advanced Options", value = TRUE),
      
      uiOutput('controls')
    ),
      
      

                                
      mainPanel(
    leafletOutput('DataPlot1') )
)



  
  )
)

