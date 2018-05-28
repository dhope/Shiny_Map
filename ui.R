
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
                         choices = c('2013','2014','2015', '2016', '2017'),
                         selected = c('2013','2014','2015', '2016', '2017')),
      
      checkboxGroupInput(inputId = 'age', 
                         label = 'Month',
                         choices = c('July','August'),
                         selected = 'July'),
      
      
      
      checkboxInput(inputId = 'advopt', label = "Advanced Options", value = TRUE),
      # This outputs the dynamic UI component
      wellPanel(uiOutput("ui")),
#       conditionalPanel(condition = "input.species == 'wesa'",
#                        selectInput(inputId = 'data_type',
#                                                                             label = 'Data type',
#                                                                             choices =c("Number of Counts" = 'n_counts', 
#                                                                                        'Total number counted' = 'sum_count', 
#                                                                                        'Average number counted' = 'mean_count',
#                                                                                        'Proportion of Monthly Total'= 'prop_count' 
#                                                                             ),
#                                                                             selected = 'n_counts'
#                                                                             )),
#       conditionalPanel(condition = "input.species == 'falc'",
#                                              selectInput(inputId = 'data_type',
#                                                 label = 'Data type',
#                                                 choices = c('Probability of observation' = 'probs',
#                                                             "Total Number Counted" = 'totalobserved',
#                                                             "Average Number Counted" = "meanobserved"),
#                                                 selected = 'probs')),
      
      uiOutput('controls')
    ),
      
      

                                
      mainPanel(width=7,
    leafletOutput('DataPlot1', height = 750 ) )
#     dataTableOutput('df.out') )
)



  
  )
)

