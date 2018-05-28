
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(dplyr)
require(leaflet)

source('dataImport.R')


shinyServer(function(input, output) { 
  
  # Set it so that a different menu bar of data options is displayed based on the
  # species of choice
  output$ui <- renderUI({
    if (is.null(input$species))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(input$species,
           'wesa' = tags$div(title = "Choose the type of data you'd like to see",
                             selectInput(inputId = 'data_type', 
                                         label = 'Data type',
                                         choices =c("Number of Counts" = 'n_counts', 
                                                    'Total number counted' = 'sum_count', 
                                                    'Average number counted' = 'mean_count',
                                                    'Proportion of Monthly Total'= 'prop_count' 
                                         ) )) ,
           'falc' = conditionalPanel(condition = 'input$species == falc',
                                     selectInput(inputId = 'data_type',
                                                 label = 'Data type',
                                                 choices = c('Probability of observation' = 'probs',
                                                             "Total Number Counted" = 'totalobserved',
                                                             "Average Number Counted" = "meanobserved")))

    )
  })
    
  output$controls <- renderUI({
    if (is.null(input$advopt)) {
        return()
    }
    
    if (input$advopt){wellPanel(
           selectInput("colors", "Color Scheme",
                choices = rownames(subset(brewer.pal.info, category %in% c("seq", "div"))),
                selected = "Reds" ),
          checkboxInput("legend", "Show legend", TRUE) )
           } else {
             return()
           }
  })



    
  output$DataPlot1 <- renderLeaflet({
    locations %>% leaflet() %>% 
    addProviderTiles("OpenStreetMap") %>% 
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude)) %>%
#     addMarkers(lng = data_$locations$longitude,
#                lat = data_$locations$latitude, 
#                popup = data_$locations$Name,
#                clusterOptions = markerClusterOptions(),
#                options = markerOptions(draggable = FALSE, riseOnHover = TRUE),
#     ) %>%  
    setView(lng = -124.251,
            lat = 49.263,
            zoom = 6)
  })



    
observe(label = 'subset data',priority = 5, {
      if (input$species == 'wesa') {
       df.out <<-
        data_[[input$species]] %>% filter(Year %in% input$yrs & Month.name %in% input$age) %>%
      mutate(total.counted = sum(max.count)) %>% filter(!is.na(max.count)) %>%
      group_by(SiteID,latitude,longitude) %>%
          dplyr::summarize(
            n_counts = n(),
            sum_count = sum(max.count, na.rm=T),
            mean_count = mean(max.count, na.rm=T) ) %>%
      ungroup %>%
      mutate(prop_count = sum_count / sum(sum_count)) %>%
        select_("latitude", "longitude", input$data_type )%>%
        rename_(output_data = input$data_type)
    }
    if (input$species == 'falc') {
      df.out <<- 
      data_[[input$species]] %>% filter(Year %in% input$yrs & Month.name %in% input$age) %>%
        group_by(SiteID, latitude, longitude) %>%
        dplyr::summarize(
          totalobserved = sum(count.falc, na.rm=T),
          meanobserved = mean(count.falc, na.rm=T),
          n.obs = sum(observed.y.n),
          n.days = n(),
          probs = (n.obs/n.days)
          ) %>%
        select_("latitude", "longitude", input$data_type ) %>%
        rename_(output_data = input$data_type)
      
    }
    


  
  
  leafletProxy('DataPlot1', data = df.out) %>% 
    #       clearPopups() %>%
    clearMarkers() %>%
    addCircleMarkers(data = df.out, lng = ~longitude,
                       lat = ~latitude,
                       color = "#777777",
                        fillOpacity = 0.7,
                      fillColor = colorNumeric('Reds', df.out$output_data, pretty = T),#"#777777",#~pal(output_data),
                       stroke = F,
#                        weight = df.out[[data_type]]+1,
                 popup = paste('Site: ', df.out[['SiteID']], '<br/>',
                               input$data_type,":" , df.out$output_data) )%>% 
    addLegend(position = "bottomleft",opacity = 1,
                        pal =  colorNumeric('Reds', df.out[["output_data"]]), values = df.out[["output_data"]]  )
              
#                  clusterOptions = markerClusterOptions(),
#                  options = markerOptions(draggable = FALSE, riseOnHover = TRUE))
    })

observe(priority = -1, {
  
  proxy <- leafletProxy('DataPlot1', data = df.out) 
  # Remove any existing legend, and only if the legend is
  # enabled, create a new one.
  proxy %>% clearControls()
  if (is.null(input$legend)) {
    do.you.want.a.legend <- TRUE
  } else{ do.you.want.a.legend <- input$legend}
  if (do.you.want.a.legend) {
    
    pal <- colorpal()
    #     print(pal)
    #     print(data_type)
    proxy %>% addLegend(position = "bottomleft",opacity = 1,
                        pal =  colorNumeric('Reds', df.out[["output_data"]]), values = df.out[["output_data"]]  )
  }            
})
})





