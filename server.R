
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
           'wesa' = #tags$div(title = "Choose the type of data you'd like to see",
                             selectInput(inputId = 'data_type',
                                         label = 'Data type',
                                         choices =WESAchoices,
                                         selected = 'n_counts'
                                         ),#) ,
           'falc' = conditionalPanel(condition = 'input$species == falc',
                                     selectInput(inputId = 'data_type',
                                                 label = 'Data type',
                                                 choices = FALCchoices,
                                                 selected = 'probs'))
           
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
      checkboxInput("legend", "Show legend", TRUE),
      selectInput("maptypes", "Background map",
                  choices = c("MapQuestOpen.Aerial",
                    "Stamen.TerrainBackground",
                    "Esri.WorldImagery",
                    "OpenStreetMap",
                    "Stamen.Watercolor"),
                  selected = "OpenStreetMap"))
    } else {
      return()
    }
  })
  
  
  
  
  output$DataPlot1 <- renderLeaflet({
    locations %>% leaflet() %>% 
      addProviderTiles(input$maptypes) %>% 
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
  

species_data <- reactive({data_[[input$species]]})

  
#   
#   observe(label = 'subset data',priority = 5, {
  df.out <- reactive({
    if (is.null(input$species) | is.null(input$data_type)) {data_i <- data_[[input$species]];data_i }
    else{     data_type <- input$data_type
      if(input$species == 'wesa') {
      data_i <- 
        species_data() %>% filter(Year %in% input$yrs & Month.name %in% input$age) %>%
        mutate(total.counted = sum(max.count)) %>% filter(!is.na(max.count)) %>%
        group_by(SiteID,latitude,longitude) %>%
        dplyr::summarize(
          n_counts = n(),
          sum_count = sum(max.count, na.rm=T),
          mean_count = round(mean(max.count, na.rm=T),2) ) %>%
        ungroup %>%
        mutate(prop_count = round(sum_count / sum(sum_count)* 100,2)) %>%
#         select_("SiteID", "latitude", "longitude", input$data_type )%>%
        mutate_("output_data" = paste0(data_type)) }  else{

    if (input$species == 'falc') { 
      data_i <- 
        species_data() %>% filter(Year %in% input$yrs & Month.name %in% input$age) %>%
          group_by(SiteID, latitude, longitude) %>%
          dplyr::summarize(
            totalobserved = round(sum(count.falc, na.rm=T), 2),
            meanobserved = round(mean(count.falc, na.rm=T),2),
            n.obs = sum(observed.y.n, na.rm=T),
            n.days = n(),
            probs = round(n.obs/n.days,3)
          ) %>%
          select_("latitude", "longitude", input$data_type ) %>%
          mutate_("output_data" = input$data_type)
        
    }}}
  data_i


  })


# This reactive expression represents the palette function,
# which changes as the user makes selections in UI.
colorpal <- reactive({
  if (is.null(input$species) | is.null(input$data_type) | is.null(input$advopt)) {colorNumeric('Reds', wesa$max.count)} else{
  if(input$advopt){
    new.df <- df.out()
    colorBin(input$colors, new.df$output_data,bins = 5)
  }else{ colorNumeric(rownames(subset(brewer.pal.info, category %in% c("seq", "div")))[23], df.out())}
  
}})

# observeEvent(input$data_type,{
#   print(input$data_type)
# #   print(df.out())
# })
#     
observeEvent({list( input$yrs, input$age, input$data_type , input$maptypes, input$colors)},{
      if(!is.null(input$data_type)){
        species_choices <- switch(input$species,
                                            'wesa' = WESAchoices,
                                            'falc' = FALCchoices)
        data_label <- names(species_choices[species_choices==input$data_type])
#     if (input$species == 'wesa') {
    df.new <- df.out()
#     print(isolate(df.new))
    # sites <- c("GRIC", "NJEN", "SJEN")
    # print(isolate(filter(df.new, SiteID %in% sites)))
#     } else{df.new <- df.out.falc()}
    pal <- colorpal()
    leafletProxy('DataPlot1', data = df.new) %>% 
    clearTiles() %>%
  addProviderTiles(input$maptypes) %>%
            clearPopups() %>%
      clearMarkers() %>% clearShapes() %>%
      addCircleMarkers(data = df.new, lng = ~longitude,
                       lat = ~latitude,
                       color = "#777777",
                       fillOpacity = 0.7,
                       fillColor = ~pal(output_data),
                       stroke = F,
                       #                        weight = df.out[[data_type]]+1,
                       popup = paste('Site: ', as.character(df.new$SiteID), '<br/>',
                                     isolate(data_label),":" , df.new$output_data) )
    
    #                  clusterOptions = markerClusterOptions(),
    #                  options = markerOptions(draggable = FALSE, riseOnHover = TRUE))
      }
  })
# # 
observeEvent(list( input$yrs, input$age, input$data_type , input$maptypes, input$colors), {
  if(!is.null(input$data_type)){
#   if (input$species == 'wesa') {
    df.new <- df.out()
#   } else{df.new <- df.out.falc()}
  proxy <- leafletProxy('DataPlot1', data = df.new) 
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
    proxy %>% addLegend(position = "topright",opacity = .7,
                        pal = pal, values = df.new[["output_data"]] )
  } }           
})

# observe({
# data_clear <- df.out()
# if (nrow(data_clear)) {  leafletProxy('DataPlot1') %>% clearControls() %>% clearMarkers()}
# })

})




