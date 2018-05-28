
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
require(dplyr)
require(leaflet)


## Import required data and add to list
locations <- read.csv('./.data/sitelocations_decdegree.csv', header = T, stringsAsFactors=F) %>%
  filter(SITEID != "") %>% mutate(longitude = as.character(lon),
                                  latitude = as.character(lat)) %>% rename(SiteID = SITEID)

wesa       <- read.csv('./.data/wesa.clean.csv') %>%
  filter(RecordID != 'RecordID' & !is.na(counts.cleaned)) %>%
  select(RecordID, ID, SiteID, counts.cleaned ) %>%
  group_by(RecordID) %>% 
  dplyr::summarize(max.count = max(counts.cleaned, na.rm=T), 
                   avg.count = mean(counts.cleaned, na.rm=T),
                   sd.count = sd(counts.cleaned, na.rm = T),
                   sum.count = sum(counts.cleaned, na.rm = T), 
                   n.counts = n() )%>%
  mutate(SiteID = substr(RecordID, 1, 4), 
         Year = substr(RecordID, 5, 8), 
         Month = substr(RecordID, 9,9),
         Day = substr(RecordID, 10, 11),
         Month.name = ifelse(Month =='7', 'July', 'August')) %>%
  left_join(locations, by = 'SiteID')


all.surveys.info <- wesa %>% select(RecordID, SiteID, Year, Month, Day) %>%
  mutate (  
    Year = substr(RecordID, 5, 8), 
    Month = substr(RecordID, 9,9),
    Day = substr(RecordID, 10, 11),
    Date = as.Date(paste(Year, "-" , Month, "-", Day,sep =""), format = "%Y-%m-%d"))


falc    <- read.csv( './.data/falc.clean.csv') %>%
  select(RecordID, SiteID, ID, Count) %>%
    mutate (  
      Year = substr(RecordID, 5, 8), 
      Month = substr(RecordID, 9,9),
      Day = substr(RecordID, 10, 11),
      Date = as.Date(paste(Year, "-" , Month, "-", Day,sep =""), format = "%Y-%m-%d")) %>%
    group_by(RecordID, SiteID, Date, Year, Month) %>%
    dplyr::summarize(
      TotalCounts = n(),
      numberCounted = mean(Count, na.rm=T),
      observed = 1
    ) %>%
    right_join(all.surveys.info, by=c('RecordID', 'SiteID', 'Date', 'Year', 'Month') ) %>%
    dplyr::mutate(observed.y.n = ifelse(is.na(observed), 0, 1),
                  count.falc = ifelse(is.na(numberCounted), 0, numberCounted),
                  Month.name = ifelse(Month =='7', 'July', 'August') ) %>%
  left_join(locations, by = 'SiteID')




data_ <- list(falc= falc,wesa= wesa,locations= locations)

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


    
#     data_type <<- input$data_type
#     age <- input$age
#     years <- input$yrs
#     species <- input$species
#     df <- data_[[species]] %>% filter(Year %in% years & Month.name %in% age) 
    
observe(label = 'subset data',priority = 5, {
    data_type <- input$data_type
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
        select_("latitude", "longitude", data_type ) %>%
        rename_(output_data = data_type)
      
    }
    
    
    # This reactive expression represents the palette function,
    # which changes as the user makes selections in UI.
    colorpal <<- reactive({
      # #             if(input$advopt){
      colorNumeric(input$colors, df.out[["output_data"]])
      #     }else{ colorNumeric(rownames(subset(brewer.pal.info, category %in% c("seq", "div")))[23], df.out[[data_type]])}
      
    })
    
    
    })
  



    require(htmltools)
observe(label = 'addmarkers', priority = 0,{  
  #print(df.out)
  
  pal <- colorpal()
  
  leafletProxy('DataPlot1', data = df.out) %>% 
    #       clearPopups() %>%
    clearMarkers() %>%
    addCircleMarkers(data = df.out, lng = ~longitude,
                       lat = ~latitude,
                       color = "#777777",
                        fillOpacity = 0.7,
                      fillColor = ~pal(output_data),
                       stroke = F,
#                        weight = df.out[[data_type]]+1,
                 popup = paste('Site: ', df.out[['SiteID']], '<br/>',
                               input$data_type,":" , df.out$output_data) )
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







