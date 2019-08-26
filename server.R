library(tidyverse)
library(leaflet)
library(shiny)
library(shinyjs)
library(ECharts2Shiny)

server <- function(input, output) {
  # create a reactive value that will store the click position
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  # Leaflet map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles('Esri.WorldImagery') %>%
      addMarkers(data = botsad.final,
                 lng = ~ X, lat = ~ Y_new,
                 popup = paste0("<img src = ", botsad.final$link_4, " />",
                                "<br> <b>", botsad.final$species, "</b> </br?> "),
                 label = botsad.final$species,
                 layerId = botsad.final$id
                 ,
                 icon = set_icons
                 ) %>%
    addEasyButton(easyButton(
      icon = "fa-crosshairs", title = "Геолокація",
      onClick = JS("function(btn, map){ map.locate({setView: true}); }")))
  })
  
  # store the click
  observeEvent(input$map_marker_click, {
    data_of_click$clickedMarker <- input$map_marker_click
  })
  
  # Make a table with ecosystem services
  output$table <- renderTable({
    if (is.null(data_of_click$clickedMarker)) {
      return(NULL)
    }
    return(
      subset(botsad.final %>%
               dplyr::select(7:12, 14), 
             id == data_of_click$clickedMarker$id
      )
    ) 
  }, na = '-', bordered = T)
  
  # Make a text with description
  output$text <- renderText({
    if (is.null(data_of_click$clickedMarker)) {
      return(NULL)
    }
    return(
      paste0(
        botsad.final[botsad.final$id == data_of_click$clickedMarker$id,]$des_3
      )
    )
  })
  
  # Make a text with species name
  output$text_name <- renderText({
    if (is.null(data_of_click$clickedMarker)) {
      return(NULL)
    }
    return(
      paste0(
        botsad.final[botsad.final$id == data_of_click$clickedMarker$id,]$species
      )
    )
  })
  
  # treemap
  renderTreeMap(div_id = "test",
                data = treemap.df,
                show.tools = F,
                name = "Усі екземпляри")
  
  # piechart
  renderPieChart(div_id = "pie",
                 data = botsad.pie,
                 theme = 'shine',
                 show.tools = F,
                 show.label = F,
                 show.legend = F,
                 radius = "85%")
}
