library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(tidyverse)

# Import data
zipdata <- allzips

zipdata <- zipdata[order(zipdata$desirabilityrank),]

function(input, output, session) {

  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen
  observe(
    {
    colorBy <- input$color
    sizeBy <- input$size

    if (colorBy == "desirabilityrank") {
      colorData <- zipdata[[colorBy]]
      bins <- c(0,5000,10000,15000,20000,25000,30000,35000)
      pal <- colorBin("YlOrRd", bins = bins, zipdata$desirabilityrank)
      legendtitle <- "Site Rank"
    } else {
      colorData <- zipdata[[colorBy]]
      bins <- c(0,20000,40000,60000,80000,100000,150000,250000,Inf)
      pal <- colorBin("YlOrRd", bins = bins, domain = zipdata$income)
      legendtitle <- "Adult Mean Income"
    }

  #Set up colors for polygons of zipcodes
  #bins <- c(0,20000,40000,60000,80000,100000,150000,250000,Inf)
  #pal <- colorBin("YlOrRd", domain = allzips$income, bins = bins)

  leafletProxy("map", data = zipdata) %>%
    clearShapes()   %>%
    addPolygons(
      fillColor = ~pal(colorData),
      #group = "poly",
      weight = .1,
      opacity = .5,
      color = "white",
      fillOpacity = 0.5,
      layerId = zipdata$zipcode) %>%

      #addLegend(title=colorBy,pal = pal, values = bins, group = "poly", position = "bottomleft") %>%
    addMarkers(lat=casinodata$geocodehere_lat,
               lng = casinodata$geocodehere_lon,
               popup = casinodata$marker_text,
               label = ~as.character(c(casinodata$name)),
               clusterOptions = markerClusterOptions()) %>%

    addLegend("bottomleft", pal=pal, values=colorData, title=legendtitle,
              layerId="colorLegend")
  })

  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- allzips[allzips$zipcode == zipcode,]
    content <- as.character(tagList(
      tags$h4("Location Desirability Rank:", as.integer(selectedZip$desirabilityrank)),
      tags$strong(HTML(sprintf("%s, %s %s",
                               selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
      ))), tags$br(),
      sprintf("Closest venue: %s", selectedZip$name), tags$br(),
      sprintf("Zipcode mean income (21+): %s", dollar(selectedZip$income)), tags$br(),
      sprintf("Zipcode population (21+): %s", as.integer(selectedZip$adultpop)), tags$br(),
      sprintf("GGR estimate (non-IR): %s", dollar(selectedZip$eGGR)), tags$br(),
      sprintf("GGR estimate (IR): %s", dollar(selectedZip$eGGR_IR)), tags$br(),
      sprintf("Catchment area geo-demand index: %s", (round(selectedZip$desirabilityindex, digits=2)))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }

  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })

  ## Data Explorer ###########################################

  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectizeInput(session, "cities", choices = cities,
                         selected = stillSelected, server = TRUE)
  })

  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectizeInput(session, "zipcodes", choices = zipcodes,
                         selected = stillSelected, server = TRUE)
  })

  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })

  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        `Location Desirability Rank` >= input$minScore,
        `Location Desirability Rank` <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")

    DT::datatable(df, options = list(pageLength = 10, width="100%", scrollX = TRUE), escape = FALSE)
  })
}
