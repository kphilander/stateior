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
      sprintf("Catchment area geo-demand index: %s", round(selectedZip$desirabilityindex, digits=2)),
      tags$hr(),
      tags$strong("Economic Impact (I-O Multipliers):"), tags$br(),
      sprintf("Output Multiplier: %sx", round(selectedZip$output_mult, 2)), tags$br(),
      sprintf("Est. Total Output: %s", dollar(selectedZip$est_total_output)), tags$br(),
      sprintf("Est. Jobs: %s", format(selectedZip$est_jobs, big.mark=",")), tags$br(),
      sprintf("Est. Labor Income: %s", dollar(selectedZip$est_labor_income))
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
        `Desirability Rank` >= input$minScore,
        `Desirability Rank` <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-map-marker"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")

    DT::datatable(df, options = list(pageLength = 10, width="100%", scrollX = TRUE), escape = FALSE)
  })

  ## Impact Calculator ###########################################

  # Store calculated results
  calc_results <- reactiveValues(
    output = NULL,
    jobs = NULL,
    income = NULL,
    multipliers = NULL
  )

  # Calculate impact when button is clicked
  observeEvent(input$calculate, {
    req(input$calc_state, input$calc_ggr)

    # Get multipliers for selected state
    state_mult <- state_multipliers[state_multipliers$state_abbr == input$calc_state, ]

    if (nrow(state_mult) == 0) {
      # Use default multipliers if state not found
      state_mult <- data.frame(
        state_abbr = input$calc_state,
        output_mult = 2.4,
        emp_mult = 14,
        income_mult = 0.55
      )
    }

    # Calculate impacts (GGR is in millions)
    ggr_dollars <- input$calc_ggr * 1e6

    calc_results$output <- ggr_dollars * state_mult$output_mult
    calc_results$jobs <- round(input$calc_ggr * state_mult$emp_mult)
    calc_results$income <- ggr_dollars * state_mult$income_mult
    calc_results$multipliers <- state_mult
  })

  # Render output results
  output$result_output <- renderText({
    if (is.null(calc_results$output)) {
      "--"
    } else {
      paste0("$", format(round(calc_results$output / 1e6, 1), big.mark = ","), "M")
    }
  })

  output$result_jobs <- renderText({
    if (is.null(calc_results$jobs)) {
      "--"
    } else {
      format(calc_results$jobs, big.mark = ",")
    }
  })

  output$result_income <- renderText({
    if (is.null(calc_results$income)) {
      "--"
    } else {
      paste0("$", format(round(calc_results$income / 1e6, 1), big.mark = ","), "M")
    }
  })

  # Render multiplier table
  output$multiplier_table <- renderTable({
    if (is.null(calc_results$multipliers)) {
      data.frame(
        Multiplier = c("Output", "Employment", "Income"),
        Value = c("--", "--", "--"),
        Description = c(
          "Total economic output per $1 GGR",
          "Jobs per $1M GGR",
          "Labor income per $1 GGR"
        )
      )
    } else {
      data.frame(
        Multiplier = c("Output", "Employment", "Income"),
        Value = c(
          paste0(round(calc_results$multipliers$output_mult, 2), "x"),
          paste0(calc_results$multipliers$emp_mult, " jobs/$M"),
          paste0(round(calc_results$multipliers$income_mult * 100, 0), "%")
        ),
        Description = c(
          "Total economic output per $1 GGR",
          "Jobs per $1M GGR",
          "Labor income as % of GGR"
        )
      )
    }
  })
}
