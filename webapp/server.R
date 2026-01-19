library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(tidyverse)
library(plotly)

# Import data
zipdata <- allzips
zipdata <- zipdata[order(zipdata$desirabilityrank),]

function(input, output, session) {

  # Reactive values for impact calculations
  impactResult <- reactiveVal(NULL)
  selectedCasinoData <- reactiveVal(NULL)

  ## Initialize dropdowns ###########################################

  # Populate casino dropdown
  observe({
    casino_choices <- setNames(
      1:nrow(casinodata),
      casinodata$name
    )
    updateSelectizeInput(session, "selectedCasino",
      choices = casino_choices,
      server = TRUE
    )
  })

  # Populate state dropdown for custom location
  observe({
    updateSelectInput(session, "customState",
      choices = getStateList()
    )
  })

  ## Interactive Map ###########################################

  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

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

  observe({
    message("Map observe triggered - rendering markers...")

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

    tryCatch({
      leafletProxy("map", data = zipdata) %>%
        clearShapes() %>%
        addPolygons(
          fillColor = ~pal(colorData),
          weight = .1,
          opacity = .5,
          color = "white",
          fillOpacity = 0.5,
          layerId = zipdata$zipcode) %>%
        addMarkers(lat=casinodata$geocodehere_lat,
                   lng = casinodata$geocodehere_lon,
                   popup = casinodata$marker_text,
                   label = ~as.character(c(casinodata$name)),
                   clusterOptions = markerClusterOptions()) %>%
        addLegend("bottomleft", pal=pal, values=colorData, title=legendtitle,
                  layerId="colorLegend")
      message("Map rendering complete")
    }, error = function(e) {
      message(paste("ERROR in map rendering:", e$message))
    })
  })

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

  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()

    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })

  # Output for conditional panel on map
  output$casinoSelected <- reactive({
    !is.null(selectedCasinoData())
  })
  outputOptions(output, "casinoSelected", suspendWhenHidden = FALSE)

  ## Impact Calculator ###########################################

  # Get casino data when selected
  observeEvent(input$selectedCasino, {
    if (!is.null(input$selectedCasino) && input$selectedCasino != "") {
      idx <- as.integer(input$selectedCasino)
      if (!is.na(idx) && idx > 0 && idx <= nrow(casinodata)) {
        casino <- casinodata[idx, ]

        # Find closest zipcode to get GGR estimate
        closest_zip <- allzips[which.min(
          (allzips$latitude - casino$geocodehere_lat)^2 +
          (allzips$longitude - casino$geocodehere_lon)^2
        ), ]

        selectedCasinoData(list(
          name = casino$name,
          lat = casino$geocodehere_lat,
          lon = casino$geocodehere_lon,
          state = closest_zip$state.x,
          state_full = getFullStateName(closest_zip$state.x),
          ggr = closest_zip$eGGR,
          ggr_ir = closest_zip$eGGR_IR
        ))
      }
    }
  })

  # Calculate impact when button clicked
  observeEvent(input$calculateImpact, {
    # Show loading indicator
    showNotification("Calculating economic impact...", type = "message", duration = 2)

    tryCatch({
      # Determine GGR and state
      if (input$locationMode == "existing") {
        casino_data <- selectedCasinoData()
        if (is.null(casino_data)) {
          showNotification("Please select a casino first", type = "error")
          return()
        }

        state <- casino_data$state_full
        ggr <- if (!is.null(input$ggrOverride) && !is.na(input$ggrOverride)) {
          input$ggrOverride * 1e6  # Convert from millions
        } else {
          casino_data$ggr
        }

      } else {
        # Custom location
        state <- input$customState
        ggr <- input$customGGR * 1e6  # Convert from millions
      }

      year <- as.integer(input$analysisYear)

      # Calculate impact
      impact <- calculateCasinoImpact(
        ggr = ggr,
        state = state,
        year = year,
        include_induced = input$includeInduced
      )

      impactResult(impact)

      showNotification("Impact calculation complete!", type = "message", duration = 2)

    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 5)
      impactResult(NULL)
    })
  })

  # Output flag for conditional panels
  output$impactCalculated <- reactive({
    !is.null(impactResult())
  })
  outputOptions(output, "impactCalculated", suspendWhenHidden = FALSE)

  # Summary outputs
  output$totalJobs <- renderText({
    impact <- impactResult()
    if (is.null(impact)) return("-")
    formatNumber(impact$jobs_total)
  })

  output$totalIncome <- renderText({
    impact <- impactResult()
    if (is.null(impact)) return("-")
    formatCurrency(impact$income_total)
  })

  output$totalOutput <- renderText({
    impact <- impactResult()
    if (is.null(impact)) return("-")
    formatCurrency(impact$output_total)
  })

  output$totalTax <- renderText({
    impact <- impactResult()
    if (is.null(impact)) return("-")
    formatCurrency(impact$tax_total)
  })

  # Impact breakdown pie chart
  output$impactPieChart <- renderPlotly({
    impact <- impactResult()
    if (is.null(impact)) return(NULL)

    breakdown <- data.frame(
      Category = c("Direct", "Indirect", "Induced"),
      Jobs = c(impact$jobs_direct, impact$jobs_indirect, impact$jobs_induced)
    )

    plot_ly(breakdown, labels = ~Category, values = ~Jobs, type = 'pie',
            textinfo = 'label+percent',
            marker = list(colors = c('#2ecc71', '#3498db', '#9b59b6'))) %>%
      layout(title = "Jobs by Impact Type",
             showlegend = TRUE)
  })

  # Detailed impact table
  output$impactTable <- renderTable({
    impact <- impactResult()
    if (is.null(impact)) return(NULL)

    data.frame(
      Category = c("Employment (Jobs)", "Labor Income", "Economic Output", "Tax Revenue"),
      Direct = c(
        formatNumber(impact$jobs_direct),
        formatCurrency(impact$income_direct),
        formatCurrency(impact$output_direct),
        formatCurrency(impact$tax_gaming)
      ),
      Indirect = c(
        formatNumber(impact$jobs_indirect),
        formatCurrency(impact$income_indirect),
        formatCurrency(impact$output_indirect),
        formatCurrency(impact$tax_income)
      ),
      Induced = c(
        formatNumber(impact$jobs_induced),
        formatCurrency(impact$income_induced),
        formatCurrency(impact$output_induced),
        formatCurrency(impact$tax_sales)
      ),
      Total = c(
        formatNumber(impact$jobs_total),
        formatCurrency(impact$income_total),
        formatCurrency(impact$output_total),
        formatCurrency(impact$tax_total)
      )
    )
  }, striped = TRUE, hover = TRUE)

  # Multiplier table
  output$multiplierTable <- renderTable({
    impact <- impactResult()
    if (is.null(impact)) return(NULL)

    mults <- impact$multipliers
    data.frame(
      Metric = c("Output Multiplier", "Employment (jobs/$M)", "Income Multiplier"),
      Value = c(
        if (!is.na(mults$output)) round(mults$output, 2) else "N/A",
        if (!is.na(mults$employment)) round(mults$employment, 1) else "N/A",
        if (!is.na(mults$income)) round(mults$income, 2) else "N/A"
      ),
      Type = rep(mults$type, 3)
    )
  }, striped = TRUE)

  # Quick impact summary for map panel
  output$quickImpactSummary <- renderText({
    casino <- selectedCasinoData()
    if (is.null(casino)) return("")

    paste0(
      "Casino: ", casino$name, "\n",
      "State: ", casino$state_full, "\n",
      "Est. GGR: ", formatCurrency(casino$ggr)
    )
  })

  # Navigate to Impact Calculator from map
  observeEvent(input$calcDetailedImpact, {
    updateNavbarPage(session, "nav", selected = "Impact Calculator")
  })

  # Download report
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste("casino_impact_report_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      impact <- impactResult()
      if (is.null(impact)) {
        write.csv(data.frame(Error = "No impact calculated"), file)
        return()
      }

      report <- data.frame(
        Metric = c(
          "State", "Year", "GGR Input",
          "Jobs - Direct", "Jobs - Indirect", "Jobs - Induced", "Jobs - Total",
          "Income - Direct", "Income - Indirect", "Income - Induced", "Income - Total",
          "Output - Direct", "Output - Indirect", "Output - Induced", "Output - Total",
          "Tax - Gaming", "Tax - Income", "Tax - Sales", "Tax - Total"
        ),
        Value = c(
          impact$state, impact$year, impact$ggr,
          impact$jobs_direct, impact$jobs_indirect, impact$jobs_induced, impact$jobs_total,
          impact$income_direct, impact$income_indirect, impact$income_induced, impact$income_total,
          impact$output_direct, impact$output_indirect, impact$output_induced, impact$output_total,
          impact$tax_gaming, impact$tax_income, impact$tax_sales, impact$tax_total
        )
      )

      write.csv(report, file, row.names = FALSE)
    }
  )

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
