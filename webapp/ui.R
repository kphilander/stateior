library(leaflet)

# Choices for drop-downs
vars <- c(
  "Site rank" = "desirabilityrank",
  "Adult (21+) mean income" = "income"
)

navbarPage("Casino Geo-Demand Model [Beta] by Kahlil Philander", id="nav",
  tabPanel("Interactive map",
    div(class="outer",
      tags$head(
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h4("Explorer"),
        selectInput("color", "Color", vars)
      ),

      tags$div(id="cite",
        'Copyright Kahlil Philander (2022). ',
        tags$em('Casino locations from worldcasinodirectory.com, HERE API, and author.'),
        tags$br(),
        'Figures based on data from US Census, BEA, and author calculations.'
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    fluidRow(
      column(1,
        numericInput("minScore", "Min score", min=0, max=99999, value=0)
      ),
      column(1,
        numericInput("maxScore", "Max score", min=0, max=99999, value=99999)
      )
    ),
    hr(),
    DT::dataTableOutput("ziptable")
  )
)
