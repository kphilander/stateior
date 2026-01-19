library(leaflet)
library(plotly)

# Choices for drop-downs
vars <- c(
  "Site rank" = "desirabilityrank",
  "Adult (21+) mean income" = "income"
)

navbarPage("Casino Economic Impact Model", id="nav",

  # Tab 1: Interactive Map
  tabPanel("Interactive map",
    div(class="outer",
      tags$head(
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      # Map control panel
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h4("Explorer"),
        selectInput("color", "Color", vars),

        hr(),

        # Impact Quick View (appears when casino selected)
        conditionalPanel(
          condition = "output.casinoSelected",
          h4("Quick Impact View"),
          verbatimTextOutput("quickImpactSummary"),
          actionButton("calcDetailedImpact", "Full Analysis", class = "btn-primary btn-sm")
        )
      ),

      tags$div(id="cite",
        'Casino Economic Impact Model - Based on stateior I-O tables. ',
        tags$br(),
        'Casino locations from worldcasinodirectory.com. Data from US Census, BEA.'
      )
    )
  ),

  # Tab 2: Impact Calculator (NEW)
  tabPanel("Impact Calculator",
    fluidPage(
      titlePanel("Economic Impact Calculator"),

      fluidRow(
        # Left column: Inputs
        column(4,
          wellPanel(
            h4("Step 1: Select Location"),

            radioButtons("locationMode", "Location Type:",
              choices = c("Existing Casino" = "existing", "Custom Location" = "custom"),
              selected = "existing"
            ),

            conditionalPanel(
              condition = "input.locationMode == 'existing'",
              selectizeInput("selectedCasino", "Select Casino:",
                choices = NULL,
                options = list(placeholder = "Search casinos...")
              )
            ),

            conditionalPanel(
              condition = "input.locationMode == 'custom'",
              selectInput("customState", "State:", choices = NULL),
              numericInput("customGGR", "Estimated Annual GGR ($M):", value = 100, min = 1, max = 10000)
            ),

            hr(),

            h4("Step 2: Parameters"),

            selectInput("analysisYear", "Analysis Year:",
              choices = 2012:2024,
              selected = 2020
            ),

            numericInput("ggrOverride", "GGR Override ($M):",
              value = NULL,
              min = 1, max = 10000,
              step = 10
            ),
            helpText("Leave blank to use estimated GGR"),

            checkboxInput("includeInduced", "Include Induced Effects (Type II)", value = TRUE),

            hr(),

            actionButton("calculateImpact", "Calculate Impact", class = "btn-primary btn-lg btn-block"),

            hr(),

            downloadButton("downloadReport", "Download Report (CSV)")
          )
        ),

        # Right column: Results
        column(8,
          conditionalPanel(
            condition = "output.impactCalculated",

            fluidRow(
              # Summary boxes
              column(3, div(class = "impact-box",
                h5("Total Jobs"),
                h2(textOutput("totalJobs", inline = TRUE)),
                p("Direct + Indirect + Induced")
              )),
              column(3, div(class = "impact-box",
                h5("Labor Income"),
                h2(textOutput("totalIncome", inline = TRUE)),
                p("Wages & Benefits")
              )),
              column(3, div(class = "impact-box",
                h5("Economic Output"),
                h2(textOutput("totalOutput", inline = TRUE)),
                p("Total Economic Activity")
              )),
              column(3, div(class = "impact-box",
                h5("Tax Revenue"),
                h2(textOutput("totalTax", inline = TRUE)),
                p("Gaming + Income + Sales")
              ))
            ),

            hr(),

            fluidRow(
              column(6,
                h4("Impact Breakdown"),
                plotlyOutput("impactPieChart", height = "300px")
              ),
              column(6,
                h4("Detailed Results"),
                tableOutput("impactTable")
              )
            ),

            hr(),

            fluidRow(
              column(12,
                h4("Multipliers Used"),
                tableOutput("multiplierTable"),
                p(class = "text-muted",
                  "Multipliers calculated from stateior two-region I-O tables. ",
                  "Type I = Direct + Indirect. Type II = Type I + Induced."
                )
              )
            )
          ),

          # Placeholder when no calculation
          conditionalPanel(
            condition = "!output.impactCalculated",
            div(class = "text-center", style = "padding: 100px;",
              icon("calculator", class = "fa-5x text-muted"),
              h4(class = "text-muted", "Select a casino and click 'Calculate Impact' to see results")
            )
          )
        )
      )
    )
  ),

  # Tab 3: Data Explorer (existing)
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
  ),

  # Tab 4: About
  tabPanel("About",
    fluidPage(
      h3("Casino Economic Impact Model"),
      p("This application combines casino location and demand data with state-level input-output economic models to estimate the economic impact of casino operations."),

      h4("Data Sources"),
      tags$ul(
        tags$li("Casino locations: worldcasinodirectory.com, HERE API"),
        tags$li("Economic I-O tables: stateior R package (US EPA)"),
        tags$li("Demographic data: US Census Bureau"),
        tags$li("Economic data: Bureau of Economic Analysis (BEA)")
      ),

      h4("Methodology"),
      p("Economic impacts are calculated using the Leontief input-output model:"),
      tags$ul(
        tags$li(strong("Direct effects:"), " Jobs and income at the casino itself"),
        tags$li(strong("Indirect effects:"), " Supply chain impacts (vendors, suppliers)"),
        tags$li(strong("Induced effects:"), " Household spending from employee wages")
      ),

      h4("Limitations"),
      tags$ul(
        tags$li("GGR estimates are modeled, not actual reported figures"),
        tags$li("I-O multipliers assume fixed technology and prices"),
        tags$li("Tax rates are approximate and may not reflect current law"),
        tags$li("Cross-state effects use simplified two-region model")
      ),

      hr(),
      p("Contact: kphilander@gamblingpolicy.com"),
      p("Based on stateior package by US EPA: https://github.com/USEPA/stateior")
    )
  ),

  conditionalPanel("false", icon("crosshair"))
)
