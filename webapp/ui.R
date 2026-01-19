library(leaflet)

# Choices for drop-downs
vars <- c(
  "Site rank" = "desirabilityrank",
  "Adult (21+) mean income" = "income"
)

# State choices for calculator
state_choices <- c("Select state" = "", structure(state.abb, names=state.name))

navbarPage("Casino Economic Impact Model", id="nav",
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
        'Casino Economic Impact Model. ',
        tags$em('Casino locations from worldcasinodirectory.com.'),
        tags$br(),
        'Economic multipliers from stateior I-O tables.'
      )
    )
  ),

  tabPanel("Impact Calculator",
    fluidPage(
      titlePanel("Economic Impact Calculator"),

      fluidRow(
        column(4,
          wellPanel(
            h4("Input Parameters"),
            selectInput("calc_state", "State:", choices = state_choices),
            numericInput("calc_ggr", "Gross Gaming Revenue ($M):", value = 100, min = 1, max = 10000),
            hr(),
            actionButton("calculate", "Calculate Impact", class = "btn-primary btn-lg"),
            hr(),
            p(class = "text-muted", "Enter the state and estimated annual GGR to calculate economic impacts using I-O multipliers.")
          )
        ),

        column(8,
          h4("Economic Impact Results"),

          fluidRow(
            column(4,
              div(class = "well text-center",
                h5("Total Economic Output"),
                h2(textOutput("result_output")),
                p("Direct + Indirect + Induced")
              )
            ),
            column(4,
              div(class = "well text-center",
                h5("Jobs Supported"),
                h2(textOutput("result_jobs")),
                p("Full-time equivalent")
              )
            ),
            column(4,
              div(class = "well text-center",
                h5("Labor Income"),
                h2(textOutput("result_income")),
                p("Wages and benefits")
              )
            )
          ),

          hr(),

          h4("Multipliers Used"),
          tableOutput("multiplier_table"),

          p(class = "text-muted",
            "Multipliers are Type II (includes induced effects) for the Amusements, Gambling, and Recreation sector (BEA code 713). ",
            "Source: Derived from stateior state-level I-O tables."
          )
        )
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
