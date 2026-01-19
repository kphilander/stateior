library(dplyr)
library(sf)
library(tigris)
library(tidyverse)

# Import casino data FIRST (before any modules that might interfere)
allzips <- readRDS("allzips.rds")
casinodata <- readRDS("casinodata.rds")

# Debug: verify data loaded correctly
message(paste("Loaded", nrow(allzips), "zip codes"))
message(paste("Loaded", nrow(casinodata), "casinos"))
message(paste("Casino columns:", paste(names(casinodata), collapse=", ")))
message(paste("Valid casino lat/lon:", sum(!is.na(casinodata$geocodehere_lat) & !is.na(casinodata$geocodehere_lon))))

# Clean table for data explorer (create BEFORE loading Matrix which can mask tidyr functions)
cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    "Location Desirability Rank" = desirabilityrank,
    "Estimated non-IR GGR ($)" = eGGR,
    "Estimated IR GGR ($)" = eGGR_IR,
    "Adults per Sq. Mile" = pop_density,
    "Adult Mean Income ($)" = income,
    Lat = latitude,
    Long = longitude
  )

# State name mapping (abbreviation to full name)
state_mapping <- c(
  AL = "Alabama", AK = "Alaska", AZ = "Arizona", AR = "Arkansas", CA = "California",
  CO = "Colorado", CT = "Connecticut", DE = "Delaware", DC = "District of Columbia",
  FL = "Florida", GA = "Georgia", HI = "Hawaii", ID = "Idaho", IL = "Illinois",
  IN = "Indiana", IA = "Iowa", KS = "Kansas", KY = "Kentucky", LA = "Louisiana",
  ME = "Maine", MD = "Maryland", MA = "Massachusetts", MI = "Michigan", MN = "Minnesota",
  MS = "Mississippi", MO = "Missouri", MT = "Montana", NE = "Nebraska", NV = "Nevada",
  NH = "New Hampshire", NJ = "New Jersey", NM = "New Mexico", NY = "New York",
  NC = "North Carolina", ND = "North Dakota", OH = "Ohio", OK = "Oklahoma", OR = "Oregon",
  PA = "Pennsylvania", RI = "Rhode Island", SC = "South Carolina", SD = "South Dakota",
  TN = "Tennessee", TX = "Texas", UT = "Utah", VT = "Vermont", VA = "Virginia",
  WA = "Washington", WV = "West Virginia", WI = "Wisconsin", WY = "Wyoming"
)

# Get full state name from abbreviation
getFullStateName <- function(abbrev) {
  if (abbrev %in% names(state_mapping)) {
    return(state_mapping[abbrev])
  }
  return(abbrev)
}

# Get list of states (for dropdown)
getStateList <- function() {
  return(as.character(state_mapping))
}

# Available years for I-O analysis
available_years <- 2012:2024

# Default analysis year
default_year <- 2020

# Now load stateior and modules (AFTER data is loaded to avoid conflicts)
stateior_available <- FALSE
tryCatch({
  if (requireNamespace("stateior", quietly = TRUE)) {
    library(stateior)
    stateior_available <- TRUE
    message("stateior loaded successfully")
  }
}, error = function(e) {
  message("stateior not available - using default multipliers")
})

# Source economic impact modules
module_files <- c("R/io_data_loader.R", "R/leontief_engine.R",
                  "R/multiplier_calc.R", "R/impact_analysis.R", "R/tax_estimator.R")
for (f in module_files) {
  if (file.exists(f)) {
    tryCatch({
      source(f)
    }, error = function(e) {
      message(paste("Could not load module:", f))
    })
  }
}
