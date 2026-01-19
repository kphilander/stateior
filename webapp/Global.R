library(dplyr)
library(sf)
library(tigris)
library(tidyverse)

# Load stateior for I-O analysis (install if needed)
if (!requireNamespace("stateior", quietly = TRUE)) {
  message("Installing stateior package...")
  remotes::install_github("USEPA/stateior")
}
library(stateior)

# Source economic impact modules
source("R/io_data_loader.R")
source("R/leontief_engine.R")
source("R/multiplier_calc.R")
source("R/impact_analysis.R")
source("R/tax_estimator.R")

# Import casino data
allzips <- readRDS("allzips.rds")
casinodata <- readRDS("casinodata.rds")

# Clean table for data explorer
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

# Available years for I-O analysis
available_years <- 2012:2024

# Default analysis year
default_year <- 2020
