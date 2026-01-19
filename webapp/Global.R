library(dplyr)
library(sf)
library(tigris)
library(tidyverse)

# Import data
allzips <- readRDS("allzips.rds")
casinodata <- readRDS("casinodata.rds")

# Fix UTF-8 encoding issues (required for WebSocket communication)
fix_encoding <- function(x) {
  if (is.character(x)) {
    iconv(x, from = "", to = "UTF-8", sub = "")
  } else {
    x
  }
}

# Apply encoding fix to all character columns
allzips <- allzips %>% mutate(across(where(is.character), fix_encoding))
casinodata <- casinodata %>% mutate(across(where(is.character), fix_encoding))

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
