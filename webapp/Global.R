library(dplyr)
library(sf)
library(tigris)
library(tidyverse)

#Import data
allzips <- readRDS("allzips.rds")
casinodata <- readRDS("casinodata.rds")
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





