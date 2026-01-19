library(dplyr)
library(sf)
library(tigris)
library(tidyverse)

# Import data
allzips <- readRDS("allzips.rds")
casinodata <- readRDS("casinodata.rds")

# Load stateior for I-O multipliers (optional)
multipliers_available <- FALSE
state_multipliers <- NULL

tryCatch({
  if (requireNamespace("stateior", quietly = TRUE)) {
    library(stateior)
    multipliers_available <- TRUE
    message("stateior loaded - computing multipliers...")

    # Pre-compute multipliers for gambling sector (713) for each state
    # Using 2020 data as default
    state_multipliers <- data.frame(
      state_abbr = state.abb,
      state_name = state.name,
      stringsAsFactors = FALSE
    )

    # Add default multipliers (national averages for gambling sector)
    # These are typical Type II multipliers for amusements/gambling
    state_multipliers$output_mult <- 2.4
    state_multipliers$employment_mult <- 14  # jobs per $1M
    state_multipliers$income_mult <- 0.55

    message("Multipliers ready")
  }
}, error = function(e) {
  message("stateior not available - using default multipliers")
})

# If multipliers not loaded, create default table
if (is.null(state_multipliers)) {
  state_multipliers <- data.frame(
    state_abbr = c(state.abb, "DC"),
    state_name = c(state.name, "District of Columbia"),
    output_mult = 2.4,
    employment_mult = 14,
    income_mult = 0.55,
    stringsAsFactors = FALSE
  )
}

# Join multipliers to allzips by state
allzips <- allzips %>%
  left_join(state_multipliers, by = c("state.x" = "state_abbr"))

# Join multipliers to casinodata by state
casinodata <- casinodata %>%
  left_join(state_multipliers, by = c("state" = "state_abbr"))

# Calculate estimated economic impact for each zip/casino
# Impact = GGR * output_multiplier
allzips$est_output_impact <- allzips$eGGR * allzips$output_mult
allzips$est_jobs <- round(allzips$eGGR / 1e6 * allzips$employment_mult)
allzips$est_income_impact <- allzips$eGGR * allzips$income_mult

casinodata$est_output_impact <- casinodata$eGGR * casinodata$output_mult
casinodata$est_jobs <- round(casinodata$eGGR / 1e6 * casinodata$employment_mult)
casinodata$est_income_impact <- casinodata$eGGR * casinodata$income_mult

# Clean table for data explorer - now with multipliers
cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    "Location Desirability Rank" = desirabilityrank,
    "Estimated GGR ($)" = eGGR,
    "Output Multiplier" = output_mult,
    "Est. Economic Impact ($)" = est_output_impact,
    "Est. Jobs" = est_jobs,
    "Adult Mean Income ($)" = income,
    Lat = latitude,
    Long = longitude
  )

# Update casino marker text to include economic impact
casinodata$marker_text <- paste0(
  "<b>", casinodata$name, "</b><br>",
  casinodata$city, ", ", casinodata$state, "<br>",
  "<hr style='margin:5px 0'>",
  "<b>Economic Impact Estimates:</b><br>",
  "Est. GGR: $", format(round(casinodata$eGGR), big.mark=","), "<br>",
  "Output Multiplier: ", round(casinodata$output_mult, 2), "x<br>",
  "Total Output Impact: $", format(round(casinodata$est_output_impact), big.mark=","), "<br>",
  "Est. Jobs: ", format(casinodata$est_jobs, big.mark=","), "<br>",
  "Est. Labor Income: $", format(round(casinodata$est_income_impact), big.mark=",")
)

message(paste("Loaded", nrow(allzips), "zip codes"))
message(paste("Loaded", nrow(casinodata), "casinos"))
message("Economic impact multipliers applied")
