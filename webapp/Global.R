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

# Load pre-computed Type II multipliers for gambling sector (BEA 713)
# Source: Derived from stateior I-O tables using Leontief analysis
# Multipliers based on BEA RIMS II methodology for sector 713 (Amusements, gambling, and recreation)
state_multipliers <- tryCatch({
  # Try RDS first (faster, includes metadata)
  if (file.exists("data/state_multipliers.rds")) {
    mults <- readRDS("data/state_multipliers.rds")
    message(paste("Loaded multipliers from RDS, computed:", attr(mults, "computed_date")))
    mults
  } else if (file.exists("data/state_multipliers.csv")) {
    # Fall back to CSV
    mults <- read.csv("data/state_multipliers.csv", stringsAsFactors = FALSE)
    message("Loaded multipliers from CSV")
    mults
  } else {
    stop("No multiplier data file found")
  }
}, error = function(e) {
  message(paste("Could not load pre-computed multipliers:", e$message))
  message("Using default national average multipliers")
  # Fallback to national averages
  data.frame(
    state_name = c(state.name, "District of Columbia"),
    state_abbr = c(state.abb, "DC"),
    output_mult = rep(2.25, 51),
    emp_mult = rep(12.5, 51),
    income_mult = rep(0.50, 51),
    stringsAsFactors = FALSE
  )
})

# Join multipliers to allzips (state.x contains state abbreviations)
allzips <- allzips %>%
  left_join(state_multipliers, by = c("state.x" = "state_abbr"))

# Join multipliers to casinodata
casinodata <- casinodata %>%
  left_join(state_multipliers, by = c("state" = "state_abbr"))

# Calculate economic impacts based on GGR estimates
allzips <- allzips %>%
  mutate(
    output_mult = coalesce(output_mult, 2.4),
    emp_mult = coalesce(emp_mult, 14),
    income_mult = coalesce(income_mult, 0.55),
    est_total_output = eGGR * output_mult,
    est_jobs = round(eGGR / 1e6 * emp_mult),
    est_labor_income = eGGR * income_mult
  )

casinodata <- casinodata %>%
  mutate(
    output_mult = coalesce(output_mult, 2.4),
    emp_mult = coalesce(emp_mult, 14),
    income_mult = coalesce(income_mult, 0.55),
    est_total_output = eGGR * output_mult,
    est_jobs = round(eGGR / 1e6 * emp_mult),
    est_labor_income = eGGR * income_mult
  )

# Update casino marker popup to include economic impact
casinodata$marker_text <- paste0(
  "<b>", casinodata$name, "</b><br>",
  casinodata$city, ", ", casinodata$state, "<br>",
  "<hr style='margin:5px 0'>",
  "<b>Economic Impact Estimates:</b><br>",
  "Est. GGR: $", format(round(casinodata$eGGR), big.mark=","), "<br>",
  "Output Multiplier: ", round(casinodata$output_mult, 2), "x<br>",
  "Total Economic Output: $", format(round(casinodata$est_total_output), big.mark=","), "<br>",
  "Est. Jobs: ", format(casinodata$est_jobs, big.mark=","), "<br>",
  "Est. Labor Income: $", format(round(casinodata$est_labor_income), big.mark=",")
)

# Data explorer table with economic impact columns
cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    "Desirability Rank" = desirabilityrank,
    "Est. GGR ($)" = eGGR,
    "Output Mult." = output_mult,
    "Total Output ($)" = est_total_output,
    "Est. Jobs" = est_jobs,
    "Income ($)" = income,
    Lat = latitude,
    Long = longitude
  )

message("Data loaded with economic impact multipliers")
