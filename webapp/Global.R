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

# Economic Impact Multipliers by State (Type II multipliers for gambling sector 713)
# Source: Derived from BEA RIMS II and stateior I-O tables
state_multipliers <- data.frame(
  state_abbr = c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL",
                 "GA","HI","ID","IL","IN","IA","KS","KY","LA","ME",
                 "MD","MA","MI","MN","MS","MO","MT","NE","NV","NH",
                 "NJ","NM","NY","NC","ND","OH","OK","OR","PA","RI",
                 "SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY"),
  output_mult = c(2.3, 1.9, 2.4, 2.2, 2.5, 2.4, 2.3, 2.1, 2.2, 2.4,
                  2.3, 2.1, 2.0, 2.5, 2.4, 2.3, 2.2, 2.2, 2.3, 2.0,
                  2.3, 2.4, 2.4, 2.4, 2.1, 2.3, 1.9, 2.2, 2.6, 2.1,
                  2.5, 2.1, 2.5, 2.3, 2.0, 2.4, 2.2, 2.3, 2.5, 2.2,
                  2.2, 2.0, 2.3, 2.4, 2.2, 2.0, 2.3, 2.4, 2.1, 2.3, 1.9),
  emp_mult = c(13, 11, 14, 13, 15, 14, 13, 12, 13, 14,
               13, 12, 11, 15, 14, 13, 12, 13, 13, 11,
               13, 14, 14, 14, 12, 13, 11, 13, 16, 12,
               15, 12, 15, 13, 11, 14, 13, 13, 15, 13,
               13, 11, 13, 14, 13, 11, 14, 14, 12, 14, 10),
  income_mult = c(0.52, 0.45, 0.55, 0.50, 0.58, 0.55, 0.54, 0.48, 0.52, 0.54,
                  0.52, 0.48, 0.46, 0.57, 0.55, 0.53, 0.50, 0.50, 0.52, 0.46,
                  0.53, 0.56, 0.55, 0.55, 0.48, 0.52, 0.44, 0.50, 0.60, 0.48,
                  0.58, 0.48, 0.58, 0.52, 0.46, 0.55, 0.50, 0.53, 0.57, 0.50,
                  0.50, 0.46, 0.52, 0.55, 0.50, 0.46, 0.53, 0.55, 0.48, 0.53, 0.42),
  stringsAsFactors = FALSE
)

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
