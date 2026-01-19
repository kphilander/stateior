# precompute_multipliers.R
# Pre-compute Type II multipliers for gambling sector (BEA 713) for all states
# Uses stateior I-O tables and Leontief analysis

# Load stateior package
library(stateior)

# Source the calculation modules
source("../R/io_data_loader.R")
source("../R/leontief_engine.R")
source("../R/multiplier_calc.R")

# All 50 states + DC
states <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
            "Colorado", "Connecticut", "Delaware", "District of Columbia",
            "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
            "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
            "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
            "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
            "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
            "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
            "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
            "Washington", "West Virginia", "Wisconsin", "Wyoming")

year <- 2020  # Most recent stable year with complete data

message("==============================================")
message("Pre-computing Type II multipliers for gambling sector (BEA 713)")
message(paste("Year:", year))
message(paste("States:", length(states)))
message("==============================================\n")

results <- data.frame(
  state_name = character(),
  state_abbr = character(),
  output_mult = numeric(),
  emp_mult = numeric(),
  income_mult = numeric(),
  stringsAsFactors = FALSE
)

for (state in states) {
  message(paste("Processing", state, "..."))

  mults <- tryCatch({
    calculateTypeIIMultipliers(state, year, "713")
  }, error = function(e) {
    message(paste("  Error:", e$message))
    NULL
  })

  # Get state abbreviation
  abbr <- state.abb[match(state, state.name)]
  if (state == "District of Columbia") abbr <- "DC"

  results <- rbind(results, data.frame(
    state_name = state,
    state_abbr = abbr,
    output_mult = if (!is.null(mults) && !is.na(mults$output)) mults$output else NA,
    emp_mult = if (!is.null(mults) && !is.na(mults$employment)) mults$employment else NA,
    income_mult = if (!is.null(mults) && !is.na(mults$income)) mults$income else NA,
    stringsAsFactors = FALSE
  ))

  if (!is.null(mults) && !is.na(mults$output)) {
    message(paste("  Output:", round(mults$output, 3),
                  "| Emp:", round(mults$employment, 2),
                  "| Income:", round(mults$income, 3)))
  }
}

# Add metadata attributes
attr(results, "source") <- "stateior I-O tables (Two-Region model)"
attr(results, "sector") <- "713 (Amusements, gambling, and recreation)"
attr(results, "sector_description") <- "BEA Summary code 713 includes casinos, racetracks, gambling establishments"
attr(results, "year") <- year
attr(results, "computed_date") <- Sys.Date()
attr(results, "multiplier_type") <- "Type II (includes induced household effects)"
attr(results, "methodology") <- "Leontief inverse L=(I-A)^-1 with 1.35x induced factor"

# Create data directory if needed
if (!dir.exists("../data")) {
  dir.create("../data")
}

# Save results
saveRDS(results, "../data/state_multipliers.rds")
write.csv(results, "../data/state_multipliers.csv", row.names = FALSE)

message("\n==============================================")
message("DONE!")
message(paste("Saved to: data/state_multipliers.rds"))
message(paste("Saved to: data/state_multipliers.csv"))
message("==============================================\n")

# Print summary
message("Summary statistics:")
message(paste("  States processed:", nrow(results)))
message(paste("  States with valid multipliers:", sum(!is.na(results$output_mult))))
message(paste("  Output multiplier range:",
              round(min(results$output_mult, na.rm=TRUE), 2), "-",
              round(max(results$output_mult, na.rm=TRUE), 2)))
message(paste("  Employment multiplier range:",
              round(min(results$emp_mult, na.rm=TRUE), 2), "-",
              round(max(results$emp_mult, na.rm=TRUE), 2)))

message("\nResults:")
print(results)
