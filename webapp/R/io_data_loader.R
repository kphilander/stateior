# io_data_loader.R
# Functions to load stateior I-O data with caching for performance

library(stateior)

# Cache for loaded I-O data (avoids repeated downloads)
io_cache <- new.env(parent = emptyenv())

#' Load two-region I-O data for a state with caching
#' @param state State name (e.g., "Nevada")
#' @param year Data year (2012-2024)
#' @return List containing DomesticUse, IndustryOutput, CommodityOutput, ValueAdded
loadTwoRegionIOData <- function(state, year) {
  cache_key <- paste(state, year, sep = "_")

  if (exists(cache_key, envir = io_cache)) {
    return(get(cache_key, envir = io_cache))
  }

  # Load data from stateior
  domestic_use_all <- loadStateIODataFile(paste0("TwoRegion_Summary_DomesticUse_", year))
  industry_output_all <- loadStateIODataFile(paste0("TwoRegion_Summary_IndustryOutput_", year))
  commodity_output_all <- loadStateIODataFile(paste0("TwoRegion_Summary_CommodityOutput_", year))
  value_added_all <- loadStateIODataFile(paste0("TwoRegion_Summary_ValueAdded_", year))

  result <- list(
    DomesticUse = domestic_use_all[[state]],
    IndustryOutput = industry_output_all[[state]],
    CommodityOutput = commodity_output_all[[state]],
    ValueAdded = value_added_all[[state]]
  )

  # Cache the result
  assign(cache_key, result, envir = io_cache)

  return(result)
}

#' Get employment data by BEA sector for a state
#' @param state State name
#' @param year Data year
#' @return Data frame with BEA sector code and employment
getStateEmployment <- function(state, year) {
  # Use 2017 schema for years >= 2017, otherwise 2012
  specs <- if (year >= 2017) {
    list(BaseIOSchema = 2017, BaseIOLevel = "Summary")
  } else {
    list(BaseIOSchema = 2012, BaseIOLevel = "Summary")
  }

  emp_table <- getStateEmploymentTable(year, specs)
  state_emp <- emp_table[emp_table$State == state, ]

  return(state_emp)
}

#' Calculate employment coefficients (jobs per $1M output)
#' @param state State name
#' @param year Data year
#' @return Named vector of employment coefficients by BEA sector
getEmploymentCoefficients <- function(state, year) {
  emp_data <- getStateEmployment(state, year)
  io_data <- loadTwoRegionIOData(state, year)

  # Get industry output for the state (SoI portion)
  industry_output <- io_data$IndustryOutput

  # Match employment to output by sector code
  schema_col <- if (year >= 2017) "BEA_2017_Summary_Code" else "BEA_2012_Summary_Code"

  # Create coefficient vector
  coefficients <- numeric()
  for (i in 1:nrow(emp_data)) {
    sector <- emp_data[[schema_col]][i]
    emp <- emp_data$Emp[i]

    # Find matching output (look for SoI sectors)
    output_row <- grep(paste0("^", sector), rownames(industry_output), value = TRUE)
    if (length(output_row) > 0) {
      output_val <- industry_output[output_row[1], "Output"]
      if (!is.na(output_val) && output_val > 0) {
        # Employment per $1M output
        coefficients[sector] <- (emp / output_val) * 1e6
      }
    }
  }

  return(coefficients)
}

#' Get compensation (labor income) coefficients
#' @param state State name
#' @param year Data year
#' @return Named vector of compensation per $1 output by sector
getCompensationCoefficients <- function(state, year) {
  io_data <- loadTwoRegionIOData(state, year)

  value_added <- io_data$ValueAdded
  industry_output <- io_data$IndustryOutput

  # Compensation is typically row "V001" in value added
  if ("V001" %in% rownames(value_added)) {
    comp_row <- value_added["V001", ]
  } else {
    # Try to find compensation row
    comp_row <- value_added[grep("Compensation|V001", rownames(value_added)), ]
    if (nrow(comp_row) > 0) comp_row <- comp_row[1, ]
  }

  # Calculate coefficients for SoI industries
  soi_cols <- grep("/US-", colnames(comp_row), value = TRUE)
  coefficients <- numeric()

  for (col in soi_cols) {
    sector <- gsub("/US-.*", "", col)
    comp_val <- as.numeric(comp_row[, col])

    output_row <- grep(paste0("^", sector), rownames(industry_output), value = TRUE)
    if (length(output_row) > 0) {
      output_val <- industry_output[output_row[1], "Output"]
      if (!is.na(output_val) && output_val > 0 && !is.na(comp_val)) {
        coefficients[sector] <- comp_val / output_val
      }
    }
  }

  return(coefficients)
}

#' Get available years for I-O data
#' @return Vector of available years
getAvailableYears <- function() {
  return(2012:2024)
}

#' Get list of states
#' @return Vector of state names
getStateList <- function() {
  return(c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
           "Colorado", "Connecticut", "Delaware", "District of Columbia",
           "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
           "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
           "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
           "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
           "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
           "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
           "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
           "Washington", "West Virginia", "Wisconsin", "Wyoming"))
}

#' Clear the I-O data cache
clearIOCache <- function() {
  rm(list = ls(envir = io_cache), envir = io_cache)
}
