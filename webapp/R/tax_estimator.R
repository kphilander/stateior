# tax_estimator.R
# Functions to estimate tax revenue from casino operations

#' Load state gaming tax rates
#' @return Data frame with state tax rates
loadStateTaxRates <- function() {
  tax_file <- "data/state_tax_rates.csv"

  if (file.exists(tax_file)) {
    return(read.csv(tax_file, stringsAsFactors = FALSE))
  }

  # Return default rates if file not found
  getDefaultTaxRates()
}

#' Get default tax rates for all states
#' @return Data frame with default tax rates
getDefaultTaxRates <- function() {
  # Gaming tax rates vary widely by state
  # These are approximate average rates
  data.frame(
    state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California",
              "Colorado", "Connecticut", "Delaware", "District of Columbia",
              "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana",
              "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland",
              "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri",
              "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey",
              "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio",
              "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina",
              "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia",
              "Washington", "West Virginia", "Wisconsin", "Wyoming"),
    gaming_tax_rate = c(0.00, 0.00, 0.08, 0.20, 0.00,
                        0.20, 0.25, 0.43, 0.10,
                        0.00, 0.00, 0.00, 0.00, 0.34, 0.35,
                        0.22, 0.27, 0.00, 0.22, 0.46, 0.67,
                        0.49, 0.19, 0.00, 0.12, 0.21,
                        0.15, 0.00, 0.07, 0.35, 0.15,
                        0.00, 0.45, 0.00, 0.00, 0.33,
                        0.10, 0.00, 0.54, 0.18, 0.00,
                        0.09, 0.00, 0.00, 0.00, 0.00, 0.00,
                        0.00, 0.53, 0.00, 0.00),
    income_tax_rate = c(0.05, 0.00, 0.045, 0.055, 0.093,
                        0.044, 0.07, 0.066, 0.085,
                        0.00, 0.055, 0.11, 0.058, 0.05, 0.032,
                        0.06, 0.057, 0.05, 0.045, 0.075, 0.058,
                        0.05, 0.043, 0.10, 0.05, 0.054,
                        0.068, 0.068, 0.00, 0.05, 0.11,
                        0.055, 0.109, 0.053, 0.029, 0.04,
                        0.05, 0.099, 0.031, 0.06, 0.07,
                        0.00, 0.00, 0.00, 0.0495, 0.088, 0.058,
                        0.00, 0.065, 0.077, 0.00),
    sales_tax_rate = c(0.04, 0.00, 0.056, 0.065, 0.073,
                       0.029, 0.064, 0.00, 0.06,
                       0.06, 0.04, 0.04, 0.06, 0.063, 0.07,
                       0.06, 0.065, 0.06, 0.045, 0.055, 0.06,
                       0.063, 0.06, 0.069, 0.07, 0.042,
                       0.00, 0.055, 0.069, 0.00, 0.066,
                       0.051, 0.08, 0.048, 0.05, 0.058,
                       0.045, 0.00, 0.06, 0.07, 0.06,
                       0.045, 0.07, 0.063, 0.061, 0.06, 0.053,
                       0.065, 0.06, 0.05, 0.04),
    stringsAsFactors = FALSE
  )
}

#' Get tax rates for a specific state
#' @param state State name
#' @return List with gaming, income, and sales tax rates
getStateTaxRates <- function(state) {
  rates <- loadStateTaxRates()
  state_rates <- rates[rates$state == state, ]

  if (nrow(state_rates) == 0) {
    # Return national averages if state not found
    return(list(
      gaming = 0.15,
      income = 0.05,
      sales = 0.06
    ))
  }

  return(list(
    gaming = state_rates$gaming_tax_rate,
    income = state_rates$income_tax_rate,
    sales = state_rates$sales_tax_rate
  ))
}

#' Estimate tax revenue from casino operations
#' @param ggr Gross Gaming Revenue
#' @param state State name
#' @param total_jobs Total jobs created (for income tax estimation)
#' @param total_income Total labor income generated
#' @return List with tax revenue estimates
estimateTaxRevenue <- function(ggr, state, total_jobs = NULL, total_income = NULL) {
  rates <- getStateTaxRates(state)

  # Gaming tax: directly on GGR
  gaming_tax <- ggr * rates$gaming

  # Income tax: on labor income generated
  income_tax <- 0
  if (!is.null(total_income) && !is.na(total_income)) {
    income_tax <- total_income * rates$income
  }

  # Sales tax: estimate based on household spending from wages
  # Assume 70% of income is spent on taxable goods/services
  sales_tax <- 0
  if (!is.null(total_income) && !is.na(total_income)) {
    taxable_spending <- total_income * 0.70
    sales_tax <- taxable_spending * rates$sales
  }

  total_tax <- gaming_tax + income_tax + sales_tax

  return(list(
    gaming_tax = gaming_tax,
    income_tax = income_tax,
    sales_tax = sales_tax,
    total_tax = total_tax,
    rates_used = rates
  ))
}

#' Compare tax revenue across states for same GGR
#' @param ggr Gross Gaming Revenue
#' @param states Vector of state names
#' @return Data frame with tax comparison
compareTaxRevenueByState <- function(ggr, states) {
  results <- data.frame()

  for (state in states) {
    tax <- estimateTaxRevenue(ggr, state, total_income = ggr * 0.35)
    results <- rbind(results, data.frame(
      State = state,
      Gaming_Tax = tax$gaming_tax,
      Income_Tax = tax$income_tax,
      Sales_Tax = tax$sales_tax,
      Total_Tax = tax$total_tax,
      Gaming_Rate = tax$rates_used$gaming,
      stringsAsFactors = FALSE
    ))
  }

  return(results[order(-results$Total_Tax), ])
}
