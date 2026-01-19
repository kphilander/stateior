# impact_analysis.R
# Functions to calculate economic impacts from casino operations

#' Calculate economic impacts from casino gross gaming revenue
#' @param ggr Gross Gaming Revenue in dollars
#' @param state State where casino is located
#' @param year Analysis year
#' @param include_induced Whether to include Type II (induced) effects
#' @return List with detailed impact breakdown
calculateCasinoImpact <- function(ggr, state, year = 2020, include_induced = TRUE) {

  # Try to get multipliers from stateior, fall back to defaults
  mults <- tryCatch({
    if (exists("stateior_available", envir = .GlobalEnv) &&
        get("stateior_available", envir = .GlobalEnv) &&
        exists("calculateTypeIIMultipliers")) {
      if (include_induced) {
        calculateTypeIIMultipliers(state, year, "713")
      } else {
        calculateTypeIMultipliers(state, year, "713")
      }
    } else {
      getDefaultMultipliers(include_induced)
    }
  }, error = function(e) {
    message(paste("Using default multipliers:", e$message))
    getDefaultMultipliers(include_induced)
  })

  # Get direct effect ratios (with fallback)
  direct <- tryCatch({
    if (exists("stateior_available", envir = .GlobalEnv) &&
        get("stateior_available", envir = .GlobalEnv) &&
        exists("getDirectEffectRatios")) {
      getDirectEffectRatios(state, year)
    } else {
      list(employment_per_million = 8, income_ratio = 0.35)
    }
  }, error = function(e) {
    list(employment_per_million = 8, income_ratio = 0.35)
  })

  # Convert GGR to millions for calculations
  ggr_millions <- ggr / 1e6

  # Calculate direct effects
  direct_output <- ggr
  direct_jobs <- if (!is.null(direct$employment_per_million) && !is.na(direct$employment_per_million)) {
    direct$employment_per_million * ggr_millions
  } else {
    8 * ggr_millions  # Default: ~8 jobs per $1M
  }
  direct_income <- if (!is.null(direct$income_ratio) && !is.na(direct$income_ratio)) {
    direct$income_ratio * ggr
  } else {
    0.35 * ggr  # Default: ~35% goes to labor
  }

  # Calculate total effects using multipliers
  output_mult <- if (!is.null(mults$output) && !is.na(mults$output)) mults$output else (if (include_induced) 2.4 else 1.8)
  emp_mult <- if (!is.null(mults$employment) && !is.na(mults$employment)) mults$employment else (if (include_induced) 14 else 10)
  income_mult <- if (!is.null(mults$income) && !is.na(mults$income)) mults$income else (if (include_induced) 0.55 else 0.40)

  total_output <- output_mult * ggr
  total_jobs <- emp_mult * ggr_millions
  total_income <- income_mult * ggr

  # Calculate indirect and induced effects
  indirect_output <- (total_output - direct_output) * (if (include_induced) 0.6 else 1.0)
  induced_output <- if (include_induced) total_output - direct_output - indirect_output else 0

  indirect_jobs <- (total_jobs - direct_jobs) * (if (include_induced) 0.6 else 1.0)
  induced_jobs <- if (include_induced) total_jobs - direct_jobs - indirect_jobs else 0

  indirect_income <- (total_income - direct_income) * (if (include_induced) 0.6 else 1.0)
  induced_income <- if (include_induced) total_income - direct_income - indirect_income else 0

  # Estimate tax revenue
  tax_estimate <- tryCatch({
    if (exists("estimateTaxRevenue")) {
      estimateTaxRevenue(ggr, state, total_jobs, total_income)
    } else {
      getDefaultTaxEstimate(ggr, total_income)
    }
  }, error = function(e) {
    getDefaultTaxEstimate(ggr, total_income)
  })

  return(list(
    # Input parameters
    ggr = ggr,
    state = state,
    year = year,
    include_induced = include_induced,

    # Jobs
    jobs_direct = round(direct_jobs),
    jobs_indirect = round(indirect_jobs),
    jobs_induced = round(induced_jobs),
    jobs_total = round(total_jobs),

    # Income (labor income)
    income_direct = direct_income,
    income_indirect = indirect_income,
    income_induced = induced_income,
    income_total = total_income,

    # Output (economic activity)
    output_direct = direct_output,
    output_indirect = indirect_output,
    output_induced = induced_output,
    output_total = total_output,

    # Tax revenue
    tax_gaming = tax_estimate$gaming_tax,
    tax_income = tax_estimate$income_tax,
    tax_sales = tax_estimate$sales_tax,
    tax_total = tax_estimate$total_tax,

    # Multipliers used
    multipliers = mults
  ))
}

#' Get default multipliers when stateior not available
#' @param include_induced Whether to use Type II multipliers
#' @return List with default multipliers
getDefaultMultipliers <- function(include_induced = TRUE) {
  if (include_induced) {
    list(
      output = 2.4,
      employment = 14,
      income = 0.55,
      sector = "713",
      state = "Default",
      year = 2020,
      type = "Type II (Default)"
    )
  } else {
    list(
      output = 1.8,
      employment = 10,
      income = 0.40,
      sector = "713",
      state = "Default",
      year = 2020,
      type = "Type I (Default)"
    )
  }
}

#' Get default tax estimate
#' @param ggr Gross gaming revenue
#' @param total_income Total labor income
#' @return List with tax estimates
getDefaultTaxEstimate <- function(ggr, total_income) {
  list(
    gaming_tax = ggr * 0.15,
    income_tax = total_income * 0.05,
    sales_tax = total_income * 0.7 * 0.06,
    total_tax = ggr * 0.15 + total_income * 0.05 + total_income * 0.7 * 0.06
  )
}

#' Format number with commas
#' @param x Number to format
#' @return Formatted string
formatNumber <- function(x) {
  if (is.na(x) || is.null(x)) return("-")
  format(round(x), big.mark = ",", scientific = FALSE)
}

#' Format currency value
#' @param x Value in dollars
#' @return Formatted currency string
formatCurrency <- function(x) {
  if (is.na(x) || is.null(x)) return("-")
  if (abs(x) >= 1e9) {
    paste0("$", round(x / 1e9, 1), "B")
  } else if (abs(x) >= 1e6) {
    paste0("$", round(x / 1e6, 1), "M")
  } else if (abs(x) >= 1e3) {
    paste0("$", round(x / 1e3, 1), "K")
  } else {
    paste0("$", format(round(x), big.mark = ","))
  }
}

#' Get impact breakdown for pie chart
#' @param impact Result from calculateCasinoImpact
#' @param metric "jobs", "income", or "output"
#' @return Data frame with category and value for plotting
getImpactBreakdown <- function(impact, metric = "jobs") {
  if (metric == "jobs") {
    data.frame(
      Category = c("Direct", "Indirect", "Induced"),
      Value = c(impact$jobs_direct, impact$jobs_indirect, impact$jobs_induced)
    )
  } else if (metric == "income") {
    data.frame(
      Category = c("Direct", "Indirect", "Induced"),
      Value = c(impact$income_direct, impact$income_indirect, impact$income_induced)
    )
  } else {
    data.frame(
      Category = c("Direct", "Indirect", "Induced"),
      Value = c(impact$output_direct, impact$output_indirect, impact$output_induced)
    )
  }
}
