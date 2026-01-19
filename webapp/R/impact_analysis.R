# impact_analysis.R
# Functions to calculate economic impacts from casino operations

#' Calculate economic impacts from casino gross gaming revenue
#' @param ggr Gross Gaming Revenue in dollars
#' @param state State where casino is located
#' @param year Analysis year
#' @param include_induced Whether to include Type II (induced) effects
#' @return List with detailed impact breakdown
calculateCasinoImpact <- function(ggr, state, year = 2020, include_induced = TRUE) {
  # Get multipliers
  if (include_induced) {
    mults <- calculateTypeIIMultipliers(state, year, "713")
  } else {
    mults <- calculateTypeIMultipliers(state, year, "713")
  }

  # Get direct effect ratios
  direct <- getDirectEffectRatios(state, year)

  # Convert GGR to millions for calculations
  ggr_millions <- ggr / 1e6

  # Calculate direct effects
  direct_output <- ggr  # Direct output equals the GGR
  direct_jobs <- if (!is.na(direct$employment_per_million)) {
    direct$employment_per_million * ggr_millions
  } else {
    # Default: ~8 jobs per $1M in gambling industry
    8 * ggr_millions
  }
  direct_income <- if (!is.na(direct$income_ratio)) {
    direct$income_ratio * ggr
  } else {
    # Default: ~35% of output goes to labor income
    0.35 * ggr
  }

  # Calculate total effects using multipliers
  total_output <- if (!is.na(mults$output)) mults$output * ggr else ggr
  total_jobs <- if (!is.na(mults$employment)) {
    mults$employment * ggr_millions
  } else {
    direct_jobs * (if (include_induced) 1.8 else 1.4)
  }
  total_income <- if (!is.na(mults$income)) mults$income * ggr else direct_income * 1.5

  # Calculate indirect and induced effects
  indirect_output <- (total_output - direct_output) * (if (include_induced) 0.6 else 1.0)
  induced_output <- if (include_induced) total_output - direct_output - indirect_output else 0

  indirect_jobs <- (total_jobs - direct_jobs) * (if (include_induced) 0.6 else 1.0)
  induced_jobs <- if (include_induced) total_jobs - direct_jobs - indirect_jobs else 0

  indirect_income <- (total_income - direct_income) * (if (include_induced) 0.6 else 1.0)
  induced_income <- if (include_induced) total_income - direct_income - indirect_income else 0

  # Estimate tax revenue
  tax_estimate <- estimateTaxRevenue(ggr, state, total_jobs, total_income)

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

#' Create impact summary data frame for display
#' @param impact Result from calculateCasinoImpact
#' @return Data frame formatted for display
formatImpactSummary <- function(impact) {
  data.frame(
    Category = c("Jobs", "Labor Income", "Economic Output", "Tax Revenue"),
    Direct = c(
      formatNumber(impact$jobs_direct),
      formatCurrency(impact$income_direct),
      formatCurrency(impact$output_direct),
      "-"
    ),
    Indirect = c(
      formatNumber(impact$jobs_indirect),
      formatCurrency(impact$income_indirect),
      formatCurrency(impact$output_indirect),
      "-"
    ),
    Induced = c(
      formatNumber(impact$jobs_induced),
      formatCurrency(impact$income_induced),
      formatCurrency(impact$output_induced),
      "-"
    ),
    Total = c(
      formatNumber(impact$jobs_total),
      formatCurrency(impact$income_total),
      formatCurrency(impact$output_total),
      formatCurrency(impact$tax_total)
    ),
    stringsAsFactors = FALSE
  )
}

#' Calculate impacts for multiple casinos
#' @param casinos Data frame with columns: name, state, ggr
#' @param year Analysis year
#' @param include_induced Include Type II effects
#' @return List with individual and aggregated impacts
calculateMultipleCasinoImpacts <- function(casinos, year = 2020, include_induced = TRUE) {
  individual_impacts <- list()
  aggregate <- list(
    jobs_total = 0,
    income_total = 0,
    output_total = 0,
    tax_total = 0
  )

  for (i in 1:nrow(casinos)) {
    casino <- casinos[i, ]
    impact <- calculateCasinoImpact(
      ggr = casino$ggr,
      state = casino$state,
      year = year,
      include_induced = include_induced
    )

    individual_impacts[[casino$name]] <- impact

    aggregate$jobs_total <- aggregate$jobs_total + impact$jobs_total
    aggregate$income_total <- aggregate$income_total + impact$income_total
    aggregate$output_total <- aggregate$output_total + impact$output_total
    aggregate$tax_total <- aggregate$tax_total + impact$tax_total
  }

  return(list(
    individual = individual_impacts,
    aggregate = aggregate,
    casino_count = nrow(casinos)
  ))
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
