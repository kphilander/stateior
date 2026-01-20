# multiplier_calc.R
# Functions to calculate Type I and Type II economic multipliers

#' Calculate Type I multipliers (direct + indirect effects)
#' @param state State name
#' @param year Data year
#' @param sector BEA sector code (default: "713" for gambling)
#' @return List with output, employment, and income multipliers
calculateTypeIMultipliers <- function(state, year, sector = "713") {
  # Build Leontief system

  leontief <- buildLeontiefSystem(state, year)

  # Find gambling sector column
  sector_col <- findGamblingSectorColumn(leontief, region = "SoI")

  if (is.null(sector_col)) {
    return(list(
      output = NA,
      employment = NA,
      income = NA,
      error = "Sector not found"
    ))
  }

  # Get column index
  col_idx <- which(colnames(leontief$L) == sector_col)
  if (length(col_idx) == 0) {
    col_idx <- which(grepl(sector, colnames(leontief$L)))[1]
  }

  # Output multiplier: sum ONLY SoI rows of Leontief column
  # This gives total output generated IN THE STATE per $1 of final demand
  # (excluding rest-of-US spillover effects for state-level analysis)
  soi_rows <- leontief$soi_sectors
  output_mult <- sum(leontief$L[soi_rows, col_idx], na.rm = TRUE)

  # Get employment and compensation coefficients
  emp_coef <- tryCatch(
    getEmploymentCoefficients(state, year),
    error = function(e) numeric()
  )

  comp_coef <- tryCatch(
    getCompensationCoefficients(state, year),
    error = function(e) numeric()
  )

  # Employment multiplier: weighted sum of Leontief column by employment coefficients
  emp_mult <- NA
  if (length(emp_coef) > 0) {
    # Match sectors between Leontief and employment
    L_col <- leontief$L[, col_idx]
    emp_impact <- 0
    for (sector_name in names(L_col)) {
      clean_sector <- gsub("/.*", "", sector_name)
      if (clean_sector %in% names(emp_coef)) {
        emp_impact <- emp_impact + L_col[sector_name] * emp_coef[clean_sector]
      }
    }
    emp_mult <- emp_impact
  }

  # Income multiplier: weighted sum by compensation coefficients
  income_mult <- NA
  if (length(comp_coef) > 0) {
    L_col <- leontief$L[, col_idx]
    income_impact <- 0
    for (sector_name in names(L_col)) {
      clean_sector <- gsub("/.*", "", sector_name)
      if (clean_sector %in% names(comp_coef)) {
        income_impact <- income_impact + L_col[sector_name] * comp_coef[clean_sector]
      }
    }
    income_mult <- income_impact
  }

  return(list(
    output = output_mult,
    employment = emp_mult,
    income = income_mult,
    sector = sector,
    state = state,
    year = year,
    type = "Type I"
  ))
}

#' Calculate Type II multipliers (including induced household effects)
#' @param state State name
#' @param year Data year
#' @param sector BEA sector code
#' @return List with output, employment, and income multipliers
calculateTypeIIMultipliers <- function(state, year, sector = "713") {
  # Type II includes household consumption induced effects
  # We approximate this by applying a standard induced effect ratio
  # Typical induced effects add 20-40% to Type I effects

  type1 <- calculateTypeIMultipliers(state, year, sector)

  if (is.na(type1$output)) {
    return(type1)
  }

  # Induced effect factor (typically 1.2-1.4x Type I)
  # This is an approximation - full Type II requires household consumption patterns
  induced_factor <- 1.35

  return(list(
    output = type1$output * induced_factor,
    employment = if (!is.na(type1$employment)) type1$employment * induced_factor else NA,
    income = if (!is.na(type1$income)) type1$income * induced_factor else NA,
    sector = sector,
    state = state,
    year = year,
    type = "Type II"
  ))
}

#' Get all multipliers for the gambling sector
#' @param state State name
#' @param year Data year
#' @return Data frame with Type I and Type II multipliers
getGamblingMultipliers <- function(state, year) {
  type1 <- calculateTypeIMultipliers(state, year, "713")
  type2 <- calculateTypeIIMultipliers(state, year, "713")

  result <- data.frame(
    Multiplier = c("Output", "Employment (jobs/$M)", "Income"),
    TypeI = c(type1$output, type1$employment, type1$income),
    TypeII = c(type2$output, type2$employment, type2$income),
    stringsAsFactors = FALSE
  )

  return(result)
}

#' Get multipliers for multiple states (for comparison)
#' @param states Vector of state names
#' @param year Data year
#' @return Data frame with multipliers by state
compareStateMultipliers <- function(states, year) {
  results <- data.frame()

  for (state in states) {
    tryCatch({
      mults <- getGamblingMultipliers(state, year)
      mults$State <- state
      results <- rbind(results, mults)
    }, error = function(e) {
      warning(paste("Could not calculate multipliers for", state))
    })
  }

  return(results)
}

#' Calculate direct effect ratios
#' @param state State name
#' @param year Data year
#' @return List with direct employment and income ratios
getDirectEffectRatios <- function(state, year) {
  emp_coef <- tryCatch(
    getEmploymentCoefficients(state, year),
    error = function(e) numeric()
  )

  comp_coef <- tryCatch(
    getCompensationCoefficients(state, year),
    error = function(e) numeric()
  )

  gambling_code <- "713"

  direct_emp <- if (gambling_code %in% names(emp_coef)) emp_coef[gambling_code] else NA
  direct_income <- if (gambling_code %in% names(comp_coef)) comp_coef[gambling_code] else NA

  return(list(
    employment_per_million = direct_emp,
    income_ratio = direct_income
  ))
}
