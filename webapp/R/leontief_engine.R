# leontief_engine.R
# Functions to construct A matrix and calculate Leontief inverse for I-O analysis
# Based on stateior ValidationFunctions.R methodology

library(Matrix)

# Cache for Leontief matrices
leontief_cache <- new.env(parent = emptyenv())

#' Get state abbreviation from state name
getStateAbbr <- function(state) {
  if (state == "District of Columbia") return("DC")
  state.abb[match(state, state.name)]
}

#' Normalize IO transactions (divide each column by corresponding output)
#' Replicates useeior:::normalizeIOTransactions
normalizeIOTransactions <- function(use_table, output_vector) {
  # Ensure use_table is a matrix
  use_mat <- as.matrix(use_table)

  # For each column, divide by the corresponding output
  for (j in seq_len(ncol(use_mat))) {
    col_name <- colnames(use_mat)[j]
    if (col_name %in% names(output_vector)) {
      out_val <- output_vector[col_name]
      if (!is.na(out_val) && out_val > 0) {
        use_mat[, j] <- use_mat[, j] / out_val
      } else {
        use_mat[, j] <- 0
      }
    } else {
      use_mat[, j] <- 0
    }
  }

  use_mat[is.na(use_mat) | is.infinite(use_mat)] <- 0
  return(use_mat)
}

#' Get industry codes (exclude final demand columns)
getIndustryCodes <- function(col_names) {
  # Final demand columns start with F0 or T0
  sector_codes <- gsub("/.*", "", col_names)
  col_names[!grepl("^F0|^T0|^F05", sector_codes)]
}

#' Build simplified Leontief system for output multiplier calculation
#' Uses only the Use table portion (industry-by-industry)
#' @param state State name
#' @param year Data year
#' @return List with A matrix, L matrix, sector info
buildLeontiefSystem <- function(state, year) {
  cache_key <- paste("leontief", state, year, sep = "_")

  if (exists(cache_key, envir = leontief_cache)) {
    return(get(cache_key, envir = leontief_cache))
  }

  state_abbr <- getStateAbbr(state)

  # Load the two-region data
  # DomesticUsewithTrade is a list with SoI2SoI, RoUS2SoI, SoI2RoUS, RoUS2RoUS
  domestic_use_list <- loadStateIODataFile(paste0("TwoRegion_Summary_DomesticUsewithTrade_", year))
  industry_output_all <- loadStateIODataFile(paste0("TwoRegion_Summary_IndustryOutput_", year))

  ls <- domestic_use_list[[state]]
  TwoRegionIndustryOutput <- industry_output_all[[state]]

  # Separate SoI and RoUS industry output
  SoI_Industry_Output <- TwoRegionIndustryOutput[endsWith(names(TwoRegionIndustryOutput), state_abbr)]
  RoUS_Industry_Output <- TwoRegionIndustryOutput[endsWith(names(TwoRegionIndustryOutput), "RoUS")]

  # Handle zero outputs (set to 1 to avoid division by zero, as in ValidationFunctions.R)
  SoI_Industry_Output[SoI_Industry_Output == 0] <- 1
  RoUS_Industry_Output[RoUS_Industry_Output == 0] <- 1

  # Get industry columns only (exclude final demand)
  soi_industries <- getIndustryCodes(colnames(ls[["SoI2SoI"]]))
  rous_industries <- getIndustryCodes(colnames(ls[["RoUS2RoUS"]]))

  # Build A matrix blocks (Use table portion only)
  # Following ValidationFunctions.R lines 231-244
  SoI2SoI_A <- normalizeIOTransactions(ls[["SoI2SoI"]][, soi_industries, drop = FALSE],
                                        SoI_Industry_Output)
  RoUS2SoI_A <- normalizeIOTransactions(ls[["RoUS2SoI"]][, soi_industries, drop = FALSE],
                                         SoI_Industry_Output)
  SoI2RoUS_A <- normalizeIOTransactions(ls[["SoI2RoUS"]][, rous_industries, drop = FALSE],
                                         RoUS_Industry_Output)
  RoUS2RoUS_A <- normalizeIOTransactions(ls[["RoUS2RoUS"]][, rous_industries, drop = FALSE],
                                          RoUS_Industry_Output)

  # Assemble the Use-only A matrix
  # Structure: [SoI commodities → SoI industries | SoI commodities → RoUS industries]
  #            [RoUS commodities → SoI industries | RoUS commodities → RoUS industries]
  A_top <- cbind(SoI2SoI_A, SoI2RoUS_A)
  A_bottom <- cbind(RoUS2SoI_A, RoUS2RoUS_A)
  A <- rbind(A_top, A_bottom)

  A[is.na(A) | is.infinite(A)] <- 0

  # Make A square by using common row/col names
  common_names <- intersect(rownames(A), colnames(A))
  if (length(common_names) > 0) {
    A <- A[common_names, common_names]
  }

  # Calculate Leontief inverse: L = (I - A)^(-1)
  I <- diag(nrow(A))
  rownames(I) <- rownames(A)
  colnames(I) <- colnames(A)

  L <- tryCatch({
    solve(I - A, tol = 1e-20)
  }, error = function(e) {
    warning("Matrix near-singular, using regularization")
    solve(I - A + diag(1e-10, nrow(A)))
  })

  # Get sector mappings
  sectors <- colnames(A)
  soi_sectors <- sectors[grepl(paste0("/US-", state_abbr, "$"), sectors)]
  rous_sectors <- sectors[grepl("/RoUS$", sectors)]

  result <- list(
    A = A,
    L = L,
    sectors = sectors,
    soi_sectors = soi_sectors,
    rous_sectors = rous_sectors,
    state = state,
    state_abbr = state_abbr,
    year = year
  )

  assign(cache_key, result, envir = leontief_cache)
  return(result)
}

#' Find the gambling sector column in the Leontief system
#' @param leontief_system Result from buildLeontiefSystem
#' @param region "SoI" for State of Interest, "RoUS" for Rest of US
#' @return Column name for gambling sector
findGamblingSectorColumn <- function(leontief_system, region = "SoI") {
  sector <- "713"

  if (region == "SoI") {
    pattern <- paste0("^", sector, "/US-", leontief_system$state_abbr, "$")
  } else {
    pattern <- paste0("^", sector, "/RoUS$")
  }

  cols <- grep(pattern, leontief_system$sectors, value = TRUE)
  if (length(cols) > 0) return(cols[1])

  # Fallback: broader search
  cols <- grep(paste0("^", sector, "/"), leontief_system$sectors, value = TRUE)
  if (length(cols) > 0) return(cols[1])

  warning(paste("Sector", sector, "not found"))
  return(NULL)
}

#' Clear the Leontief cache
clearLeontiefCache <- function() {
  rm(list = ls(envir = leontief_cache), envir = leontief_cache)
}
