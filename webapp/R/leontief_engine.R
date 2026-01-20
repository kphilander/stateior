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

#' Normalize Use table by output (handles name format mismatch)
#' @param use_mat Use table matrix
#' @param output_vec Industry output vector (with location suffixes)
#' @return Normalized A matrix block
normalizeUseTable <- function(use_mat, output_vec) {
  use_mat <- as.matrix(use_mat)

  # Strip location suffix from output names to match Use table columns
  output_names_clean <- gsub("/US-.*$|/RoUS$", "", names(output_vec))
  names(output_vec) <- output_names_clean

  for (j in seq_len(ncol(use_mat))) {
    col_name <- colnames(use_mat)[j]
    if (col_name %in% names(output_vec)) {
      out_val <- output_vec[col_name]
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

#' Get industry columns (exclude final demand F0xx, T0xx)
getIndustryCols <- function(col_names) {
  col_names[!grepl("^F0|^T0", col_names)]
}

#' Add suffix to row/col names to distinguish SoI from RoUS
suffixNames <- function(mat, suffix) {
  rownames(mat) <- paste0(rownames(mat), suffix)
  colnames(mat) <- paste0(colnames(mat), suffix)
  return(mat)
}

#' Build Leontief system for I-O analysis
#' Uses stateior two-region structure (SoI and RoUS)
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
  domestic_use_list <- loadStateIODataFile(paste0("TwoRegion_Summary_DomesticUsewithTrade_", year))
  industry_output_all <- loadStateIODataFile(paste0("TwoRegion_Summary_IndustryOutput_", year))

  ls <- domestic_use_list[[state]]
  output <- industry_output_all[[state]]

  # Separate SoI and RoUS industry output
  soi_output <- output[endsWith(names(output), state_abbr)]
  rous_output <- output[endsWith(names(output), "RoUS")]

  # Handle zero outputs
  soi_output[soi_output == 0] <- 1
  rous_output[rous_output == 0] <- 1

  # Get industry columns from each block
  soi2soi_ind <- getIndustryCols(colnames(ls$SoI2SoI))
  rous2soi_ind <- getIndustryCols(colnames(ls$RoUS2SoI))
  soi2rous_ind <- getIndustryCols(colnames(ls$SoI2RoUS))
  rous2rous_ind <- getIndustryCols(colnames(ls$RoUS2RoUS))

  # Use common columns for each region
  soi_ind <- intersect(soi2soi_ind, rous2soi_ind)
  rous_ind <- intersect(soi2rous_ind, rous2rous_ind)

  # Normalize each block
  A_SoI2SoI <- normalizeUseTable(ls$SoI2SoI[, soi_ind, drop = FALSE], soi_output)
  A_RoUS2SoI <- normalizeUseTable(ls$RoUS2SoI[, soi_ind, drop = FALSE], soi_output)
  A_SoI2RoUS <- normalizeUseTable(ls$SoI2RoUS[, rous_ind, drop = FALSE], rous_output)
  A_RoUS2RoUS <- normalizeUseTable(ls$RoUS2RoUS[, rous_ind, drop = FALSE], rous_output)

  # Add suffixes to distinguish SoI from RoUS sectors
  # Column names: SoI industries get "_SoI", RoUS industries get "_RoUS"
  colnames(A_SoI2SoI) <- paste0(colnames(A_SoI2SoI), "_SoI")
  colnames(A_RoUS2SoI) <- paste0(colnames(A_RoUS2SoI), "_SoI")
  colnames(A_SoI2RoUS) <- paste0(colnames(A_SoI2RoUS), "_RoUS")
  colnames(A_RoUS2RoUS) <- paste0(colnames(A_RoUS2RoUS), "_RoUS")

  # Row names: commodities from SoI get "_SoI", from RoUS get "_RoUS"
  rownames(A_SoI2SoI) <- paste0(rownames(A_SoI2SoI), "_SoI")
  rownames(A_SoI2RoUS) <- paste0(rownames(A_SoI2RoUS), "_SoI")
  rownames(A_RoUS2SoI) <- paste0(rownames(A_RoUS2SoI), "_RoUS")
  rownames(A_RoUS2RoUS) <- paste0(rownames(A_RoUS2RoUS), "_RoUS")

  # Assemble A matrix (two-region block structure)
  # Rows: SoI commodities, RoUS commodities
  # Cols: SoI industries, RoUS industries
  A <- rbind(
    cbind(A_SoI2SoI, A_SoI2RoUS),
    cbind(A_RoUS2SoI, A_RoUS2RoUS)
  )

  A[is.na(A) | is.infinite(A)] <- 0

  # Make A square by using common row/col names
  common <- intersect(rownames(A), colnames(A))
  if (length(common) > 0) {
    A <- A[common, common]
  }

  # Calculate Leontief inverse: L = (I - A)^(-1)
  I_mat <- diag(nrow(A))
  L <- tryCatch({
    solve(I_mat - A)
  }, error = function(e) {
    warning("Matrix near-singular, using regularization")
    solve(I_mat - A + diag(1e-10, nrow(A)))
  })
  rownames(L) <- rownames(A)
  colnames(L) <- colnames(A)

  # Track SoI sectors (those ending in "_SoI")
  soi_sectors <- grep("_SoI$", colnames(A), value = TRUE)

  result <- list(
    A = A,
    L = L,
    sectors = colnames(A),
    soi_sectors = soi_sectors,
    state = state,
    state_abbr = state_abbr,
    year = year
  )

  assign(cache_key, result, envir = leontief_cache)
  return(result)
}

#' Find the gambling sector column in the Leontief system
#' @param leontief_system Result from buildLeontiefSystem
#' @param region "SoI" for State of Interest
#' @return Column name for gambling sector
findGamblingSectorColumn <- function(leontief_system, region = "SoI") {
  # Look for sector 713 with the appropriate suffix
  pattern <- paste0("^713_", region, "$")
  cols <- grep(pattern, leontief_system$sectors, value = TRUE)
  if (length(cols) > 0) return(cols[1])

  warning("Sector 713 not found for region: ", region)
  return(NULL)
}

#' Clear the Leontief cache
clearLeontiefCache <- function() {
  rm(list = ls(envir = leontief_cache), envir = leontief_cache)
}
