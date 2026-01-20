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
#' Use table columns are like "111CA", "713" (no suffix)
#' Output vector names are like "111CA/US-NV", "713/US-NV" (with suffix)
#' @param use_mat Use table matrix
#' @param output_vec Industry output vector (with location suffixes)
#' @return Normalized A matrix block
normalizeUseTable <- function(use_mat, output_vec) {
  use_mat <- as.matrix(use_mat)

  # Strip location suffix from output names to match Use table columns
  # e.g., "111CA/US-NV" -> "111CA", "713/RoUS" -> "713"
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
#' @param col_names Column names from Use table
#' @return Industry column names only
getIndustryCols <- function(col_names) {
  col_names[!grepl("^F0|^T0", col_names)]
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

  # Handle zero outputs (set to 1 to avoid division by zero)
  soi_output[soi_output == 0] <- 1
  rous_output[rous_output == 0] <- 1

  # Get industry columns from each block (exclude final demand)
  # Use INTERSECTION because column counts may differ between blocks
  soi2soi_ind <- getIndustryCols(colnames(ls$SoI2SoI))
  rous2soi_ind <- getIndustryCols(colnames(ls$RoUS2SoI))
  soi2rous_ind <- getIndustryCols(colnames(ls$SoI2RoUS))
  rous2rous_ind <- getIndustryCols(colnames(ls$RoUS2RoUS))

  # Use common columns for SoI industries (SoI2SoI and RoUS2SoI blocks)
  soi_ind <- intersect(soi2soi_ind, rous2soi_ind)
  # Use common columns for RoUS industries (SoI2RoUS and RoUS2RoUS blocks)
  rous_ind <- intersect(soi2rous_ind, rous2rous_ind)

  # Normalize each block of the Use table
  A_SoI2SoI <- normalizeUseTable(ls$SoI2SoI[, soi_ind, drop = FALSE], soi_output)
  A_RoUS2SoI <- normalizeUseTable(ls$RoUS2SoI[, soi_ind, drop = FALSE], soi_output)
  A_SoI2RoUS <- normalizeUseTable(ls$SoI2RoUS[, rous_ind, drop = FALSE], rous_output)
  A_RoUS2RoUS <- normalizeUseTable(ls$RoUS2RoUS[, rous_ind, drop = FALSE], rous_output)

  # Assemble A matrix (two-region block structure)
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
  I <- diag(nrow(A))
  L <- tryCatch({
    solve(I - A)
  }, error = function(e) {
    warning("Matrix near-singular, using regularization")
    solve(I - A + diag(1e-10, nrow(A)))
  })
  rownames(L) <- rownames(A)
  colnames(L) <- colnames(A)

  # Track SoI sectors (first block)
  soi_sectors <- colnames(A)[seq_len(length(soi_ind))]

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
#' Sector 713 in Use table is just "713" (no suffix)
#' @param leontief_system Result from buildLeontiefSystem
#' @param region "SoI" for State of Interest (not used, kept for compatibility)
#' @return Column name for gambling sector
findGamblingSectorColumn <- function(leontief_system, region = "SoI") {
  # Sector 713 in the Use table is just "713" (no location suffix)
  cols <- grep("^713$", leontief_system$sectors, value = TRUE)
  if (length(cols) > 0) return(cols[1])

  warning("Sector 713 not found")
  return(NULL)
}

#' Clear the Leontief cache
clearLeontiefCache <- function() {
  rm(list = ls(envir = leontief_cache), envir = leontief_cache)
}
