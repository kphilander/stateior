# leontief_engine.R
# Functions to construct A matrix and calculate Leontief inverse for I-O analysis

library(Matrix)

# Cache for Leontief matrices
leontief_cache <- new.env(parent = emptyenv())

#' Get state abbreviation from state name
getStateAbbr <- function(state) {
  if (state == "District of Columbia") return("DC")
  state.abb[match(state, state.name)]
}

#' Construct the direct requirements (A) matrix from two-region data
#' @param io_data List with DomesticUsewithTrade and IndustryOutput
#' @param state_abbr State abbreviation (e.g., "NV")
#' @return A matrix (direct requirements)
constructAMatrix <- function(io_data, state_abbr) {
  use_list <- io_data$DomesticUsewithTrade
  output <- io_data$IndustryOutput

  # Separate SoI and RoUS output vectors
  soi_pattern <- paste0("/US-", state_abbr, "$")
  soi_output <- output[grepl(soi_pattern, names(output))]
  rous_output <- output[grepl("/RoUS$", names(output))]

  # Get industry columns (exclude final demand F0xx, T0xx)
  get_industry_cols <- function(mat) {
    cols <- colnames(mat)
    sector_codes <- gsub("/.*", "", cols)
    cols[!grepl("^F0|^T0", sector_codes)]
  }

  # Normalize use table by output
  normalize <- function(use_mat, output_vec) {
    ind_cols <- get_industry_cols(use_mat)
    if (length(ind_cols) == 0) return(matrix(0, nrow = nrow(use_mat), ncol = 0))
    use_mat <- as.matrix(use_mat[, ind_cols, drop = FALSE])
    for (j in seq_len(ncol(use_mat))) {
      col_name <- colnames(use_mat)[j]
      out_val <- output_vec[col_name]
      if (!is.null(out_val) && !is.na(out_val) && out_val > 0) {
        use_mat[, j] <- use_mat[, j] / out_val
      } else {
        use_mat[, j] <- 0
      }
    }
    use_mat[is.na(use_mat) | is.infinite(use_mat)] <- 0
    use_mat
  }

  # Build A matrix blocks from each regional pairing
  A_SoI2SoI <- normalize(use_list$SoI2SoI, soi_output)
  A_RoUS2SoI <- normalize(use_list$RoUS2SoI, soi_output)
  A_SoI2RoUS <- normalize(use_list$SoI2RoUS, rous_output)
  A_RoUS2RoUS <- normalize(use_list$RoUS2RoUS, rous_output)

  # Assemble block matrix
  # [SoI commodities to SoI industries | SoI commodities to RoUS industries]
  # [RoUS commodities to SoI industries | RoUS commodities to RoUS industries]
  A <- rbind(
    cbind(A_SoI2SoI, A_SoI2RoUS),
    cbind(A_RoUS2SoI, A_RoUS2RoUS)
  )

  A[is.na(A) | is.infinite(A)] <- 0
  return(A)
}

#' Calculate the Leontief inverse matrix
#' @param A Direct requirements matrix
#' @return L matrix = (I - A)^(-1)
calculateLeontiefInverse <- function(A) {
  # Make A square if needed (use common dimensions)
  if (nrow(A) != ncol(A)) {
    common_names <- intersect(rownames(A), colnames(A))
    if (length(common_names) > 0) {
      A <- A[common_names, common_names]
    } else {
      min_dim <- min(nrow(A), ncol(A))
      A <- A[1:min_dim, 1:min_dim]
    }
  }

  # Identity matrix with same row/col names
  I <- diag(nrow(A))
  rownames(I) <- rownames(A)
  colnames(I) <- colnames(A)

  # Calculate Leontief inverse: L = (I - A)^(-1)
  tryCatch({
    L <- solve(I - A, tol = 1e-20)
    L
  }, error = function(e) {
    # If matrix is singular, use regularization
    warning("Matrix near-singular, using regularization")
    solve(I - A + diag(1e-10, nrow(A)))
  })
}

#' Build complete Leontief system for a state
#' @param state State name
#' @param year Data year
#' @return List with A matrix, L matrix, sector info
buildLeontiefSystem <- function(state, year) {
  cache_key <- paste("leontief", state, year, sep = "_")

  if (exists(cache_key, envir = leontief_cache)) {
    return(get(cache_key, envir = leontief_cache))
  }

  # Get state abbreviation
  state_abbr <- getStateAbbr(state)

  # Load I-O data
  io_data <- loadTwoRegionIOData(state, year)

  # Construct A matrix using two-region structure
  A <- constructAMatrix(io_data, state_abbr)

  # Calculate Leontief inverse
  L <- calculateLeontiefInverse(A)

  # Get sector mappings
  sectors <- colnames(A)
  soi_sectors <- sectors[grep("/US-", sectors)]  # State of Interest
  rous_sectors <- sectors[grep("/RoUS", sectors)]  # Rest of US

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

  # Cache result
  assign(cache_key, result, envir = leontief_cache)

  return(result)
}

#' Get the gambling industry sector code
#' @param year Data year (affects schema)
#' @return BEA sector code for gambling/amusements
getGamblingSectorCode <- function(year = 2020) {
  # BEA Summary code for "Amusements, gambling, and recreation industries"
  # Same in both 2012 and 2017 schemas
  return("713")
}

#' Find the gambling sector column in the Leontief system
#' @param leontief_system Result from buildLeontiefSystem
#' @param region "SoI" for State of Interest, "RoUS" for Rest of US
#' @return Column name/index for gambling sector
findGamblingSectorColumn <- function(leontief_system, region = "SoI") {
  gambling_code <- getGamblingSectorCode(leontief_system$year)

  if (region == "SoI") {
    # Match "713/US-XX" pattern for State of Interest
    pattern <- paste0("^", gambling_code, "/US-", leontief_system$state_abbr)
    cols <- grep(pattern, leontief_system$sectors, value = TRUE)
  } else {
    pattern <- paste0("^", gambling_code, "/RoUS")
    cols <- grep(pattern, leontief_system$sectors, value = TRUE)
  }

  if (length(cols) > 0) {
    return(cols[1])
  }

  # Fallback: try broader search
  cols <- grep(gambling_code, leontief_system$sectors, value = TRUE)
  if (length(cols) > 0) {
    return(cols[1])
  }

  warning(paste("Gambling sector", gambling_code, "not found in Leontief system"))
  return(NULL)
}

#' Clear the Leontief cache
clearLeontiefCache <- function() {
  rm(list = ls(envir = leontief_cache), envir = leontief_cache)
}
