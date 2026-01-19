# leontief_engine.R
# Functions to construct A matrix and calculate Leontief inverse for I-O analysis

library(Matrix)

# Cache for Leontief matrices
leontief_cache <- new.env(parent = emptyenv())

#' Extract the intermediate use portion of the Use table
#' @param use_table Full Use table (commodities x industries+final demand)
#' @return Matrix of intermediate use only (commodities x industries)
extractIntermediateUse <- function(use_table) {
  # Remove value added rows (V001, V002, V003, Used, Other)
  va_rows <- c("V001", "V002", "V003", "Used", "Other", "T00OTOP", "T00SUB", "T00TOP")
  use_clean <- use_table[!rownames(use_table) %in% va_rows, ]

  # Keep only industry columns (exclude final demand categories)
  # Final demand codes typically start with F or are specific codes

  fd_patterns <- c("^F0", "^T0", "^Used", "^Other")
  industry_cols <- colnames(use_clean)
  for (pattern in fd_patterns) {
    industry_cols <- industry_cols[!grepl(pattern, industry_cols)]
  }

  return(as.matrix(use_clean[, industry_cols]))
}

#' Construct the direct requirements (A) matrix
#' @param use_table Domestic Use table
#' @param industry_output Industry output vector
#' @return A matrix (commodities x industries)
constructAMatrix <- function(use_table, industry_output) {
  # Extract intermediate use
  intermediate_use <- extractIntermediateUse(use_table)

  # Get output values for each industry column
  output_vec <- numeric(ncol(intermediate_use))
  names(output_vec) <- colnames(intermediate_use)

  for (col in colnames(intermediate_use)) {
    # Match column to output row
    output_row <- grep(paste0("^", gsub("/.*", "", col)), rownames(industry_output), value = TRUE)
    if (length(output_row) > 0) {
      output_vec[col] <- industry_output[output_row[1], "Output"]
    }
  }

  # Calculate A matrix: A[i,j] = Use[i,j] / Output[j]
  A <- intermediate_use
  for (j in 1:ncol(A)) {
    if (output_vec[j] > 0) {
      A[, j] <- A[, j] / output_vec[j]
    } else {
      A[, j] <- 0
    }
  }

  # Replace NA/Inf with 0
  A[is.na(A) | is.infinite(A)] <- 0

  return(A)
}

#' Calculate the Leontief inverse matrix
#' @param A Direct requirements matrix
#' @return L matrix = (I - A)^(-1)
calculateLeontiefInverse <- function(A) {
  n <- ncol(A)

  # Make A square if needed (use common dimensions)
  if (nrow(A) != ncol(A)) {
    # Use the smaller dimension
    common_names <- intersect(rownames(A), colnames(A))
    if (length(common_names) > 0) {
      A <- A[common_names, common_names]
    } else {
      # Create square matrix from min dimension
      min_dim <- min(nrow(A), ncol(A))
      A <- A[1:min_dim, 1:min_dim]
    }
  }

  # Identity matrix
  I <- diag(nrow(A))

  # Calculate Leontief inverse: L = (I - A)^(-1)
  tryCatch({
    L <- solve(I - A, tol = 1e-20)
    return(L)
  }, error = function(e) {
    # If matrix is singular, use pseudoinverse
    warning("Matrix near-singular, using regularization")
    L <- solve(I - A + diag(1e-10, nrow(A)))
    return(L)
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

  # Load I-O data
  io_data <- loadTwoRegionIOData(state, year)

  # Construct A matrix
  A <- constructAMatrix(io_data$DomesticUse, io_data$IndustryOutput)

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
    pattern <- paste0("^", gambling_code, "/US-")
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
