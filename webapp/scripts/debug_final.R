# Final Debug Script - Verify Leontief Implementation
# Tests the suffix-based two-region structure

library(stateior)

# Source the webapp modules
source("../R/io_data_loader.R")
source("../R/leontief_engine.R")
source("../R/multiplier_calc.R")

year <- 2020
state <- "Nevada"

message("==============================================")
message("DEBUG: Leontief System for ", state, " (", year, ")")
message("==============================================\n")

# Step 1: Build the Leontief system
message("STEP 1: Building Leontief System...")
leontief <- buildLeontiefSystem(state, year)

message("  L matrix dimensions: ", nrow(leontief$L), " x ", ncol(leontief$L))
message("  Total sectors: ", length(leontief$sectors))
message("  SoI sectors: ", length(leontief$soi_sectors))

# Step 2: Check sector naming
message("\nSTEP 2: Checking Sector Names...")
message("  First 5 sectors: ", paste(head(leontief$sectors, 5), collapse=", "))
message("  Last 5 sectors: ", paste(tail(leontief$sectors, 5), collapse=", "))

soi_count <- sum(grepl("_SoI$", leontief$sectors))
rous_count <- sum(grepl("_RoUS$", leontief$sectors))
message("  Sectors ending in '_SoI': ", soi_count)
message("  Sectors ending in '_RoUS': ", rous_count)

# Step 3: Find sector 713 (gambling)
message("\nSTEP 3: Finding Gambling Sector (713)...")

# Look for 713_SoI
sector_713_soi <- grep("^713_SoI$", leontief$sectors, value = TRUE)
sector_713_rous <- grep("^713_RoUS$", leontief$sectors, value = TRUE)

message("  713_SoI found: ", length(sector_713_soi) > 0, " -> '", sector_713_soi, "'")
message("  713_RoUS found: ", length(sector_713_rous) > 0, " -> '", sector_713_rous, "'")

# Also check what findGamblingSectorColumn returns
sector_col <- findGamblingSectorColumn(leontief, "SoI")
message("  findGamblingSectorColumn() returns: '", sector_col, "'")

# Step 4: Calculate multipliers
message("\nSTEP 4: Calculating Output Multipliers...")

if (!is.null(sector_col) && sector_col %in% colnames(leontief$L)) {
  col_idx <- which(colnames(leontief$L) == sector_col)
  message("  Column index for ", sector_col, ": ", col_idx)

  # Get column values
  L_col <- leontief$L[, col_idx]

  # Full column sum (all regions)
  full_sum <- sum(L_col, na.rm = TRUE)
  message("\n  FULL column sum (SoI + RoUS): ", round(full_sum, 4))

  # SoI-only sum (state-level effect)
  soi_rows <- leontief$soi_sectors
  valid_soi_rows <- soi_rows[soi_rows %in% rownames(leontief$L)]
  soi_sum <- sum(leontief$L[valid_soi_rows, col_idx], na.rm = TRUE)
  message("  SoI-only sum (state effect): ", round(soi_sum, 4))

  # RoUS-only sum (spillover to other states)
  rous_rows <- grep("_RoUS$", rownames(leontief$L), value = TRUE)
  rous_sum <- sum(leontief$L[rous_rows, col_idx], na.rm = TRUE)
  message("  RoUS-only sum (spillover): ", round(rous_sum, 4))

  message("\n  --- Multiplier Interpretation ---")
  message("  Type I Output Multiplier (SoI only): ", round(soi_sum, 4))
  message("  Type II Estimate (x1.35): ", round(soi_sum * 1.35, 4))
  message("  Full System Multiplier: ", round(full_sum, 4))

  # Show top contributing sectors
  message("\n  Top 5 sectors in L column (by coefficient):")
  L_col_sorted <- sort(L_col, decreasing = TRUE)
  for (i in 1:min(5, length(L_col_sorted))) {
    message("    ", names(L_col_sorted)[i], ": ", round(L_col_sorted[i], 4))
  }

  # Check diagonal element
  diag_val <- leontief$L[sector_col, sector_col]
  message("\n  Diagonal element L[713_SoI, 713_SoI]: ", round(diag_val, 4))
  message("  (Should be >= 1.0, representing direct effect)")

} else {
  message("  ERROR: Could not find sector 713_SoI in L matrix!")
  message("  Available sectors containing '713':")
  print(grep("713", leontief$sectors, value = TRUE))
}

# Step 5: Sanity checks
message("\nSTEP 5: Sanity Checks...")

# Check A matrix properties
A_max <- max(abs(leontief$A), na.rm = TRUE)
A_col_sums <- colSums(leontief$A, na.rm = TRUE)
message("  Max A matrix value: ", round(A_max, 4), " (should be < 1)")
message("  Max A column sum: ", round(max(A_col_sums), 4), " (should be < 1)")

# Check L matrix diagonal
L_diag <- diag(leontief$L)
message("  Min L diagonal: ", round(min(L_diag), 4), " (should be >= 1)")
message("  Max L diagonal: ", round(max(L_diag), 4))

# Step 6: Compare states
message("\n==============================================")
message("STEP 6: Compare Multiple States")
message("==============================================\n")

states_to_test <- c("Nevada", "New Jersey", "California", "Wyoming")

for (st in states_to_test) {
  tryCatch({
    leon <- buildLeontiefSystem(st, year)
    sector <- findGamblingSectorColumn(leon, "SoI")

    if (!is.null(sector) && sector %in% colnames(leon$L)) {
      col_idx <- which(colnames(leon$L) == sector)
      soi_rows <- leon$soi_sectors
      valid_soi <- soi_rows[soi_rows %in% rownames(leon$L)]

      soi_mult <- sum(leon$L[valid_soi, col_idx], na.rm = TRUE)
      full_mult <- sum(leon$L[, col_idx], na.rm = TRUE)

      message(sprintf("  %-20s Type I: %.4f  Type II (est): %.4f  Full: %.4f",
                      st, soi_mult, soi_mult * 1.35, full_mult))
    } else {
      message("  ", st, ": Sector 713 not found")
    }
  }, error = function(e) {
    message("  ", st, ": ERROR - ", e$message)
  })
}

message("\n==============================================")
message("Expected Results:")
message("  - Type I multipliers should range from ~1.2 to ~1.8")
message("  - Nevada should have relatively higher multiplier")
message("  - States should show meaningful variation (10-30%)")
message("==============================================")
