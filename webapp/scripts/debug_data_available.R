# Check what stateior data is available
library(stateior)

year <- 2020
state <- "Nevada"

message("=== Checking available stateior data ===\n")

# Check DomesticUsewithTrade (we know this works)
message("1. TwoRegion_Summary_DomesticUsewithTrade:")
tryCatch({
  data <- loadStateIODataFile(paste0("TwoRegion_Summary_DomesticUsewithTrade_", year))
  message("   SUCCESS - ", length(names(data)), " states")
}, error = function(e) message("   FAILED: ", e$message))

# Check IndustryOutput (we know this works)
message("\n2. TwoRegion_Summary_IndustryOutput:")
tryCatch({
  data <- loadStateIODataFile(paste0("TwoRegion_Summary_IndustryOutput_", year))
  message("   SUCCESS - ", length(names(data)), " states")
}, error = function(e) message("   FAILED: ", e$message))

# Check Make tables (needed for full model)
message("\n3. TwoRegion_Summary_Make:")
tryCatch({
  data <- loadStateIODataFile(paste0("TwoRegion_Summary_Make_", year))
  message("   SUCCESS - ", length(names(data)), " states")
  message("   Structure for Nevada:")
  nv <- data[[state]]
  message("   Rows: ", nrow(nv), ", Cols: ", ncol(nv))
  message("   First 5 row names: ", paste(head(rownames(nv), 5), collapse=", "))
  message("   First 5 col names: ", paste(head(colnames(nv), 5), collapse=", "))
}, error = function(e) message("   FAILED: ", e$message))

# Check CommodityOutput (needed for full model)
message("\n4. TwoRegion_Summary_CommodityOutput:")
tryCatch({
  data <- loadStateIODataFile(paste0("TwoRegion_Summary_CommodityOutput_", year))
  message("   SUCCESS - ", length(names(data)), " states")
  message("   Nevada length: ", length(data[[state]]))
}, error = function(e) message("   FAILED: ", e$message))

# Check ValueAdded (for compensation/income)
message("\n5. TwoRegion_Summary_ValueAdded:")
tryCatch({
  data <- loadStateIODataFile(paste0("TwoRegion_Summary_ValueAdded_", year))
  message("   SUCCESS - ", length(names(data)), " states")
}, error = function(e) message("   FAILED: ", e$message))

message("\n=== Now test the actual multiplier calculation ===\n")

source("../R/io_data_loader.R")
source("../R/leontief_engine.R")
source("../R/multiplier_calc.R")

message("Building Leontief system...")
leontief <- buildLeontiefSystem(state, year)

message("Finding gambling sector (713_SoI)...")
sector_col <- findGamblingSectorColumn(leontief, "SoI")
message("  Result: ", sector_col)

if (!is.null(sector_col)) {
  col_idx <- which(colnames(leontief$L) == sector_col)
  message("  Column index: ", col_idx)

  message("\nCalculating multipliers:")
  soi_rows <- leontief$soi_sectors
  message("  Number of SoI rows: ", length(soi_rows))
  message("  713_SoI in soi_rows: ", "713_SoI" %in% soi_rows)

  # Check if soi_rows exist in L
  valid_rows <- soi_rows[soi_rows %in% rownames(leontief$L)]
  message("  Valid SoI rows in L: ", length(valid_rows))

  if (length(col_idx) > 0 && length(valid_rows) > 0) {
    soi_sum <- sum(leontief$L[valid_rows, col_idx], na.rm = TRUE)
    full_sum <- sum(leontief$L[, col_idx], na.rm = TRUE)
    message("\n  SoI-only output multiplier (Type I): ", round(soi_sum, 4))
    message("  Full column output multiplier: ", round(full_sum, 4))
    message("  Type II (x1.35): ", round(soi_sum * 1.35, 4))
  }
}
