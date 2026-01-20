# Debug script to trace exactly what's happening in Leontief calculation
library(stateior)

source("../R/io_data_loader.R")
source("../R/leontief_engine.R")

state <- "Nevada"
year <- 2020

message("=== Building Leontief system for ", state, " ===\n")

# Manually trace through buildLeontiefSystem
state_abbr <- "NV"

domestic_use_list <- loadStateIODataFile(paste0("TwoRegion_Summary_DomesticUsewithTrade_", year))
industry_output_all <- loadStateIODataFile(paste0("TwoRegion_Summary_IndustryOutput_", year))

ls <- domestic_use_list[[state]]
output <- industry_output_all[[state]]

# Get industry columns
getIndustryCols <- function(col_names) {
  col_names[!grepl("^F0|^T0", col_names)]
}

soi2soi_ind <- getIndustryCols(colnames(ls$SoI2SoI))
rous2soi_ind <- getIndustryCols(colnames(ls$RoUS2SoI))
soi2rous_ind <- getIndustryCols(colnames(ls$SoI2RoUS))
rous2rous_ind <- getIndustryCols(colnames(ls$RoUS2RoUS))

soi_ind <- intersect(soi2soi_ind, rous2soi_ind)
rous_ind <- intersect(soi2rous_ind, rous2rous_ind)

message("soi_ind length: ", length(soi_ind))
message("rous_ind length: ", length(rous_ind))
message("First 5 soi_ind: ", paste(head(soi_ind, 5), collapse=", "))
message("First 5 rous_ind: ", paste(head(rous_ind, 5), collapse=", "))

# Check if 713 is in either
message("\n713 in soi_ind: ", "713" %in% soi_ind)
message("713 in rous_ind: ", "713" %in% rous_ind)

# Now call the actual function
leontief <- buildLeontiefSystem(state, year)

message("\n=== After buildLeontiefSystem ===")
message("L matrix dimensions: ", nrow(leontief$L), " x ", ncol(leontief$L))
message("Number of sectors: ", length(leontief$sectors))
message("Number of soi_sectors: ", length(leontief$soi_sectors))

message("\nFirst 10 sectors (colnames of L):")
print(head(leontief$sectors, 10))

message("\nFirst 10 soi_sectors:")
print(head(leontief$soi_sectors, 10))

message("\nFirst 10 rownames of L:")
print(head(rownames(leontief$L), 10))

message("\n713 in sectors: ", "713" %in% leontief$sectors)
message("713 in soi_sectors: ", "713" %in% leontief$soi_sectors)
message("713 in rownames(L): ", "713" %in% rownames(leontief$L))

# Find sector 713
col_713 <- which(colnames(leontief$L) == "713")
message("\nColumn index for 713: ", col_713)

if (length(col_713) > 0) {
  message("\n=== Testing L[soi_sectors, col_713] ===")
  message("soi_sectors that exist in rownames(L):")
  valid_rows <- leontief$soi_sectors[leontief$soi_sectors %in% rownames(leontief$L)]
  message("  Count: ", length(valid_rows))
  message("  First 5: ", paste(head(valid_rows, 5), collapse=", "))

  if (length(valid_rows) > 0) {
    test_sum <- sum(leontief$L[valid_rows, col_713], na.rm = TRUE)
    message("\nSum of L[valid_soi_rows, 713]: ", round(test_sum, 4))
  }

  # Also try full column sum for comparison
  full_sum <- sum(leontief$L[, col_713], na.rm = TRUE)
  message("Sum of L[, 713] (full column): ", round(full_sum, 4))
}
