# Detailed debug script
library(stateior)

state <- "Nevada"
year <- 2020
state_abbr <- "NV"

message("=== Loading raw data ===")
domestic_use_list <- loadStateIODataFile(paste0("TwoRegion_Summary_DomesticUsewithTrade_", year))
industry_output_all <- loadStateIODataFile(paste0("TwoRegion_Summary_IndustryOutput_", year))

ls <- domestic_use_list[[state]]
TwoRegionIndustryOutput <- industry_output_all[[state]]

message("\n=== Data structure ===")
message("ls is a list with components:")
print(names(ls))

message("\n=== SoI2SoI dimensions ===")
print(dim(ls[["SoI2SoI"]]))
message("First 10 row names:")
print(head(rownames(ls[["SoI2SoI"]]), 10))
message("First 10 col names:")
print(head(colnames(ls[["SoI2SoI"]]), 10))

message("\n=== Industry Output ===")
message("Length of TwoRegionIndustryOutput:")
print(length(TwoRegionIndustryOutput))
message("First 10 names:")
print(head(names(TwoRegionIndustryOutput), 10))
message("First 10 values:")
print(head(TwoRegionIndustryOutput, 10))

message("\n=== Filtering output by state ===")
SoI_Industry_Output <- TwoRegionIndustryOutput[endsWith(names(TwoRegionIndustryOutput), state_abbr)]
message("SoI output length:")
print(length(SoI_Industry_Output))
message("First 10 SoI output names:")
print(head(names(SoI_Industry_Output), 10))

message("\n=== Checking if col names match output names ===")
soi2soi_cols <- colnames(ls[["SoI2SoI"]])
message("Columns in SoI2SoI that are also in SoI_Industry_Output:")
matching <- soi2soi_cols[soi2soi_cols %in% names(SoI_Industry_Output)]
print(length(matching))
print(head(matching, 10))

message("\n=== Sample values from SoI2SoI ===")
message("Sum of SoI2SoI matrix:")
print(sum(ls[["SoI2SoI"]], na.rm = TRUE))
message("Non-zero elements:")
print(sum(ls[["SoI2SoI"]] != 0, na.rm = TRUE))

message("\n=== Check sector 713 ===")
cols_713 <- grep("^713/", colnames(ls[["SoI2SoI"]]), value = TRUE)
message("Columns containing 713:")
print(cols_713)
rows_713 <- grep("^713/", rownames(ls[["SoI2SoI"]]), value = TRUE)
message("Rows containing 713:")
print(rows_713)

if (length(cols_713) > 0) {
  message("\n=== Column 713 values in SoI2SoI ===")
  col_713 <- ls[["SoI2SoI"]][, cols_713[1]]
  message("Sum of column:")
  print(sum(col_713, na.rm = TRUE))
  message("Non-zero values in column:")
  print(sum(col_713 != 0, na.rm = TRUE))
}

message("\n=== Attempt normalization ===")
if (length(matching) > 0) {
  test_col <- matching[1]
  message(paste("Testing column:", test_col))
  message(paste("Output value:", SoI_Industry_Output[test_col]))
  message(paste("Use table column sum:", sum(ls[["SoI2SoI"]][, test_col], na.rm = TRUE)))

  if (SoI_Industry_Output[test_col] > 0) {
    normalized <- ls[["SoI2SoI"]][, test_col] / SoI_Industry_Output[test_col]
    message(paste("Normalized column sum:", sum(normalized, na.rm = TRUE)))
  }
}
