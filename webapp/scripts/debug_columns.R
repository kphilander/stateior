# Debug column names in each matrix block
library(stateior)

state <- "Nevada"
year <- 2020

domestic_use_list <- loadStateIODataFile(paste0("TwoRegion_Summary_DomesticUsewithTrade_", year))
ls <- domestic_use_list[[state]]

message("=== Column names in each block ===")
message("\nSoI2SoI columns (first 10):")
print(head(colnames(ls$SoI2SoI), 10))

message("\nRoUS2SoI columns (first 10):")
print(head(colnames(ls$RoUS2SoI), 10))

message("\nSoI2RoUS columns (first 10):")
print(head(colnames(ls$SoI2RoUS), 10))

message("\nRoUS2RoUS columns (first 10):")
print(head(colnames(ls$RoUS2RoUS), 10))

message("\n=== Row names in each block ===")
message("\nSoI2SoI rows (first 10):")
print(head(rownames(ls$SoI2SoI), 10))

message("\nRoUS2SoI rows (first 10):")
print(head(rownames(ls$RoUS2SoI), 10))

message("\n=== Dimensions ===")
message(paste("SoI2SoI:", nrow(ls$SoI2SoI), "x", ncol(ls$SoI2SoI)))
message(paste("RoUS2SoI:", nrow(ls$RoUS2SoI), "x", ncol(ls$RoUS2SoI)))
message(paste("SoI2RoUS:", nrow(ls$SoI2RoUS), "x", ncol(ls$SoI2RoUS)))
message(paste("RoUS2RoUS:", nrow(ls$RoUS2RoUS), "x", ncol(ls$RoUS2RoUS)))

message("\n=== Are SoI columns same in SoI2SoI and RoUS2SoI? ===")
print(identical(colnames(ls$SoI2SoI), colnames(ls$RoUS2SoI)))

message("\n=== Are RoUS columns same in SoI2RoUS and RoUS2RoUS? ===")
print(identical(colnames(ls$SoI2RoUS), colnames(ls$RoUS2RoUS)))
