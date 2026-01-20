# stateior Error Report

Generated: 2026-01-20
Status: **FIXED**

## Overview

This report documents errors found during a code review of the stateior R package.
All errors have been fixed in this commit.

## Fixed Errors

### 1. Undefined Variable: `schema` in IOFunctions.R

**File:** `R/IOFunctions.R`
**Line:** 59
**Function:** `calculateUSInternationalTransportMarginsRatioMatrix()`

**Issue:** The variable `schema` was used but not defined within the function scope.

**Fix:** Added `schema <- specs$BaseIOSchema` at the start of the function.

---

### 2. Undefined Variable: `BEA_col` in StateSupplyFunctions.R

**File:** `R/StateSupplyFunctions.R`
**Line:** 346-347
**Function:** `getStateEmploymentTable()`

**Issue:** The variable `BEA_col` was used but not defined.

**Fix:** Added `BEA_col <- paste0("BEA_", schema, "_Summary_Code")` after schema definition.

---

### 3. Missing Parameter: `specs` in StateUseFunctions.R

**File:** `R/StateUseFunctions.R`
**Line:** 137
**Function:** `adjustGVAComponent()`

**Issue:** The function used `specs` internally but didn't accept it as a parameter.

**Fix:** Added `specs` parameter to function signature and updated all call sites.

---

### 4. Missing Argument in InteregionalCommodityFlowFunctions.R

**File:** `R/InteregionalCommodityFlowFunctions.R`
**Line:** 190
**Function:** `generateDomestic2RegionICFs()`

**Issue:** `calculateElectricityFlowRatios()` was called without required `specs` argument.

**Fix:** Added `specs` argument to the function call.

---

### 5. Undefined Variable: `matrix` in WriteModel.R

**File:** `R/WriteModel.R`
**Line:** 14
**Function:** `writeStateIODatatoCSV()`

**Issue:** The variable `matrix` was used but not defined.

**Fix:** Changed `matrix` to `filename` and fixed the file path construction.

---

### 6. Missing Parameter: `specs` in StateUseFunctions.R

**File:** `R/StateUseFunctions.R`
**Lines:** 294, 296
**Function:** `calculateStateTotalPCE()`

**Issue:** `getStatePCE()` was called without required `specs` argument.

**Fix:** Added `specs` parameter to function signature and updated the call to `getStatePCE()`.

---

### 7. Undefined Variable: `year` in data-raw scripts

**Files:** `data-raw/StateSupplyModel.R`, `data-raw/StateUseModel.R`, `data-raw/TwoRegionModel.R`

**Issue:** The variable `year` was used but never defined.

**Fix:** Added `year` variable definition with default value and comments. Also fixed `specs` initialization from `{}` to `list()`.

---

## Summary

| Severity | Count | Status |
|----------|-------|--------|
| Critical (undefined variables) | 6 | Fixed |
| Critical (missing function arguments) | 2 | Fixed |
| **Total** | **8** | **All Fixed** |
