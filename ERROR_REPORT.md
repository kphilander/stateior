# stateior Error Report

Generated: 2026-01-20

## Overview

This report documents errors found during a code review of the stateior R package.

## Critical Errors

### 1. Undefined Variable: `schema` in IOFunctions.R

**File:** `R/IOFunctions.R`
**Line:** 59
**Function:** `calculateUSInternationalTransportMarginsRatioMatrix()`

**Issue:** The variable `schema` is used but not defined within the function scope.

```r
# Line 59 - Current (broken)
US_Import <- loadDatafromUSEEIOR(paste(iolevel, "Import", year, "BeforeRedef",
                                       paste0(substr(schema, 3, 4), "sch"),
                                       sep = "_"))*1E6
```

**Fix:** Add `schema <- specs$BaseIOSchema` at the start of the function.

---

### 2. Undefined Variable: `BEA_col` in StateSupplyFunctions.R

**File:** `R/StateSupplyFunctions.R`
**Line:** 346-347
**Function:** `getStateEmploymentTable()`

**Issue:** The variable `BEA_col` is used but not defined.

```r
# Lines 346-347 - Current (broken)
BEAStateEmp <- stats::aggregate(BEAStateEmp[, as.character(year)],
                                by = list(BEAStateEmp[[BEA_col]],
                                          BEAStateEmp$GeoName), sum)
```

**Fix:** Add `BEA_col <- paste0("BEA_", schema, "_Summary_Code")` before line 346.

---

### 3. Undefined Variable: `specs` in StateUseFunctions.R

**File:** `R/StateUseFunctions.R`
**Line:** 139
**Function:** `adjustGVAComponent()`

**Issue:** The function uses `specs` internally but doesn't accept it as a parameter.

```r
# Line 139 - Current (broken)
gva <- getStateGVA(year, specs)
```

**Fix:** Add `specs` parameter to function signature: `adjustGVAComponent <- function(year, return, specs)`

---

### 4. Missing `specs` Argument in InteregionalCommodityFlowFunctions.R

**File:** `R/InteregionalCommodityFlowFunctions.R`
**Line:** 190
**Function:** `generateDomestic2RegionICFs()`

**Issue:** `calculateElectricityFlowRatios()` is called without required `specs` argument.

```r
# Line 190 - Current (broken)
ICF[ICF[, bea] == "221100", cols] <- calculateElectricityFlowRatios(state, year)[, cols]
```

**Fix:** Change to `calculateElectricityFlowRatios(state, year, specs)[, cols]`

---

### 5. Undefined Variable: `matrix` in WriteModel.R

**File:** `R/WriteModel.R`
**Line:** 14
**Function:** `writeStateIODatatoCSV()`

**Issue:** The variable `matrix` is used but not defined.

```r
# Line 14 - Current (broken)
utils::write.csv(data, file.path(outputfolder, "/", matrix, ".csv"),
                 na = "", row.names = TRUE, fileEncoding = "UTF-8")
```

**Fix:** Change `matrix` to `filename`.

---

### 6. Missing `specs` Argument in StateUseFunctions.R

**File:** `R/StateUseFunctions.R`
**Lines:** 295, 300-301
**Function:** `calculateStateTotalPCE()`

**Issue:** `getStatePCE()` is called without required `specs` argument.

```r
# Line 295 - Current (broken)
PCE <- getStatePCE(year)
```

**Fix:** Add `specs` parameter to function and pass to `getStatePCE(year, specs)`.

---

### 7. Undefined Variable: `year` in data-raw scripts

**Files:** `data-raw/StateSupplyModel.R`, `data-raw/StateUseModel.R`
**Line:** 7 (both files)

**Issue:** The variable `year` is used but never defined.

```r
# Line 7 - Current (broken)
StateSupplyModel <- buildStateSupplyModel(year, specs)
```

**Fix:** Define `year` variable before use, e.g., `year <- 2017`.

---

## Summary

| Severity | Count |
|----------|-------|
| Critical (undefined variables) | 6 |
| Critical (missing function arguments) | 2 |
| **Total** | **8** |

These errors would cause runtime failures when the affected functions are called.
