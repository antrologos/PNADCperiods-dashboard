# ==============================================================================
# Debug X-11 in App Context - Check actual seasonal variance
# ==============================================================================

library(data.table)
library(seasonal)

cat("=== Debug X-11 Seasonal Variance ===\n\n")

setwd("d:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods-dashboard")

# Load data exactly as app does
monthly_sidra <- readRDS("data/monthly_sidra.rds")

# Get taxapartic - same as the screenshot
anomesexato <- monthly_sidra$anomesexato
dates <- as.Date(paste0(substr(anomesexato, 1, 4), "-",
                        substr(anomesexato, 5, 6), "-15"))
values <- monthly_sidra$m_taxapartic

cat("Data loaded:\n")
cat("  Observations:", length(values), "\n")
cat("  Date range:", as.character(min(dates)), "to", as.character(max(dates)), "\n")
cat("  Value range:", round(min(values, na.rm = TRUE), 2), "to",
    round(max(values, na.rm = TRUE), 2), "\n")
cat("  Original variance:", round(var(values, na.rm = TRUE), 4), "\n\n")

# Order by date (as deseasonalize_x13 does)
ord <- order(dates)
values_ord <- values[ord]
dates_ord <- dates[ord]

# Split at 2015-10-01 (as the function does)
split_date <- as.Date("2015-10-01")
split_idx <- which(dates_ord >= split_date)[1]

cat("Split info:\n")
cat("  Split date:", as.character(split_date), "\n")
cat("  Split index:", split_idx, "\n\n")

# Function to test a period
test_period <- function(vals, dates_period, period_name) {
  cat("=== ", period_name, " ===\n")
  cat("  Observations:", length(vals), "\n")
  cat("  Date range:", as.character(min(dates_period)), "to",
      as.character(max(dates_period)), "\n")
  cat("  Value range:", round(min(vals, na.rm = TRUE), 2), "to",
      round(max(vals, na.rm = TRUE), 2), "\n")

  vals_var <- var(vals, na.rm = TRUE)
  cat("  Original variance:", round(vals_var, 4), "\n")

  # Clean NAs
  vals_clean <- vals
  na_idx <- which(is.na(vals_clean))
  if (length(na_idx) > 0 && length(na_idx) < length(vals_clean)) {
    non_na_idx <- which(!is.na(vals_clean))
    vals_clean[na_idx] <- approx(non_na_idx, vals_clean[non_na_idx], xout = na_idx)$y
  }

  # Create ts
  start_year <- as.numeric(format(dates_period[1], "%Y"))
  start_month <- as.numeric(format(dates_period[1], "%m"))
  ts_obj <- ts(vals_clean, frequency = 12, start = c(start_year, start_month))

  cat("  TS start:", start(ts_obj), "\n")

  # Fit X-11
  cat("  Fitting X-13 with X-11...\n")
  m <- seas(ts_obj, x11 = "")

  # Get seasonal component
  d10 <- series(m, "d10")
  d10_var <- var(as.numeric(d10), na.rm = TRUE)

  cat("  d10 variance:", format(d10_var, scientific = TRUE), "\n")
  cat("  d10 range:", round(diff(range(as.numeric(d10), na.rm = TRUE)), 4), "\n")
  cat("  d10 sample:", paste(round(head(as.numeric(d10), 6), 4), collapse = ", "), "\n")

  # Check ratio
  ratio <- d10_var / vals_var
  cat("  Seasonal/Original variance ratio:", format(ratio * 100, digits = 4), "%\n")

  # What the validation would say
  if (d10_var < 1e-10) {
    cat("  VALIDATION: Would flag as FLAT (variance near zero)\n")
  } else if (ratio < 0.001) {
    cat("  VALIDATION: Would flag as TRIVIAL (< 0.1% of original)\n")
  } else {
    cat("  VALIDATION: Would PASS\n")
  }

  cat("\n")

  return(list(d10_var = d10_var, vals_var = vals_var, ratio = ratio))
}

# Test both periods
cat("\n")
results1 <- test_period(values_ord[1:(split_idx-1)], dates_ord[1:(split_idx-1)], "PERIOD 1 (Pre-2015)")
results2 <- test_period(values_ord[split_idx:length(values_ord)], dates_ord[split_idx:length(dates_ord)], "PERIOD 2 (Post-2015)")

cat("=== Summary ===\n")
cat("Period 1 ratio:", format(results1$ratio * 100, digits = 4), "%\n")
cat("Period 2 ratio:", format(results2$ratio * 100, digits = 4), "%\n")
cat("\nCurrent threshold: 0.1%\n")
cat("If threshold were 0.01%:\n")
cat("  Period 1:", if(results1$ratio < 0.0001) "FAIL" else "PASS", "\n")
cat("  Period 2:", if(results2$ratio < 0.0001) "FAIL" else "PASS", "\n")
