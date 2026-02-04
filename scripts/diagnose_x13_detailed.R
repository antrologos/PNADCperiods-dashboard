# ==============================================================================
# Detailed X-13 Diagnostic - What tables are available?
# ==============================================================================

library(data.table)
library(seasonal)

cat("=== Detailed X-13 Diagnostic ===\n\n")

setwd("d:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods-dashboard")

# Load data
monthly_sidra <- readRDS("data/monthly_sidra.rds")

# Get dates and values
anomesexato <- monthly_sidra$anomesexato
dates <- as.Date(paste0(substr(anomesexato, 1, 4), "-",
                        substr(anomesexato, 5, 6), "-15"))
values <- monthly_sidra$m_taxapartic

# Order by date
ord <- order(dates)
values_ord <- values[ord]
dates_ord <- dates[ord]

# Split at 2015-10
split_date <- as.Date("2015-10-01")
split_idx <- which(dates_ord >= split_date)[1]

# ------------------------------------------------------------------------------
# Test Period 2 (post-2015) in detail
# ------------------------------------------------------------------------------

cat("Testing Period 2 (post-2015) in detail...\n\n")

idx2 <- split_idx:length(values_ord)
vals2 <- values_ord[idx2]
dates2 <- dates_ord[idx2]

# Clean NAs
vals2_clean <- vals2
na_idx <- which(is.na(vals2_clean))
if (length(na_idx) > 0 && length(na_idx) < length(vals2_clean)) {
  non_na_idx <- which(!is.na(vals2_clean))
  vals2_clean[na_idx] <- approx(non_na_idx, vals2_clean[non_na_idx], xout = na_idx)$y
}

# Create ts object
start_year <- as.numeric(format(dates2[1], "%Y"))
start_month <- as.numeric(format(dates2[1], "%m"))
ts2 <- ts(vals2_clean, frequency = 12, start = c(start_year, start_month))

cat("TS object info:\n")
cat("  Start:", start(ts2), "\n")
cat("  End:", end(ts2), "\n")
cat("  Frequency:", frequency(ts2), "\n")
cat("  Length:", length(ts2), "\n\n")

# Fit X-13 model
cat("Fitting X-13 model...\n")
m <- seas(ts2)

cat("\n1. Model summary:\n")
print(summary(m))

cat("\n2. ARIMA model:\n")
cat("  Model string:", m$model$arima$model, "\n")

cat("\n3. Available series (tables):\n")
# List all available series
available_tables <- tryCatch({
  # Get the list of available output tables
  out <- m$spc$series$save
  if (is.null(out)) {
    # Try to get from the x13 output directly
    out <- names(m$x13)
  }
  out
}, error = function(e) {
  paste("Error:", e$message)
})
cat("  ", paste(available_tables, collapse = ", "), "\n")

cat("\n4. Trying different seasonal component tables:\n")

# Try various table names
tables_to_try <- c("s10", "s11", "s12", "s13", "s14", "s16", "s18",
                   "d10", "d11", "d12", "d13",
                   "seats.seasonal", "seats.adjustfac",
                   "seasonal", "seasonaladj", "trend")

for (tbl in tables_to_try) {
  result <- tryCatch({
    s <- series(m, tbl)
    if (!is.null(s) && length(s) > 0) {
      paste("OK - length:", length(s), "variance:", round(var(as.numeric(s), na.rm = TRUE), 6))
    } else {
      "NULL or empty"
    }
  }, error = function(e) {
    paste("Error:", e$message)
  })
  cat(sprintf("  %-20s: %s\n", tbl, result))
}

cat("\n5. Check if model is seasonal:\n")
# Check the ARIMA specification
arima_spec <- m$model$arima
cat("  ARIMA spec:\n")
print(arima_spec)

cat("\n6. Check transform and method:\n")
cat("  Transform function:", m$spc$transform$function., "\n")
cat("  Seasonal method: ")
if (!is.null(m$spc$seats)) {
  cat("SEATS\n")
} else if (!is.null(m$spc$x11)) {
  cat("X-11\n")
} else {
  cat("Unknown\n")
}

cat("\n7. Final adjusted series:\n")
final_adj <- final(m)
cat("  Length:", length(final_adj), "\n")
cat("  Variance:", round(var(as.numeric(final_adj), na.rm = TRUE), 4), "\n")
cat("  First 6 values:", paste(round(head(as.numeric(final_adj), 6), 2), collapse = ", "), "\n")
cat("  Original first 6:", paste(round(head(vals2_clean, 6), 2), collapse = ", "), "\n")

# Check if adjusted == original
diff_adj <- as.numeric(final_adj) - vals2_clean
cat("\n8. Difference (adjusted - original):\n")
cat("  Mean difference:", round(mean(diff_adj, na.rm = TRUE), 6), "\n")
cat("  Max abs difference:", round(max(abs(diff_adj), na.rm = TRUE), 6), "\n")

if (max(abs(diff_adj), na.rm = TRUE) < 0.01) {
  cat("\n  *** ISSUE: Adjusted is nearly identical to original! ***\n")
  cat("  This suggests X-13 is NOT applying seasonal adjustment.\n")
}

cat("\n=== End Detailed Diagnostic ===\n")
