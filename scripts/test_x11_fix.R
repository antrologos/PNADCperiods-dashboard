# ==============================================================================
# Test X-11 Method Fix
# ==============================================================================

library(data.table)
library(seasonal)

cat("=== Testing X-11 Method Fix ===\n\n")

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

# Test Period 2 (post-2015) with X-11 method
cat("Testing Period 2 (post-2015) with X-11 method...\n\n")

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

cat("TS object:\n")
cat("  Start:", start(ts2), "\n")
cat("  End:", end(ts2), "\n")
cat("  Length:", length(ts2), "\n\n")

# Fit with X-11 method (like Marcos)
cat("Fitting X-13 with X-11 method (x11 = '')...\n\n")
m <- seas(ts2, x11 = "")

cat("1. Model summary:\n")
print(summary(m))

cat("\n2. Check if X-11 is active:\n")
if (!is.null(m$spc$x11)) {
  cat("  X-11 method: ACTIVE\n")
} else {
  cat("  X-11 method: NOT ACTIVE\n")
}

cat("\n3. Extract d10 (X-11 seasonal factors):\n")
d10 <- tryCatch({
  seasonal::series(m, "d10")
}, error = function(e) {
  paste("Error:", e$message)
})

if (is.numeric(d10)) {
  cat("  d10 length:", length(d10), "\n")
  cat("  d10 variance:", round(var(as.numeric(d10), na.rm = TRUE), 6), "\n")
  cat("  d10 range:", round(diff(range(as.numeric(d10), na.rm = TRUE)), 4), "\n")
  cat("  d10 sample:", paste(round(head(as.numeric(d10), 6), 4), collapse = ", "), "\n")
} else {
  cat("  d10 result:", d10, "\n")
}

cat("\n4. Extract d11 (X-11 seasonally adjusted):\n")
d11 <- tryCatch({
  seasonal::series(m, "d11")
}, error = function(e) {
  paste("Error:", e$message)
})

if (is.numeric(d11)) {
  cat("  d11 length:", length(d11), "\n")
  cat("  d11 variance:", round(var(as.numeric(d11), na.rm = TRUE), 4), "\n")
} else {
  cat("  d11 result:", d11, "\n")
}

cat("\n5. Compare adjusted vs original:\n")
final_adj <- final(m)
diff_adj <- as.numeric(final_adj) - vals2_clean
cat("  Mean difference:", round(mean(diff_adj, na.rm = TRUE), 4), "\n")
cat("  Max abs difference:", round(max(abs(diff_adj), na.rm = TRUE), 4), "\n")
cat("  Original variance:", round(var(vals2_clean, na.rm = TRUE), 4), "\n")
cat("  Adjusted variance:", round(var(as.numeric(final_adj), na.rm = TRUE), 4), "\n")

if (max(abs(diff_adj), na.rm = TRUE) > 0.1) {
  cat("\n  SUCCESS: Adjusted differs from original - seasonal adjustment is working!\n")
} else {
  cat("\n  PROBLEM: Adjusted is too similar to original\n")
}

cat("\n=== Test Complete ===\n")
