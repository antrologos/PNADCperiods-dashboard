# ==============================================================================
# Diagnostic Script: Investigate X-13 ARIMA Seasonal Component Issue
# ==============================================================================
# This script investigates why X-13 produces flat seasonal components post-2015

library(data.table)

cat("=== X-13 ARIMA Diagnostic ===\n\n")

# Set working directory
setwd("d:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods-dashboard")

# ------------------------------------------------------------------------------
# 1. Check if data files exist
# ------------------------------------------------------------------------------

cat("1. Checking data files...\n")
data_files <- c(
  "data/monthly_sidra.rds",
  "data/rolling_quarters.rds",
  "data/series_metadata.rds",
  "data/deseasonalized_cache.rds"
)

for (f in data_files) {
  exists <- file.exists(f)
  size <- if (exists) file.size(f) else 0
  cat(sprintf("   %s: %s (%s bytes)\n", f, if (exists) "EXISTS" else "MISSING", format(size, big.mark = ",")))
}

# ------------------------------------------------------------------------------
# 2. Load and inspect monthly_sidra data
# ------------------------------------------------------------------------------

cat("\n2. Loading monthly_sidra.rds...\n")
monthly_sidra <- readRDS("data/monthly_sidra.rds")

cat("   Class:", class(monthly_sidra), "\n")
cat("   Rows:", nrow(monthly_sidra), "\n")
cat("   Columns:", ncol(monthly_sidra), "\n")
cat("   Date range:", min(monthly_sidra$anomesexato), "to", max(monthly_sidra$anomesexato), "\n")

# Check a specific series (taxapartic - labor force participation rate)
if ("m_taxapartic" %in% names(monthly_sidra)) {
  cat("\n   Sample series: m_taxapartic\n")
  cat("   Non-NA values:", sum(!is.na(monthly_sidra$m_taxapartic)), "\n")
  cat("   Range:", round(min(monthly_sidra$m_taxapartic, na.rm = TRUE), 2), "to",
      round(max(monthly_sidra$m_taxapartic, na.rm = TRUE), 2), "\n")
} else {
  cat("   WARNING: m_taxapartic not found in data!\n")
  cat("   Available columns:", paste(head(names(monthly_sidra), 10), collapse = ", "), "...\n")
}

# ------------------------------------------------------------------------------
# 3. Check date conversion
# ------------------------------------------------------------------------------

cat("\n3. Checking date conversion...\n")

# Create dates from anomesexato (YYYYMM format)
anomesexato <- monthly_sidra$anomesexato
dates <- as.Date(paste0(substr(anomesexato, 1, 4), "-",
                        substr(anomesexato, 5, 6), "-15"))

cat("   First 5 dates:", paste(head(dates, 5), collapse = ", "), "\n")
cat("   Last 5 dates:", paste(tail(dates, 5), collapse = ", "), "\n")

# Check split point (2015-10-01)
split_date <- as.Date("2015-10-01")
split_idx <- which(dates >= split_date)[1]
cat("   Split date:", as.character(split_date), "\n")
cat("   Split index:", split_idx, "\n")
cat("   Date at split:", as.character(dates[split_idx]), "\n")
cat("   Observations before split:", split_idx - 1, "\n")
cat("   Observations after split:", length(dates) - split_idx + 1, "\n")

# ------------------------------------------------------------------------------
# 4. Test X-13 ARIMA directly on the data
# ------------------------------------------------------------------------------

cat("\n4. Testing X-13 ARIMA directly...\n")

if (!requireNamespace("seasonal", quietly = TRUE)) {
  cat("   ERROR: 'seasonal' package not available!\n")
} else {
  library(seasonal)

  # Get taxapartic values (or first numeric column)
  if ("m_taxapartic" %in% names(monthly_sidra)) {
    values <- monthly_sidra$m_taxapartic
  } else {
    # Find first numeric column that's not anomesexato
    num_cols <- names(monthly_sidra)[sapply(monthly_sidra, is.numeric)]
    num_cols <- setdiff(num_cols, "anomesexato")
    if (length(num_cols) > 0) {
      values <- monthly_sidra[[num_cols[1]]]
      cat("   Using column:", num_cols[1], "\n")
    } else {
      stop("No numeric columns found!")
    }
  }

  # Order by date
  ord <- order(dates)
  values_ord <- values[ord]
  dates_ord <- dates[ord]

  # PERIOD 1: Before 2015-10 (old questionnaire)
  cat("\n   PERIOD 1 (Before 2015-10):\n")
  idx1 <- 1:(split_idx - 1)
  vals1 <- values_ord[idx1]
  dates1 <- dates_ord[idx1]

  cat("     Observations:", length(vals1), "\n")
  cat("     Non-NA:", sum(!is.na(vals1)), "\n")
  cat("     Date range:", as.character(dates1[1]), "to", as.character(tail(dates1, 1)), "\n")

  if (sum(!is.na(vals1)) >= 24) {
    # Interpolate NAs
    vals1_clean <- vals1
    na_idx <- which(is.na(vals1_clean))
    if (length(na_idx) > 0 && length(na_idx) < length(vals1_clean)) {
      non_na_idx <- which(!is.na(vals1_clean))
      vals1_clean[na_idx] <- approx(non_na_idx, vals1_clean[non_na_idx], xout = na_idx)$y
    }

    # Create ts object
    start_year1 <- as.numeric(format(dates1[1], "%Y"))
    start_month1 <- as.numeric(format(dates1[1], "%m"))
    ts1 <- ts(vals1_clean, frequency = 12, start = c(start_year1, start_month1))

    cat("     TS start:", start_year1, "-", start_month1, "\n")
    cat("     TS length:", length(ts1), "\n")

    tryCatch({
      m1 <- seas(ts1)
      adj1 <- final(m1)

      # Get seasonal component
      seasonal1 <- tryCatch({
        s <- series(m1, "s10")
        if (is.null(s)) s <- series(m1, "d10")
        as.numeric(s)
      }, error = function(e) NULL)

      cat("     X-13 model:", as.character(m1$mdl$arima), "\n")
      cat("     Adjusted variance:", round(var(as.numeric(adj1), na.rm = TRUE), 4), "\n")

      if (!is.null(seasonal1)) {
        cat("     Seasonal variance:", round(var(seasonal1, na.rm = TRUE), 6), "\n")
        cat("     Seasonal range:", round(diff(range(seasonal1, na.rm = TRUE)), 6), "\n")
        cat("     Seasonal sample:", paste(round(head(seasonal1, 6), 4), collapse = ", "), "\n")
      } else {
        cat("     WARNING: Could not extract seasonal component!\n")
      }
    }, error = function(e) {
      cat("     ERROR:", e$message, "\n")
    })
  } else {
    cat("     SKIPPED: Not enough data\n")
  }

  # PERIOD 2: After 2015-10 (new questionnaire)
  cat("\n   PERIOD 2 (After 2015-10):\n")
  idx2 <- split_idx:length(values_ord)
  vals2 <- values_ord[idx2]
  dates2 <- dates_ord[idx2]

  cat("     Observations:", length(vals2), "\n")
  cat("     Non-NA:", sum(!is.na(vals2)), "\n")
  cat("     Date range:", as.character(dates2[1]), "to", as.character(tail(dates2, 1)), "\n")

  if (sum(!is.na(vals2)) >= 24) {
    # Interpolate NAs
    vals2_clean <- vals2
    na_idx <- which(is.na(vals2_clean))
    if (length(na_idx) > 0 && length(na_idx) < length(vals2_clean)) {
      non_na_idx <- which(!is.na(vals2_clean))
      vals2_clean[na_idx] <- approx(non_na_idx, vals2_clean[non_na_idx], xout = na_idx)$y
    }

    # Create ts object
    start_year2 <- as.numeric(format(dates2[1], "%Y"))
    start_month2 <- as.numeric(format(dates2[1], "%m"))
    ts2 <- ts(vals2_clean, frequency = 12, start = c(start_year2, start_month2))

    cat("     TS start:", start_year2, "-", start_month2, "\n")
    cat("     TS length:", length(ts2), "\n")

    tryCatch({
      m2 <- seas(ts2)
      adj2 <- final(m2)

      # Get seasonal component
      seasonal2 <- tryCatch({
        s <- series(m2, "s10")
        if (is.null(s)) s <- series(m2, "d10")
        as.numeric(s)
      }, error = function(e) NULL)

      cat("     X-13 model:", as.character(m2$mdl$arima), "\n")
      cat("     Adjusted variance:", round(var(as.numeric(adj2), na.rm = TRUE), 4), "\n")

      if (!is.null(seasonal2)) {
        cat("     Seasonal variance:", round(var(seasonal2, na.rm = TRUE), 6), "\n")
        cat("     Seasonal range:", round(diff(range(seasonal2, na.rm = TRUE)), 6), "\n")
        cat("     Seasonal sample:", paste(round(head(seasonal2, 6), 4), collapse = ", "), "\n")
      } else {
        cat("     WARNING: Could not extract seasonal component!\n")
      }

      # Additional diagnostic: Check udg (diagnostics)
      cat("\n     Additional diagnostics:\n")
      tryCatch({
        udg_info <- udg(m2)
        cat("       Transform:", udg_info["transform"], "\n")
        cat("       Seasonal:", udg_info["seasonal"], "\n")
      }, error = function(e) {
        cat("       Could not get udg info:", e$message, "\n")
      })

    }, error = function(e) {
      cat("     ERROR:", e$message, "\n")
    })
  } else {
    cat("     SKIPPED: Not enough data\n")
  }
}

# ------------------------------------------------------------------------------
# 5. Check deseasonalized cache
# ------------------------------------------------------------------------------

cat("\n5. Checking deseasonalized cache...\n")

if (file.exists("data/deseasonalized_cache.rds")) {
  cache <- readRDS("data/deseasonalized_cache.rds")
  cat("   Cache exists with", length(cache), "series\n")

  # Check first series
  if (length(cache) > 0) {
    first_name <- names(cache)[1]
    first_data <- cache[[first_name]]
    cat("   First series:", first_name, "\n")
    cat("   Has X-13:", "x13" %in% names(first_data), "\n")
    cat("   Has STL:", "stl" %in% names(first_data), "\n")

    if ("x13" %in% names(first_data)) {
      x13_vals <- first_data$x13
      cat("   X-13 length:", length(x13_vals), "\n")
      cat("   X-13 non-NA:", sum(!is.na(x13_vals)), "\n")
      cat("   X-13 variance:", round(var(x13_vals, na.rm = TRUE), 4), "\n")
    }
  }
} else {
  cat("   Cache does NOT exist (will be computed on-the-fly)\n")
}

# ------------------------------------------------------------------------------
# 6. Check how app loads deseasonalized data
# ------------------------------------------------------------------------------

cat("\n6. Checking global.R data loading...\n")
cat("   Reading global.R to see how data is loaded...\n")

global_content <- readLines("global.R", warn = FALSE)
cache_lines <- grep("deseasonalized", global_content, value = TRUE, ignore.case = TRUE)
cat("   Lines mentioning 'deseasonalized':\n")
for (line in cache_lines) {
  cat("     ", trimws(line), "\n")
}

cat("\n=== Diagnostic Complete ===\n")
