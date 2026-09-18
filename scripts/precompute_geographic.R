# ==============================================================================
# Precompute Geographic Data for PNADCperiods Dashboard
# ==============================================================================
#
# Phase 3: Geographic Analysis Tab
#
# This script fetches state-level (UF) labor market indicators from SIDRA
# for the geographic visualization tab.
#
# FALLBACK ONLY. The geographic tab prefers data/state_monthly_data.rds,
# which the update_via_targets pipeline builds from the microdata and which
# carries genuine monthly estimates per UF. global.R only falls back to the
# file written here when that one is missing.
#
# Why this is coarser: SIDRA publishes rolling-quarter (monthly) PNADC
# estimates for Brazil only -- every "trimestral movel" table is N1. The
# UF-level tables (4093, 4094) are plain quarterly, so this script emits
# four observations per year, each stamped with its quarter's final month,
# matching the anomesfinaltrimmovel convention the app reads.
#
# Output files:
#   - data/geographic_data.rds
#
# ==============================================================================

library(data.table)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

cat("=== PNADCperiods Dashboard: Precompute Geographic Data ===\n\n")

# ==============================================================================
# SIDRA API Helper Function
# ==============================================================================

#' Fetch data from SIDRA API
#'
#' @param api_path SIDRA API path
#' @param verbose Print progress messages
#' @return data.table with results
fetch_sidra <- function(api_path, verbose = TRUE) {
  if (verbose) cat("  Fetching:", api_path, "\n")

  # Goes through the PNADCperiods client so the project keeps a single
  # SIDRA implementation. It translates these legacy apisidra paths to
  # IBGE's aggregated-data API v3, which is what still answers:
  # apisidra.ibge.gov.br has been behind a Cloudflare challenge since
  # 2026-09-15. fetch_sidra_rolling_quarters() cannot serve this script
  # because its metadata table only covers national (N1) series.
  tryCatch({
    raw <- PNADCperiods:::.get_sidra_v3(api_path)
    if (is.data.frame(raw) && nrow(raw) > 0) {
      return(as.data.table(raw))
    }
    warning("No data returned from SIDRA")
    NULL
  }, error = function(e) {
    warning("Error fetching from SIDRA: ", e$message)
    NULL
  })
}

# ==============================================================================
# Geographic Indicator Definitions
# ==============================================================================

# State-level tables for rolling quarters
# These are the tables that provide UF-level data
geographic_series <- list(
  taxadesocup = list(
    table_id = 4093,
    variable_id = 4099,
    api_path = "/t/4093/n3/all/v/4099/p/all/d/v4099%201",
    description = "Unemployment rate"
  ),
  taxapartic = list(
    # Table 4092 does not carry variable 4096 (it only has 1641, 4087,
    # 4104 and 4105), so this request had always failed. Table 4093 does,
    # at the same territorial level.
    table_id = 4093,
    variable_id = 4096,
    api_path = "/t/4093/n3/all/v/4096/p/all/d/v4096%201",
    description = "Labor force participation rate"
  ),
  nivelocup = list(
    table_id = 4094,
    variable_id = 4097,
    api_path = "/t/4094/n3/all/v/4097/p/all/d/v4097%201",
    description = "Employment-population ratio"
  )
)

# ==============================================================================
# Fetch Geographic Data
# ==============================================================================

cat("Fetching state-level data from SIDRA...\n")
cat("(This may take a few minutes...)\n\n")

all_data <- list()

for (indicator_name in names(geographic_series)) {
  indicator <- geographic_series[[indicator_name]]

  cat("Fetching:", indicator$description, "...\n")

  dt <- fetch_sidra(indicator$api_path)

  if (!is.null(dt) && nrow(dt) > 0) {
    # Process the data
    # SIDRA labels the columns in Portuguese, with accents:
    # - "Unidade da Federacao (Codigo)" = UF code
    # - "Unidade da Federacao" = UF name
    # - "Trimestre (Codigo)" = quarter code (YYYYQ, e.g. 202601)
    # - "Valor" = Value
    # The patterns below stand in for the accented characters with `.`, so
    # they match whatever encoding the response arrives in.
    uf_code_col <- names(dt)[grepl("Unidade.*Federa.*C.*digo|UF.*C.*digo",
                                    names(dt), ignore.case = TRUE)][1]
    period_col <- names(dt)[grepl("Trimestre.*C.*digo|Per.*odo.*C.*digo",
                                   names(dt), ignore.case = TRUE)][1]
    value_col <- "Valor"

    if (is.na(uf_code_col)) {
      # Try alternative: look for column with UF codes
      for (col in names(dt)) {
        if (all(nchar(as.character(dt[[col]])) == 2 &
                grepl("^[0-9]+$", dt[[col]]))) {
          uf_code_col <- col
          break
        }
      }
    }

    if (!is.na(uf_code_col) && !is.na(period_col) && value_col %in% names(dt)) {
      # These tables are quarterly: the period code is YYYYQ (202601 =
      # first quarter of 2026). The app keys on a YYYYMM month, so each
      # quarter is stamped with its final month (Q1 -> March, Q4 ->
      # December), the same convention anomesfinaltrimmovel uses for a
      # rolling quarter.
      quarter_code <- as.integer(dt[[period_col]])
      yyyymm <- (quarter_code %/% 100L) * 100L + (quarter_code %% 100L) * 3L

      result <- data.table(
        uf_code = as.integer(dt[[uf_code_col]]),
        anomesfinaltrimmovel = yyyymm,
        value = as.numeric(gsub(",", ".", dt[[value_col]])),
        indicator = indicator_name
      )

      # Remove NA values
      result <- result[!is.na(value) & !is.na(uf_code)]

      all_data[[indicator_name]] <- result
      cat("  - Retrieved", nrow(result), "observations\n")
    } else {
      cat("  - Could not parse columns. Available:",
          paste(names(dt), collapse = ", "), "\n")
    }
  } else {
    cat("  - No data returned\n")
  }
}

# ==============================================================================
# Combine and Save
# ==============================================================================

if (length(all_data) > 0) {
  geographic_data <- rbindlist(all_data, fill = TRUE)

  # Filter to valid UF codes (11-53)
  geographic_data <- geographic_data[uf_code >= 11 & uf_code <= 53]

  # Sort by indicator, period, and UF
  setorder(geographic_data, indicator, anomesfinaltrimmovel, uf_code)

  cat("\n=== Summary ===\n")
  cat("Total observations:", nrow(geographic_data), "\n")
  cat("Indicators:", length(unique(geographic_data$indicator)), "\n")
  cat("States (UFs):", length(unique(geographic_data$uf_code)), "\n")
  cat("Period range:", min(geographic_data$anomesfinaltrimmovel), "to",
      max(geographic_data$anomesfinaltrimmovel), "\n")

  # Save
  saveRDS(geographic_data, "data/geographic_data.rds")
  cat("\nSaved to: data/geographic_data.rds\n")
  cat("File size:", format(file.size("data/geographic_data.rds"),
                           big.mark = ","), "bytes\n")

} else {
  cat("\nERROR: No data was retrieved. Check SIDRA API connectivity.\n")
  cat("\nAlternative: You can generate sample data for testing:\n")
  cat("  source('scripts/generate_sample_geographic_data.R')\n")
}

cat("\n=== Precomputation Complete ===\n")
