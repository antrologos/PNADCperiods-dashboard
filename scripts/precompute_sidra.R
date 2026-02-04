# ==============================================================================
# Precompute SIDRA Data for PNADCperiods Dashboard
# ==============================================================================
#
# This script generates the precomputed data files needed for Tab 1 (Series Explorer).
# Run this script before deploying the app or to refresh the data.
#
# Output files:
#   - data/series_metadata.rds
#   - data/rolling_quarters.rds
#   - data/monthly_sidra.rds
#
# ==============================================================================

library(PNADCperiods)
library(data.table)

# Create data directory if it doesn't exist
if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

cat("=== PNADCperiods Dashboard: Precompute SIDRA Data ===\n\n")

# ------------------------------------------------------------------------------
# Step 1: Generate series metadata
# ------------------------------------------------------------------------------

cat("Step 1: Generating series metadata...\n")

metadata <- get_sidra_series_metadata()
saveRDS(metadata, "data/series_metadata.rds")

cat("  - Saved", nrow(metadata), "series across", length(unique(metadata$theme)), "themes\n")
cat("  - Output: data/series_metadata.rds\n\n")

# ------------------------------------------------------------------------------
# Step 2: Fetch SIDRA rolling quarters
# ------------------------------------------------------------------------------

cat("Step 2: Fetching SIDRA rolling quarters...\n")
cat("  (This may take a few minutes...)\n")

rolling_quarters <- fetch_sidra_rolling_quarters(
  verbose = TRUE,
  use_cache = FALSE  # Force fresh fetch
)

saveRDS(rolling_quarters, "data/rolling_quarters.rds")

cat("\n  - Fetched", ncol(rolling_quarters) - 1, "series\n")
cat("  - Date range:", min(rolling_quarters$anomesfinaltrimmovel), "to",
    max(rolling_quarters$anomesfinaltrimmovel), "\n")
cat("  - Output: data/rolling_quarters.rds\n\n")

# ------------------------------------------------------------------------------
# Step 3: Mensalize SIDRA series
# ------------------------------------------------------------------------------

cat("Step 3: Mensalizing SIDRA series...\n")

monthly_sidra <- mensalize_sidra_series(
  rolling_quarters,
  verbose = TRUE
)

saveRDS(monthly_sidra, "data/monthly_sidra.rds")

cat("\n  - Generated", ncol(monthly_sidra) - 1, "monthly series\n")
cat("  - Date range:", min(monthly_sidra$anomesexato), "to",
    max(monthly_sidra$anomesexato), "\n")
cat("  - Output: data/monthly_sidra.rds\n\n")

# ------------------------------------------------------------------------------
# Step 4: Precompute de-seasonalized series (optional)
# ------------------------------------------------------------------------------

# Check if seasonal and forecast packages are available
seasonal_ok <- requireNamespace("seasonal", quietly = TRUE)
forecast_ok <- requireNamespace("forecast", quietly = TRUE)

if (seasonal_ok || forecast_ok) {
  cat("Step 4: Precomputing de-seasonalized series...\n")

  # Source de-seasonalization utilities
  source("R/utils_deseasonalize.R")

  # Select top series to precompute (most commonly used)
  # Use the series names as they appear in metadata (without m_ prefix)
  top_series <- c(
    # Rates
    "taxadesocup",           # Unemployment rate
    "taxapartic",            # Labor force participation rate
    "nivelocup",             # Employment-to-population ratio
    "niveldesocup",          # Unemployment level
    "taxacombdesosub",       # Combined unemployment+underemployment rate
    "taxacompsubutlz",       # Composite underutilization rate
    "percdesalento",         # Discouraged worker rate
    "perccontribprev",       # Social security contribution rate

    # Population (millions)
    "populacao",             # Total population
    "pop14mais",             # Working-age population
    "popnaforca",            # Labor force
    "popocup",               # Employed population
    "popdesocup",            # Unemployed population
    "popforadaforca",        # Out of labor force

    # Income
    "rendhabnominaltodos",   # Average usual nominal earnings
    "rendhabrealtodos",      # Average usual real earnings
    "rendefetnominaltodos",  # Average effective nominal earnings
    "rendefetrealtodos",     # Average effective real earnings

    # Employment types
    "empregado",             # Employees
    "contapropria",          # Self-employed
    "empregador",            # Employers

    # Sectors
    "agropecuaria",          # Agriculture
    "industria",             # Industry
    "comercio",              # Commerce
    "construcao"             # Construction
  )

  # Get dates from monthly data
  dates <- as.Date(paste0(substr(monthly_sidra$anomesexato, 1, 4), "-",
                          substr(monthly_sidra$anomesexato, 5, 6), "-15"))

  # Initialize storage for deseasonalized data
  deseasonalized_cache <- list()

  for (series_name in top_series) {
    monthly_col <- paste0("m_", series_name)
    if (!monthly_col %in% names(monthly_sidra)) {
      monthly_col <- series_name
    }

    if (monthly_col %in% names(monthly_sidra)) {
      cat("  - Processing:", series_name, "...")

      values <- monthly_sidra[[monthly_col]]

      tryCatch({
        result <- list(
          series_name = series_name,
          original = values
        )

        # X-13 ARIMA if available
        if (seasonal_ok) {
          result$x13 <- deseasonalize_x13(values, dates)
          cat(" X-13")
        }

        # STL if available
        if (forecast_ok) {
          result$stl <- deseasonalize_stl(values, dates)
          cat(" STL")
        }

        deseasonalized_cache[[series_name]] <- result
        cat(" OK\n")

      }, error = function(e) {
        cat(" ERROR:", e$message, "\n")
      })
    }
  }

  # Save cache
  saveRDS(deseasonalized_cache, "data/deseasonalized_cache.rds")
  cat("\n  - Processed", length(deseasonalized_cache), "series\n")
  cat("  - Output: data/deseasonalized_cache.rds\n\n")

} else {
  cat("Step 4: Skipping de-seasonalization (packages not available)\n")
  cat("  Install 'seasonal' and/or 'forecast' packages to enable this step.\n\n")
}

# ------------------------------------------------------------------------------
# Summary
# ------------------------------------------------------------------------------

cat("=== Precomputation Complete ===\n\n")
cat("Files generated:\n")
cat("  - data/series_metadata.rds (", format(file.size("data/series_metadata.rds"), big.mark = ","), " bytes)\n")
cat("  - data/rolling_quarters.rds (", format(file.size("data/rolling_quarters.rds"), big.mark = ","), " bytes)\n")
cat("  - data/monthly_sidra.rds (", format(file.size("data/monthly_sidra.rds"), big.mark = ","), " bytes)\n")
if (file.exists("data/deseasonalized_cache.rds")) {
  cat("  - data/deseasonalized_cache.rds (", format(file.size("data/deseasonalized_cache.rds"), big.mark = ","), " bytes)\n")
}
cat("\nTotal size: ", format(sum(file.size(list.files("data", full.names = TRUE))), big.mark = ","), " bytes\n")
cat("\nYou can now run the Shiny app.\n")
