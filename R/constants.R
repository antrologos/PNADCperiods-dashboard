# ==============================================================================
# Shared Constants
# ==============================================================================
# Single source of truth: sourced by global.R (Shiny app) and by external
# scripts (e.g. .github/scripts/fetch_sidra_daily.R), to avoid drift.
# ==============================================================================

# ------------------------------------------------------------------------------
# Release where SIDRA .qs2 assets are published by .github/workflows/sidra-daily.yml
# ------------------------------------------------------------------------------
RELEASE_REPO    <- "antrologos/PNADCperiods-dashboard"
RELEASE_TAG     <- Sys.getenv("DASHBOARD_RELEASE_TAG", "data-latest")
RELEASE_TIMEOUT_SECONDS <- 30L

# Names of the four .qs2 assets the dashboard consumes (also matches keys in
# sidra_log.json::asset_md5).
RELEASE_QS2_FILES <- c(
  series_metadata      = "series_metadata.qs2",
  rolling_quarters     = "rolling_quarters.qs2",
  monthly_sidra        = "monthly_sidra.qs2",
  deseasonalized_cache = "deseasonalized_cache.qs2"
)

# ------------------------------------------------------------------------------
# Top SIDRA series to pre-compute de-seasonalized variants for.
# ------------------------------------------------------------------------------
TOP_SERIES_FOR_PRECOMPUTE <- c(
  # Rates
  "taxadesocup", "taxapartic", "nivelocup", "niveldesocup",
  "taxacombdesosub", "taxacompsubutlz", "percdesalento", "perccontribprev",

  # Population (millions)
  "populacao", "pop14mais", "popnaforca", "popocup", "popdesocup", "popforadaforca",

  # Income
  "rendhabnominaltodos", "rendhabrealtodos", "rendefetnominaltodos", "rendefetrealtodos",

  # Employment types
  "empregado", "contapropria", "empregador",

  # Sectors
  "agropecuaria", "industria", "comercio", "construcao"
)
