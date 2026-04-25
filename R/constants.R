# ==============================================================================
# Shared Constants
# ==============================================================================
# Constants referenced by the Shiny app and by external scripts (e.g.,
# .github/scripts/fetch_sidra_daily.R), to avoid drift between callers.
#
# To use from outside the app (e.g., GitHub Actions), do:
#   source("R/constants.R")
# ==============================================================================

# Top SIDRA series to pre-compute de-seasonalized variants for.
# Kept in sync with the local definition inside seriesExplorerServer
# (R/mod_series_explorer.R, around line 331). The module's local copy
# will be removed in Phase 3 (dashboard refactor) once R/constants.R
# is sourced from global.R.
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
