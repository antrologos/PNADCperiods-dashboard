# ==============================================================================
# Canonical package list for the targets pipeline
#
# Loaded by `_targets.R` via tar_option_set(packages = .).
# Keep this list in sync with the dashboard DESCRIPTION when adding deps.
# ==============================================================================

tar_pipeline_packages <- c(
  # Core targets engine
  "targets",
  "tarchetypes",

  # Data manipulation / IO
  "data.table",
  "fst",
  "readxl",
  "jsonlite",
  "yaml",

  # Filesystem helpers
  "fs",

  # PNADC-specific (CRAN, pinned by tar_option_set imports)
  "PNADCperiods",
  "PNADcIBGE",

  # Deflation
  "deflateBR",

  # SIDRA (used only in offline-rebuild mode)
  "sidrar",

  # Geospatial (brazil_states_sf)
  "sf",
  "geobr",
  "rmapshaper",

  # HTTP for FTP listings
  "httr2"
)
