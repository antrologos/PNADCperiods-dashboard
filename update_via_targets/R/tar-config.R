# ==============================================================================
# tar-config.R — constants and paths for the targets pipeline
#
# All hard-coded paths and column lists live here so the rest of R/tar-*.R
# files stay declarative. Override paths with environment variables when
# running on a different machine.
# ==============================================================================

# ------------------------------------------------------------------------------
# Filesystem roots
#
# Acervo (custody): user-curated local mirror of the IBGE microdata. The
# pipeline's Layer 1 only INSPECTS this folder and ASKS PNADcIBGE for files
# that are missing. When IBGE republishes a quarter or visit (reweighting),
# the user removes the local file manually; the next `tar_make()` will
# detect MISSING and re-download via PNADcIBGE.
# ------------------------------------------------------------------------------

tar_acervo_root <- function() {
  Sys.getenv("PNADC_ACERVO_ROOT", "D:/Dropbox/Bancos_Dados/PNADC")
}

tar_project_root <- function() {
  # Pipeline lives at: <project>/PNADCperiods-dashboard/update_via_targets/
  # Project root is two levels up.
  normalizePath(file.path(getwd(), "..", ".."), winslash = "/", mustWork = FALSE)
}

tar_dashboard_root <- function() {
  # Dashboard lives at: <project>/PNADCperiods-dashboard/
  normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE)
}

tar_dashboard_data_dir <- function() {
  file.path(tar_dashboard_root(), "data")
}

tar_processed_cache_dir <- function() {
  Sys.getenv(
    "PNADC_PROCESSED_DIR",
    file.path(tar_project_root(), "data", "processed")
  )
}

# ------------------------------------------------------------------------------
# Acervo subpaths
# ------------------------------------------------------------------------------

acervo_subpaths <- function(base = tar_acervo_root()) {
  list(
    quarterly  = file.path(base, "Trimestral", "Dados"),
    annual     = file.path(base, "Anual", "visitas"),
    deflator   = file.path(base, "Anual", "visitas", "documentacao"),
    suplements = file.path(base, "Anual", "Trimestres")  # out of MVP scope
  )
}

# ------------------------------------------------------------------------------
# Visit selection rule (validated against precompute_microdata_base.R:152-155)
#
# 2020-2021 use visit 5 (COVID re-design); all other years use visit 1.
# ------------------------------------------------------------------------------

get_default_visit <- function(year) {
  ifelse(year %in% c(2020L, 2021L), 5L, 1L)
}

# ------------------------------------------------------------------------------
# Column lists for downloads (minimum vars to ask PNADcIBGE for)
# ------------------------------------------------------------------------------

quarterly_required_vars <- c(
  "Ano", "Trimestre", "UPA", "V1008", "V1014",
  "V2003", "V2005", "V2008", "V20081", "V20082", "V2009",
  "V1028", "UF", "posest", "posest_sxi", "Estrato",
  # Labor market vars used by state_monthly_data
  "VD4001", "VD4002", "VD4003", "VD4004", "VD4004A",
  "VD4005", "VD4009", "VD4010", "VD4012", "V4019"
)

annual_required_vars <- c(
  # Join keys
  "Ano", "Trimestre", "UPA", "V1008", "V1014",
  # Demographics
  "V2005", "V2007", "V2009", "V2010", "VD3004", "V1022",
  "UF", "Estrato",
  # Weights
  "V1032", "V1028", "posest", "posest_sxi",
  # Total per capita income (IBGE pre-calculated)
  "VD5008",
  # Labor income
  "VD4019", "VD4020",
  # Income by source — post-2015q4 (new format)
  "V5001A2", "V5002A2", "V5003A2", "V5004A2",
  "V5005A2", "V5006A2", "V5007A2", "V5008A2",
  # Income by source — pre-2015 (old format)
  "V500111", "V500211", "V500311", "V500411",
  "V500511", "V500611", "V500711", "V500811",
  "V500911", "V501011", "V501111", "V501211", "V501311"
)

# ------------------------------------------------------------------------------
# Expected cardinalities (range checks for validate_downloaded_file)
#
# These are coarse sanity ranges; detail in tar-validation.R reads the actual
# row counts via fst::metadata_fst() and aborts if the count is below
# 0.5 * expected_median.
# ------------------------------------------------------------------------------

expected_n_rows <- list(
  quarterly = list(min = 350000L, median = 500000L, max = 700000L),
  annual    = list(min = 250000L, median = 400000L, max = 600000L)
)

# ------------------------------------------------------------------------------
# Reference date for deflation (used by tar-microdata.R)
# ------------------------------------------------------------------------------

deflation_target_date <- "12/2025"

# ------------------------------------------------------------------------------
# VD4004 / VD4004A boundary
#
# VD4004  = subocupação por horas EFETIVAS (canonical 2012-Q1 to 2015-Q3).
# VD4004A = subocupação por horas HABITUAIS (canonical 2015-Q4 onward; absent
#           from microdata before 2015-Q4).
#
# SIDRA backfills tables 6438 and 6785 to 201203 using VD4004 then switches
# to VD4004A from 201510 onward (Hecksher reference Stata code,
# code/original_codes/MensalizacaoPNADC_VersaoNov2024.do:356-357).
# Mirror the explicit cutoff to stay SIDRA-compatible without depending on
# NA-coalescence.
# ------------------------------------------------------------------------------

vd4004_split_yyyymm <- 201509L

# ------------------------------------------------------------------------------
# fst threading guard (avoids worker oversubscription when targets parallelizes)
# ------------------------------------------------------------------------------

set_fst_threads <- function(n = 2L) {
  if (requireNamespace("fst", quietly = TRUE)) {
    fst::threads_fst(n)
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# Minimum free-disk threshold before allowing a download (bytes)
# ------------------------------------------------------------------------------

acervo_min_free_bytes <- 5 * 1024^3  # 5 GB

# ------------------------------------------------------------------------------
# Dry-run helper: prefer environment variable so future workers inherit it.
# Use Sys.setenv(ACERVO_DRY_RUN = "1") before tar_make() to enable.
# ------------------------------------------------------------------------------

acervo_is_dry_run <- function() {
  env <- Sys.getenv("ACERVO_DRY_RUN", "")
  if (nzchar(env)) {
    env %in% c("1", "TRUE", "true", "yes")
  } else {
    isTRUE(getOption("acervo.dry_run"))
  }
}
