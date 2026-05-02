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
# pipeline's Layer 1 INSPECTS this folder, compares against the IBGE FTP
# catalog + sidecar (last-known upstream identity), and ASKS PNADcIBGE for
# files that are MISSING or OUTDATED. Republication (reweighting) is
# detected automatically when the FTP filename or Last-Modified advances.
# ------------------------------------------------------------------------------

tar_acervo_root <- function() {
  Sys.getenv("PNADC_ACERVO_ROOT", "D:/Dropbox/Bancos_Dados/PNADC")
}

tar_dashboard_root <- function() {
  # Pipeline lives at: <dashboard>/update_via_targets/. Dashboard root is one
  # level up from the working directory used by tar_make().
  normalizePath(file.path(getwd(), ".."), winslash = "/", mustWork = FALSE)
}

tar_dashboard_data_dir <- function() {
  file.path(tar_dashboard_root(), "data")
}

# Cache directory for prepared_microdata.fst (~340 MB). Always lives outside
# the dashboard repo. Set PNADC_PROCESSED_DIR to point at a stable directory
# (re-running the pipeline against an existing cache avoids a 30-60 min rebuild).
tar_processed_cache_dir <- function() {
  dir <- Sys.getenv("PNADC_PROCESSED_DIR", unset = "")
  if (!nzchar(dir)) {
    stop("PNADC_PROCESSED_DIR is not set. Point it to a directory where the ",
         "targets pipeline can cache prepared_microdata.fst, e.g.\n",
         "  Sys.setenv(PNADC_PROCESSED_DIR = \"D:/Dropbox/Bancos_Dados/PNADC/processed_cache\")")
  }
  dir
}

# ------------------------------------------------------------------------------
# Acervo subpaths
# ------------------------------------------------------------------------------

acervo_subpaths <- function(base = tar_acervo_root()) {
  list(
    quarterly          = file.path(base, "Trimestral", "Dados"),
    annual             = file.path(base, "Anual", "visitas"),
    deflator           = file.path(base, "Anual", "visitas", "documentacao"),
    quarterly_deflator = file.path(base, "Trimestral", "Documentacao"),
    suplements         = file.path(base, "Anual", "Trimestres")  # out of MVP scope
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

# Cardinality envelopes used by validate_downloaded_file().
# `min` is informational; only `0.5 * median` is enforced. Annual visit-1
# files for early years (2015-2017) can have ~150-250k rows, so the median
# is calibrated conservatively to avoid quarantining valid early data.
expected_n_rows <- list(
  quarterly = list(min = 350000L, median = 500000L, max = 700000L),
  annual    = list(min = 130000L, median = 250000L, max = 600000L)
)

# ------------------------------------------------------------------------------
# Reference date for deflation (used by tar-microdata.R)
# ------------------------------------------------------------------------------

deflation_target_date <- "12/2025"

#' Validate deflation_target_date format.
#'
#' Format expected by `deflateBR::inpc(real_date = ...)` is "MM/YYYY".
#' Caller validates the constant once at pipeline startup so a typo doesn't
#' produce silent NaN factors deep inside the build.
#'
#' @param s character of length 1
#' @return invisible(s) on success; throws otherwise.
validate_deflation_target_date <- function(s) {
  if (!is.character(s) || length(s) != 1L)
    stop("deflation_target_date must be a single character", call. = FALSE)
  if (!grepl("^(0[1-9]|1[0-2])/\\d{4}$", s))
    stop(sprintf("deflation_target_date '%s' is not in MM/YYYY format", s),
         call. = FALSE)
  invisible(s)
}

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
