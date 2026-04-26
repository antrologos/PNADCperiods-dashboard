# ==============================================================================
# tar-stack.R — Layer 2 single-pass stackers
#
# Responsibilities (PR3 of DAG re-architecture):
#  - stack_quarterly: read all 56 quarterly .fst files into a single
#    in-memory data.table target. With the full column superset
#    (`quarterly_required_vars`), this enables BOTH crosswalk_target and
#    state_monthly_asset to consume the same stack — eliminating the previous
#    duplicate ~40 GB I/O / ~6-8 min of redundant reads.
#  - build_crosswalk_from_stack: derives the period crosswalk from the
#    in-memory stack (no .fst re-read; PNADCperiods does its own column
#    subsetting).
# ==============================================================================

#' Stack all quarterly PNADC .fst files into a single data.table.
#'
#' Reads the union of `cols` available in each quarterly file and rbinds with
#' fill = TRUE so years where IBGE added/dropped columns coexist. Output is
#' the persisted target value consumed by both Layer 2 (crosswalk) and Layer 3
#' (state_monthly).
#'
#' @param quarterly_manifest manifest data.table restricted to file_type == "quarterly"
#' @param cols character vector of columns to retain (default
#'   `quarterly_required_vars` from `tar-config.R`, which is the full superset
#'   of mensalization + labor-market vars)
#' @return data.table (in-memory, ~28M rows × ~26 cols when complete)
stack_quarterly <- function(quarterly_manifest,
                            cols = quarterly_required_vars) {
  set_fst_threads(2L)
  qf <- quarterly_manifest[
    status %in% c("OK", "DOWNLOADED_NEW", "DOWNLOADED_UPDATE") &
      !is.na(local_path)
  ]
  if (!nrow(qf)) stop("No quarterly files available to stack.", call. = FALSE)

  message(sprintf("Loading %d quarterly files (single stack)...", nrow(qf)))
  out <- data.table::rbindlist(
    lapply(qf$local_path, function(p) {
      avail <- intersect(cols, names(fst::read_fst(p, from = 1L, to = 1L)))
      fst::read_fst(p, columns = avail, as.data.table = TRUE)
    }),
    fill = TRUE
  )
  gc()
  message(sprintf("Quarterly stack: %s rows", format(nrow(out), big.mark = ",")))
  out
}

#' Build the period crosswalk from the in-memory quarterly stack.
#'
#' Renamed from `build_crosswalk_from_quarterly` (which took a manifest and
#' re-read the 56 .fst). PR3 makes it consume the already-stacked data.table,
#' so a tar_make() pass reads the .fst exactly once.
#'
#' @param quarterly_stacked data.table from `stack_quarterly`
#' @return crosswalk data.table (output of `pnadc_identify_periods`)
build_crosswalk_from_stack <- function(quarterly_stacked) {
  build_crosswalk(quarterly_stacked)
}


# ==============================================================================
# Annual stack (PR4)
# ==============================================================================

#' Stack all annual visit .fst files into a single harmonized data.table.
#'
#' Wraps `load_annual_with_income_harmonization` (which handles pre/post-2015
#' income variable schema reconciliation) and applies the `get_default_visit`
#' rule (visit 5 for COVID years 2020-2021, visit 1 otherwise).
#'
#' @param annual_manifest manifest data.table restricted to file_type=="annual"
#' @return data.table of harmonized annual microdata (~5.7M rows × ~25 cols)
stack_annual <- function(annual_manifest) {
  set_fst_threads(2L)
  af <- annual_manifest[
    status %in% c("OK", "DOWNLOADED_NEW", "DOWNLOADED_UPDATE") &
      !is.na(local_path)
  ]
  af[, default_visit := get_default_visit(year)]
  # Pick only the default visit per year for the dashboard pipeline,
  # even when the acervo holds all 5 visits (post-FTP-watcher rollout).
  af <- af[period == default_visit]
  if (!nrow(af)) {
    stop("No annual files available after visit filter.", call. = FALSE)
  }
  message(sprintf("Loading %d annual visits (single stack)...", nrow(af)))
  load_annual_with_income_harmonization(af$local_path)
}


# ==============================================================================
# Annual loader with income variable harmonization (pre-2015 vs post-2015)
# ==============================================================================
#
# Moved from tar-microdata.R (PR4). The pre/post-2015 income column schema
# split — and the union/coalesce logic on the 8 income sources at the 2015
# transition — lives here because it's a stacking concern, not a builder.

load_annual_with_income_harmonization <- function(paths) {
  vars_pre <- c("v500111", "v500211", "v500311", "v500411",
                "v500511", "v500611", "v500711", "v500811",
                "v500911", "v501011", "v501111", "v501211", "v501311")
  vars_post <- c("v5001a2", "v5002a2", "v5003a2", "v5005a2",
                 "v5008a2", "v5004a2", "v5006a2", "v5007a2")
  base <- c("ano", "trimestre", "upa", "v1008", "v1014",
            "v2005", "v2007", "v2009", "v2010", "vd3004", "v1022",
            "uf", "estrato",
            "v1032", "posest", "posest_sxi",
            "vd5008", "vd4019", "vd4020")

  Sum <- function(...) {
    X <- cbind(...)
    all_na <- rowSums(!is.na(X)) == 0L
    res <- rowSums(X, na.rm = TRUE)
    res[all_na] <- NA
    res
  }

  data.table::rbindlist(lapply(paths, function(f) {
    dt <- fst::read_fst(f, as.data.table = TRUE)
    data.table::setnames(dt, tolower(names(dt)))
    yr <- if ("ano" %in% names(dt)) as.integer(dt$ano[1L]) else
      as.integer(sub(".*pnadc_(\\d{4}).*", "\\1", basename(f)))

    income_vars <- if (yr < 2015) vars_pre
                   else if (yr == 2015) c(vars_pre, vars_post)
                   else vars_post
    cols <- intersect(c(base, income_vars), names(dt))
    dt <- dt[, ..cols]

    if (yr <= 2014L) {
      dt[, v5001a2 := data.table::fifelse(is.na(v500911), NA_real_, as.numeric(v500911))]
      dt[, v5002a2 := data.table::fifelse(is.na(v501011), NA_real_, as.numeric(v501011))]
      dt[, v5003a2 := data.table::fifelse(is.na(v501111), NA_real_, as.numeric(v501111))]
      dt[, v5005a2 := data.table::fifelse(is.na(v500811), NA_real_, as.numeric(v500811))]
      dt[, v5008a2 := Sum(v500311, v500411, v501211, v501311)]
      dt[, v5004a2 := Sum(v500111, v500211)]
      dt[, v5006a2 := Sum(v500711, v500511)]
      dt[, v5007a2 := data.table::fifelse(is.na(v500611), NA_real_, as.numeric(v500611))]
      old <- intersect(vars_pre, names(dt))
      if (length(old)) dt[, (old) := NULL]
    } else if (yr == 2015L) {
      dt[, v5001a2 := Sum(v5001a2, v500911)]
      dt[, v5002a2 := Sum(v5002a2, v501011)]
      dt[, v5003a2 := Sum(v5003a2, v501111)]
      dt[, v5005a2 := Sum(v5005a2, v500811)]
      dt[, v5008a2 := Sum(v5008a2, v500311, v500411, v501211, v501311)]
      dt[, v5004a2 := Sum(v5004a2, v500111, v500211)]
      dt[, v5006a2 := Sum(v5006a2, v500711, v500511)]
      dt[, v5007a2 := Sum(v5007a2, v500611)]
      old <- intersect(vars_pre, names(dt))
      if (length(old)) dt[, (old) := NULL]
    }
    dt
  }), fill = TRUE)
}
