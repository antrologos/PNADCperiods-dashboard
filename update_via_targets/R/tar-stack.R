# ==== Layer 2 single-pass stackers ============================================
# stack_quarterly reads all 56 quarterly .fst once into an in-memory data.table.
# stack_annual does the same for the 14 annual visit-1 files, with pre/post-2015
# income variable harmonization.

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
  quarterly_files <- quarterly_manifest[
    status %in% c("OK", "DOWNLOADED_NEW", "DOWNLOADED_UPDATE") &
      !is.na(local_path)
  ]
  if (!nrow(quarterly_files)) {
    stop("No quarterly files available to stack.", call. = FALSE)
  }

  message(sprintf("Loading %d quarterly files (single stack)...",
                  nrow(quarterly_files)))
  out <- data.table::rbindlist(
    lapply(quarterly_files$local_path, function(p) {
      avail <- intersect(cols, names(fst::read_fst(p, from = 1L, to = 1L)))
      fst::read_fst(p, columns = avail, as.data.table = TRUE)
    }),
    fill = TRUE
  )
  gc()
  message(sprintf("Quarterly stack: %s rows", format(nrow(out), big.mark = ",")))
  out
}

# ==== Annual stack ============================================================

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
  annual_files <- annual_manifest[
    status %in% c("OK", "DOWNLOADED_NEW", "DOWNLOADED_UPDATE") &
      !is.na(local_path)
  ]
  annual_files[, default_visit := get_default_visit(year)]
  # Pick only the default visit per year for the dashboard pipeline,
  # even when the acervo holds all 5 visits (post-FTP-watcher rollout).
  annual_files <- annual_files[period == default_visit]
  if (!nrow(annual_files)) {
    stop("No annual files available after visit filter.", call. = FALSE)
  }
  message(sprintf("Loading %d annual visits (single stack)...",
                  nrow(annual_files)))
  load_annual_with_income_harmonization(annual_files$local_path)
}


# ==============================================================================
# Annual loader with income variable harmonization (pre-2015 vs post-2015)
# ==============================================================================
#
# Moved from tar-microdata.R (PR4). The pre/post-2015 income column schema
# split — and the union/coalesce logic on the 8 income sources at the 2015
# transition — lives here because it's a stacking concern, not a builder.

#' Income harmonization rule (PNADC anual visits):
#'   - 2012-2014: pre-2015 vars (v500*) only — remap into post-2015 names
#'                (v5*a2) via direct rename or Sum() of multiple sources.
#'   - 2015:      both schemas present in IBGE microdata — coalesce per
#'                target var via Sum() (sum-skipping-NA across overlapping
#'                inputs).
#'   - 2016+:     post-2015 vars (v5*a2) only — kept as-is.
#' Output schema is always the post-2015 names so downstream builders
#' (recode_annual, build_inequality_outputs, ...) see a stable set of cols.
#' `is_simplified_annual_year` flags years where IBGE published a stripped
#' income module (PNADC 2025 visita 1 has no VD5008/V5*A2); those rows are
#' tagged `income_module_complete = FALSE` and excluded from the inequality
#' and poverty assets downstream.
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
    # Flag year-level income-module completeness (e.g. PNADC anual 2025
    # visita 1 was published without VD5008 + V5*A2). Propagated as a
    # constant column so the harmonized stack carries it into recode_annual
    # and the .fst — downstream income/poverty builders filter on it.
    is_simplified <- detect_simplified_annual_year(dt)
    dt[, income_module_complete := !is_simplified]
    if (is_simplified) {
      message(sprintf(
        "Annual %d: simplified income module detected (no VD5008 / V5*A2); will be excluded from inequality and poverty assets.",
        yr
      ))
    }
    dt
  }), fill = TRUE)
}
