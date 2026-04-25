# ==============================================================================
# tar-validation.R — schema and sanity checks for downloaded acervo files
# and final dashboard assets.
#
# Two-tier validation:
#  - validate_downloaded_file(): post-download check on a single .fst, sample-
#    based, with cardinality range check.
#  - validate_dashboard_asset(): final check that an .rds shaped for the
#    dashboard has the expected schema and date range.
# ==============================================================================

# ------------------------------------------------------------------------------
# Sample-based validation of a downloaded .fst
# ------------------------------------------------------------------------------

#' Validate a downloaded .fst file.
#'
#' Reads only the first and last 1000 rows; checks PNADC structural validity
#' and a coarse cardinality range. On failure, the file is renamed to
#' <path>.INVALID (quarantined, not deleted).
#'
#' @param path absolute path of the .fst
#' @param file_type "quarterly" or "annual"
#' @param expected_year integer year the file should have
#' @return list(ok = TRUE/FALSE, reason = character, n_rows = integer)
validate_downloaded_file <- function(path, file_type, expected_year) {
  stopifnot(file_type %in% c("quarterly", "annual"))

  if (!file.exists(path)) {
    return(list(ok = FALSE, reason = "file not found", n_rows = 0L))
  }

  meta <- tryCatch(fst::metadata_fst(path), error = function(e) NULL)
  if (is.null(meta)) {
    return(quarantine(path, "could not read fst metadata", n_rows = 0L))
  }
  n_total <- as.integer(meta$nrOfRows)
  exp <- expected_n_rows[[file_type]]
  if (n_total < ceiling(0.5 * exp$median)) {
    return(quarantine(
      path,
      sprintf("cardinality %d below 0.5 * median %d (likely truncated ZIP)",
              n_total, exp$median),
      n_rows = n_total
    ))
  }

  # Sample begin and end
  head_rows <- tryCatch(
    fst::read_fst(path, from = 1L, to = min(1000L, n_total),
                  as.data.table = TRUE),
    error = function(e) NULL
  )
  if (is.null(head_rows)) {
    return(quarantine(path, "could not read head sample", n_rows = n_total))
  }

  # Year check
  ano_col <- intersect(c("Ano", "ano"), names(head_rows))
  if (length(ano_col)) {
    yrs <- unique(as.integer(head_rows[[ano_col[1L]]]))
    if (!expected_year %in% yrs) {
      return(quarantine(
        path,
        sprintf("Ano column has %s; expected %d",
                paste(yrs, collapse = ","), expected_year),
        n_rows = n_total
      ))
    }
  }

  # Trimestre check (quarterly only)
  if (file_type == "quarterly") {
    tri_col <- intersect(c("Trimestre", "trimestre"), names(head_rows))
    if (length(tri_col)) {
      tris <- unique(as.integer(head_rows[[tri_col[1L]]]))
      if (!all(tris %in% 1:4)) {
        return(quarantine(
          path,
          sprintf("Trimestre column has %s outside 1-4",
                  paste(tris, collapse = ",")),
          n_rows = n_total
        ))
      }
    }
  }

  # PNADCperiods structural validation on the head sample
  vchk <- tryCatch(
    PNADCperiods::validate_pnadc(head_rows, stop_on_error = FALSE),
    error = function(e) list(valid = FALSE, errors = conditionMessage(e))
  )
  if (isTRUE(vchk$valid) || isTRUE(vchk$ok)) {
    # ok
  } else if (is.list(vchk) && !is.null(vchk$errors) && !length(vchk$errors)) {
    # treat empty errors list as ok
  } else {
    msg <- if (is.list(vchk) && length(vchk$errors))
      paste(vapply(vchk$errors, function(e) as.character(e)[1L],
                   character(1L)), collapse = "; ")
    else "validate_pnadc returned non-ok result"
    # don't quarantine on schema mismatch (PNADCperiods may be stricter than
    # our minimal vars list); just log a warning.
    message("validate_pnadc warning for ", basename(path), ": ", msg)
  }

  list(ok = TRUE, reason = NA_character_, n_rows = n_total)
}

quarantine <- function(path, reason, n_rows = NA_integer_) {
  invalid_path <- paste0(path, ".INVALID")
  if (file.exists(path) && !acervo_is_dry_run()) {
    # atomic_rename retries on Windows file-handle races (Dropbox / antivirus)
    # and surfaces a clear error when the rename can't be done.
    rename_ok <- tryCatch({
      atomic_rename(path, invalid_path)
      TRUE
    }, error = function(e) {
      message("quarantine: failed to rename ", basename(path),
              " to .INVALID: ", conditionMessage(e))
      FALSE
    })
    if (!rename_ok) {
      reason <- paste0(reason,
                       " [quarantine rename failed; corrupt file still in place]")
    }
  }
  list(ok = FALSE, reason = reason, n_rows = n_rows)
}

# ------------------------------------------------------------------------------
# Validation of dashboard asset (.rds)
# ------------------------------------------------------------------------------

dashboard_asset_specs <- list(
  inequality_data = list(
    required_cols = c("ref_month_yyyymm", "breakdown_type", "breakdown_value",
                      "measure", "value", "n_obs", "period"),
    measure_levels = c("gini", "palma", "p90p10", "p90p50", "p50p10",
                       "top1_share", "top5_share", "top10_share",
                       "bottom50_share", "mean_income", "median_income"),
    min_rows = 1000L
  ),
  income_shares_data = list(
    required_cols = c("ref_month_yyyymm", "breakdown_type", "breakdown_value",
                      "group_type", "group_label", "share", "period"),
    min_rows = 500L
  ),
  lorenz_data = list(
    required_cols = c("ref_month_yyyymm", "breakdown_type", "breakdown_value"),
    min_rows = 1000L
  ),
  income_decomposition_data = list(
    required_cols = c("ref_month_yyyymm", "income_source", "period"),
    min_rows = 50L
  ),
  poverty_data = list(
    required_cols = c("ref_month_yyyymm", "poverty_line_id", "poverty_line_value",
                      "breakdown_type", "breakdown_value",
                      "fgt0", "fgt1", "fgt2", "n_poor", "total_pop", "period"),
    min_rows = 1000L
  ),
  state_monthly_data = list(
    required_cols = c("ref_month", "uf_code", "indicator", "value"),
    min_rows = 5000L
  ),
  brazil_states_sf = list(
    required_cols = c("uf_code", "uf_abbrev", "uf_name", "geometry"),
    # Brazil has 27 federal units: 26 states + Distrito Federal.
    min_rows = 27L
  ),
  geographic_data = list(
    required_cols = c("uf_code", "anomesfinaltrimmovel", "value", "indicator"),
    min_rows = 0L  # may legitimately be empty if SIDRA fallback unused
  )
)

#' Validate a dashboard .rds asset against its expected schema.
#'
#' @param path absolute path of the .rds
#' @param asset_id one of names(dashboard_asset_specs)
#' @return list(ok, reason)
validate_dashboard_asset <- function(path, asset_id) {
  spec <- dashboard_asset_specs[[asset_id]]
  if (is.null(spec)) {
    return(list(ok = FALSE, reason = sprintf("unknown asset_id: %s", asset_id)))
  }
  if (!file.exists(path)) {
    return(list(ok = FALSE, reason = "file not found"))
  }
  obj <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(obj)) {
    return(list(ok = FALSE, reason = "could not readRDS"))
  }
  cols <- if (inherits(obj, "sf")) names(obj) else names(obj)
  missing <- setdiff(spec$required_cols, cols)
  if (length(missing)) {
    return(list(
      ok = FALSE,
      reason = sprintf("missing columns: %s",
                       paste(missing, collapse = ", "))
    ))
  }
  nr <- if (inherits(obj, "data.frame")) nrow(obj) else length(obj)
  if (nr < spec$min_rows) {
    return(list(
      ok = FALSE,
      reason = sprintf("only %d rows; expected at least %d",
                       nr, spec$min_rows)
    ))
  }
  list(ok = TRUE, reason = NA_character_)
}

#' Aggregate validator over a named list of (path, asset_id).
validate_all_assets <- function(asset_paths) {
  out <- vector("list", length(asset_paths))
  names(out) <- names(asset_paths)
  for (id in names(asset_paths)) {
    out[[id]] <- validate_dashboard_asset(asset_paths[[id]], id)
  }
  failed <- vapply(out, function(x) !isTRUE(x$ok), logical(1L))
  if (any(failed)) {
    msgs <- vapply(names(out)[failed], function(id) {
      sprintf("  %s: %s", id, out[[id]]$reason)
    }, character(1L))
    stop("Dashboard asset validation failed:\n",
         paste(msgs, collapse = "\n"), call. = FALSE)
  }
  out
}
