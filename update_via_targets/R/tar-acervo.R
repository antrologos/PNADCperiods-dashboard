# ==============================================================================
# tar-acervo.R — Layer 1 (custody of the PNADC microdata acervo)
#
# Responsibilities (simplified):
#  - Inventory expected vs. local files
#  - Plan: each expected file is either OK (already local) or MISSING
#  - Download missing files via PNADcIBGE (no design, no labels, no deflators)
#  - Validate downloaded files
#
# IBGE republication detection (e.g., reweighting) is handled OUT of band:
# the user removes the affected local file and runs `tar_make()` again. There
# is no automated FTP listing, no upstream-date parsing, no automated archive.
# ==============================================================================

# ------------------------------------------------------------------------------
# Listing of expected files
# ------------------------------------------------------------------------------

#' List expected quarterly files up to (year, quarter)
#'
#' @param up_to_year integer current year
#' @param up_to_quarter integer current quarter (1-4)
#' @return data.table(year, quarter, period, basename)
list_expected_quarters <- function(up_to_year, up_to_quarter) {
  stopifnot(up_to_quarter %in% 1:4)
  ys <- 2012L:up_to_year
  out <- data.table::CJ(year = ys, quarter = 1L:4L)
  out <- out[!(year == up_to_year & quarter > up_to_quarter)]
  out[, period := quarter]
  out[, basename := sprintf("pnadc_%d-%dq.fst", year, quarter)]
  out[]
}

#' List expected annual files (visit selection follows get_default_visit())
#'
#' @param up_to_year integer current year (annual data lags ~1 year)
list_expected_visits <- function(up_to_year) {
  ys <- 2012L:(up_to_year - 1L)
  out <- data.table::data.table(year = ys)
  out[, visit := get_default_visit(year)]
  out[, period := visit]
  out[, basename := sprintf("pnadc_%d_visita%d.fst", year, visit)]
  out[]
}

# ------------------------------------------------------------------------------
# Local inventory
# ------------------------------------------------------------------------------

#' Inventory local files in a directory matching a pattern.
#'
#' @param dir character directory path
#' @param pattern regex passed to list.files()
#' @return data.table(basename, path, size_bytes, mtime_utc)
inventory_local <- function(dir, pattern, ignore.case = FALSE) {
  if (!dir.exists(dir)) {
    return(data.table::data.table(
      basename = character(),
      path = character(),
      size_bytes = numeric(),
      mtime_utc = as.POSIXct(character(), tz = "UTC")
    ))
  }
  paths <- list.files(dir, pattern = pattern, full.names = TRUE,
                      ignore.case = ignore.case)
  if (length(paths) == 0L) {
    return(data.table::data.table(
      basename = character(),
      path = character(),
      size_bytes = numeric(),
      mtime_utc = as.POSIXct(character(), tz = "UTC")
    ))
  }
  info <- file.info(paths)
  # file.info$mtime is POSIXct in the user's local TZ; convert (not relabel)
  # by formatting to UTC then re-parsing.
  mtime_utc <- as.POSIXct(
    format(info$mtime, tz = "UTC", usetz = FALSE),
    tz = "UTC"
  )
  data.table::data.table(
    basename = basename(paths),
    path = paths,
    size_bytes = as.numeric(info$size),
    mtime_utc = mtime_utc
  )
}

# ------------------------------------------------------------------------------
# Plan: compare expected vs. local; flag missing files for download.
# ------------------------------------------------------------------------------

#' Compare expected files against local inventory.
#'
#' Returns a data.table with columns:
#'   file_type, year, period, basename, local_path, size_bytes, status, reason
#'
#' Status values:
#'   OK      — present locally
#'   MISSING — expected but not present (will be downloaded by apply_acervo_plan)
#'
#' @param file_type "quarterly" or "annual"
#' @param expected output of list_expected_quarters() or list_expected_visits()
#' @param local_inventory output of inventory_local()
plan_acervo_actions <- function(file_type, expected, local_inventory) {
  stopifnot(file_type %in% c("quarterly", "annual"))

  expected <- data.table::copy(expected)
  out <- merge(
    expected,
    local_inventory[, .(basename, local_path = path,
                        local_mtime = mtime_utc, size_bytes)],
    by = "basename",
    all.x = TRUE
  )
  out[, status := data.table::fifelse(is.na(local_path), "MISSING", "OK")]
  out[, reason := NA_character_]
  out[]
}

# ------------------------------------------------------------------------------
# Atomic rename with Windows-aware semantics.
#
# file.rename() on Windows fails if the destination already exists. To allow
# Layer 2/3 rebuilds (where the previous .rds/.fst is overwritten), unlink
# the destination first. file.rename across volumes also fails — callers
# should keep src and dst on the same volume.
# ------------------------------------------------------------------------------

atomic_rename <- function(src, dst, retries = 3L, wait_seconds = 5L) {
  for (i in seq_len(retries + 1L)) {
    if (file.exists(dst)) {
      suppressWarnings(file.remove(dst))
    }
    ok <- suppressWarnings(file.rename(src, dst))
    if (isTRUE(ok)) return(invisible(TRUE))
    if (i <= retries) Sys.sleep(wait_seconds)
  }
  stop(sprintf(
    "atomic_rename failed after %d retries: '%s' -> '%s' (file in use?)",
    retries, src, dst
  ), call. = FALSE)
}

# ------------------------------------------------------------------------------
# Download wrappers (PNADcIBGE)
#
# PNADcIBGE downloads the full ZIP from the IBGE FTP and parses internally.
# `vars` is a post-download column filter (saves memory, not bandwidth).
# We always disable design/deflator/labels — the targets pipeline applies its
# own deflation (deflator XLS) and labelling (utils_inequality.R helpers).
# ------------------------------------------------------------------------------

#' Download a quarterly file via PNADcIBGE and persist as .fst.
download_quarter <- function(year, quarter, dest_path,
                             vars = quarterly_required_vars,
                             retries = 3L) {
  if (acervo_is_dry_run()) {
    message("[dry-run] would download ", basename(dest_path))
    return(NA_character_)
  }
  for (i in seq_len(retries + 1L)) {
    ok <- tryCatch({
      df <- PNADcIBGE::get_pnadc(
        year = year,
        quarter = quarter,
        vars = vars,
        labels = FALSE,
        deflator = FALSE,
        design = FALSE
      )
      dt <- data.table::as.data.table(df)
      tmp <- paste0(dest_path, ".tmp")
      fst::write_fst(dt, tmp, compress = 50)
      atomic_rename(tmp, dest_path)
      TRUE
    }, error = function(e) {
      message(sprintf("download_quarter %d-%dq attempt %d failed: %s",
                      year, quarter, i, conditionMessage(e)))
      FALSE
    })
    if (isTRUE(ok)) return(dest_path)
    if (i <= retries) Sys.sleep(2L^i)
  }
  stop(sprintf("download_quarter failed for %d-%dq", year, quarter),
       call. = FALSE)
}

#' Download a visit (annual) file via PNADcIBGE and persist as .fst.
download_visit <- function(year, visit, dest_path,
                           vars = annual_required_vars,
                           retries = 3L) {
  if (acervo_is_dry_run()) {
    message("[dry-run] would download ", basename(dest_path))
    return(NA_character_)
  }
  for (i in seq_len(retries + 1L)) {
    ok <- tryCatch({
      df <- PNADcIBGE::get_pnadc(
        year = year,
        interview = visit,
        vars = vars,
        labels = FALSE,
        deflator = FALSE,
        design = FALSE
      )
      dt <- data.table::as.data.table(df)
      tmp <- paste0(dest_path, ".tmp")
      fst::write_fst(dt, tmp, compress = 50)
      atomic_rename(tmp, dest_path)
      TRUE
    }, error = function(e) {
      message(sprintf("download_visit %d-v%d attempt %d failed: %s",
                      year, visit, i, conditionMessage(e)))
      FALSE
    })
    if (isTRUE(ok)) return(dest_path)
    if (i <= retries) Sys.sleep(2L^i)
  }
  stop(sprintf("download_visit failed for %d-v%d", year, visit),
       call. = FALSE)
}

# ------------------------------------------------------------------------------
# Apply plan: for each MISSING row, download via PNADcIBGE.
# ------------------------------------------------------------------------------

#' Apply an acervo-action plan and return an updated manifest.
#'
#' Side effects: writes files on disk unless ACERVO_DRY_RUN=1.
#'
#' @param plan output of plan_acervo_actions()
#' @param file_type "quarterly" or "annual"
#' @param dest_dir directory to write downloaded files into
#' @return updated manifest data.table
apply_acervo_plan <- function(plan, file_type, dest_dir) {
  out <- data.table::copy(plan)
  out[, download_timestamp := as.POSIXct(NA, tz = "UTC")]
  out[, n_rows := NA_integer_]

  for (i in seq_len(nrow(out))) {
    row <- out[i]
    if (row$status != "MISSING") next

    dest <- file.path(dest_dir, row$basename)
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

    download_ok <- tryCatch({
      if (file_type == "quarterly") {
        download_quarter(row$year, row$period, dest)
      } else {
        download_visit(row$year, row$period, dest)
      }
      TRUE
    }, error = function(e) {
      message("download failed for ", row$basename, ": ", conditionMessage(e))
      FALSE
    })

    if (download_ok && !acervo_is_dry_run()) {
      out[i, `:=`(
        status = "DOWNLOADED_NEW",
        local_path = dest,
        download_timestamp = Sys.time()
      )]
    } else if (!download_ok) {
      out[i, `:=`(status = "FAILED",
                  reason = "download error")]
    }
  }
  out[]
}

# ------------------------------------------------------------------------------
# Manifest persistence (CSV sidecar for human inspection)
# ------------------------------------------------------------------------------

write_manifest_csv <- function(manifest, csv_path) {
  if (acervo_is_dry_run()) return(invisible(csv_path))
  dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(csv_path, ".tmp")
  data.table::fwrite(manifest, tmp)
  atomic_rename(tmp, csv_path)
  invisible(csv_path)
}
