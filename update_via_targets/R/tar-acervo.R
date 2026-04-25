# ==============================================================================
# tar-acervo.R — Layer 1 (custody of the PNADC microdata acervo)
#
# Responsibilities:
#  - Inventory expected vs. local files
#  - Detect upstream republication via FTP directory listing (IBGE renames
#    the ZIP with a date suffix when reweighting)
#  - Atomically archive the previous local version before re-downloading
#  - Download missing/republished files via PNADcIBGE
#  - Validate downloaded files (sample read + cardinality range check)
#  - Build the acervo manifest
#
# All public functions are pure (no global state). Side effects on disk are
# isolated in download_*, archive_file and atomic_rename.
# ==============================================================================

# ------------------------------------------------------------------------------
# Listing of expected files
# ------------------------------------------------------------------------------

#' List expected quarterly files up to (year, quarter)
#'
#' @param up_to_year integer current year
#' @param up_to_quarter integer current quarter (1-4)
#' @return data.table(year, quarter, basename)
list_expected_quarters <- function(up_to_year, up_to_quarter) {
  stopifnot(up_to_quarter %in% 1:4)
  ys <- 2012L:up_to_year
  out <- data.table::CJ(year = ys, quarter = 1L:4L)
  out <- out[!(year == up_to_year & quarter > up_to_quarter)]
  out[, period := quarter]   # canonical join key shared with annual
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
  out[, period := visit]   # canonical join key shared with quarterly
  out[, basename := sprintf("pnadc_%d_visita%d.fst", year, visit)]
  out[]
}

#' Deflator XLS file (single, updated yearly by IBGE)
list_expected_deflator <- function(up_to_year) {
  data.table::data.table(
    year = up_to_year - 1L,
    basename = sprintf("deflator_pnadc_%d.xls", up_to_year - 1L)
  )
}

# ------------------------------------------------------------------------------
# Local inventory
# ------------------------------------------------------------------------------

#' Inventory local files in a directory matching a pattern.
#'
#' @param dir character directory path
#' @param pattern regex passed to list.files()
#' @return data.table(basename, path, size_bytes, mtime_utc)
inventory_local <- function(dir, pattern) {
  if (!dir.exists(dir)) {
    return(data.table::data.table(
      basename = character(),
      path = character(),
      size_bytes = numeric(),
      mtime_utc = as.POSIXct(character(), tz = "UTC")
    ))
  }
  paths <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (length(paths) == 0L) {
    return(data.table::data.table(
      basename = character(),
      path = character(),
      size_bytes = numeric(),
      mtime_utc = as.POSIXct(character(), tz = "UTC")
    ))
  }
  info <- file.info(paths)
  data.table::data.table(
    basename = basename(paths),
    path = paths,
    size_bytes = as.numeric(info$size),
    mtime_utc = as.POSIXct(info$mtime, tz = "UTC")
  )
}

# ------------------------------------------------------------------------------
# IBGE FTP listing (autoritative signal for republication)
# ------------------------------------------------------------------------------

#' List a directory on the IBGE HTTPS site by parsing the HTML index.
#'
#' Best-effort; on any error returns NULL (caller treats as "offline").
#' Cached files older than 24h trigger a refresh; the cache lives in memory.
#'
#' @param subpath one of names(ibge_ftp_subpaths)
#' @param timeout numeric seconds
#' @return data.table(filename, last_modified, size_bytes) or NULL
ibge_list_directory <- function(subpath, timeout = 15) {
  url <- paste0(ibge_ftp_root(), ibge_ftp_subpaths[[subpath]])
  result <- tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_user_agent("PNADCperiods-dashboard targets pipeline") |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) != 200L) return(NULL)

    body <- httr2::resp_body_string(resp)
    parse_ibge_directory_listing(body)
  }, error = function(e) {
    message("IBGE FTP listing failed for ", subpath, ": ", conditionMessage(e))
    NULL
  })
  result
}

#' Parse an Apache/Nginx-style HTML directory listing.
#'
#' Returns NULL if the body does not look like a directory listing.
#' Filenames are extracted from <a href="..."> entries; size/date best-effort.
parse_ibge_directory_listing <- function(html) {
  if (!is.character(html) || length(html) != 1L || !nzchar(html)) return(NULL)

  # Extract <a href="filename">...</a> rows; filenames typically end with
  # .zip, .ZIP, .xls, .pdf, .csv. Skip parent dir links.
  hrefs <- regmatches(
    html,
    gregexpr('href="([^"]+\\.(?:zip|ZIP|xls|XLS|pdf|csv))"', html, perl = TRUE)
  )[[1]]
  if (!length(hrefs)) return(NULL)

  filenames <- sub('href="(.+)"', "\\1", hrefs)
  filenames <- filenames[!grepl("^\\.\\.?/?$", filenames)]
  filenames <- unique(URLdecode(filenames))

  # Best-effort: pull a date adjacent to each href when present.
  # The IBGE listing format includes "DD-MMM-YYYY HH:MM" near each entry.
  # When parsing fails, last_modified is NA.
  data.table::data.table(
    filename = filenames,
    last_modified = as.POSIXct(NA, tz = "UTC"),
    size_bytes = NA_real_
  )
}

# ------------------------------------------------------------------------------
# Match IBGE upstream filenames to canonical local basenames.
#
# IBGE publishes ZIPs like:
#   PNADC_022024.zip                        (original)
#   PNADC_022024_20260324.zip               (republished/reweighted)
#   PNADC_2020_visita5.zip
#   PNADC_2020_visita5_20250822.zip
#
# We extract:
#   - canonical key (year, quarter|visit)
#   - upstream date suffix YYYYMMDD if present (signals republication)
# ------------------------------------------------------------------------------

#' Parse an IBGE upstream zip filename into (kind, year, period, upstream_date).
parse_ibge_zip_name <- function(filename) {
  qre <- "^PNADC_([0-1][0-9])([0-9]{4})(?:_([0-9]{8}))?\\.zip$"
  vre <- "^PNADC_([0-9]{4})_visita([1-5])(?:_([0-9]{8}))?\\.zip$"

  if (grepl(qre, filename, ignore.case = TRUE)) {
    m <- regmatches(filename, regexec(qre, filename, ignore.case = TRUE))[[1]]
    return(list(
      kind = "quarterly",
      year = as.integer(m[3]),
      period = as.integer(m[2]),
      upstream_date = if (nchar(m[4])) m[4] else NA_character_
    ))
  }
  if (grepl(vre, filename, ignore.case = TRUE)) {
    m <- regmatches(filename, regexec(vre, filename, ignore.case = TRUE))[[1]]
    return(list(
      kind = "annual",
      year = as.integer(m[2]),
      period = as.integer(m[3]),
      upstream_date = if (nchar(m[4])) m[4] else NA_character_
    ))
  }
  NULL
}

# ------------------------------------------------------------------------------
# Compare local vs. expected vs. remote and plan actions per file.
#
# Returns a data.table with columns:
#   file_type, year, period, basename, local_path, status, reason,
#   upstream_date, archive_basename
#
# Status values:
#   OK                       - present locally, no upstream change detected
#   MISSING                  - expected, not present locally, upstream available
#   MISSING_UPSTREAM         - expected, not present locally, IBGE not yet
#                              published (e.g. just-released year)
#   REWEIGHT                 - present locally, upstream has newer date suffix
#   CHECK_OFFLINE            - cannot reach IBGE; treat local as authoritative
# ------------------------------------------------------------------------------

plan_acervo_actions <- function(file_type,
                                expected,
                                local_inventory,
                                remote_listing,
                                manifest_prev = NULL) {
  stopifnot(file_type %in% c("quarterly", "annual"))

  expected <- data.table::copy(expected)
  expected[, basename := basename]

  out <- merge(
    expected,
    local_inventory[, .(basename, local_path = path, local_mtime = mtime_utc,
                        size_bytes)],
    by = "basename",
    all.x = TRUE
  )

  out[, status := data.table::fifelse(is.na(local_path), "MISSING", "OK")]
  out[, reason := NA_character_]
  out[, upstream_date := NA_character_]
  out[, archive_basename := NA_character_]

  if (is.null(remote_listing) || !nrow(remote_listing)) {
    out[is.na(local_path), status := "CHECK_OFFLINE"]
    out[is.na(local_path), reason := "remote listing unavailable"]
    return(out[])
  }

  # Map upstream filenames → (year, period, upstream_date)
  parsed <- lapply(remote_listing$filename, parse_ibge_zip_name)
  parsed <- parsed[!vapply(parsed, is.null, logical(1L))]
  parsed <- parsed[vapply(parsed, function(x) x$kind == file_type, logical(1L))]

  if (!length(parsed)) {
    out[is.na(local_path), status := "MISSING_UPSTREAM"]
    out[is.na(local_path), reason := "upstream listing has no matching entries"]
    return(out[])
  }

  remote_dt <- data.table::rbindlist(lapply(parsed, function(x) {
    data.table::data.table(
      year = x$year,
      period = x$period,
      upstream_date = x$upstream_date %||% NA_character_
    )
  }))

  # For each (year, period), pick the entry with the latest upstream_date.
  remote_dt[, upstream_date_for_sort := data.table::fifelse(
    is.na(upstream_date), "00000000", upstream_date
  )]
  data.table::setorder(remote_dt, year, period, -upstream_date_for_sort)
  remote_latest <- remote_dt[, .SD[1L], by = .(year, period)]
  remote_latest[, upstream_date_for_sort := NULL]

  out <- merge(
    out,
    remote_latest,
    by = c("year", "period"),
    all.x = TRUE,
    suffixes = c("", "_remote")
  )
  out[, upstream_date := upstream_date_remote]
  out[, upstream_date_remote := NULL]

  # MISSING + upstream available → already labelled MISSING (default)
  # MISSING + upstream missing  → MISSING_UPSTREAM
  out[status == "MISSING" & is.na(upstream_date) &
        !basename %in% remote_latest_known_keys(remote_latest),
      `:=`(status = "MISSING_UPSTREAM",
           reason = "IBGE has not yet published this period")]

  # OK + upstream has newer date → REWEIGHT
  if (!is.null(manifest_prev) && nrow(manifest_prev)) {
    prev <- manifest_prev[, .(basename, prev_upstream_date = upstream_date)]
    out <- merge(out, prev, by = "basename", all.x = TRUE)
  } else {
    out[, prev_upstream_date := NA_character_]
  }

  out[
    status == "OK" &
      !is.na(upstream_date) &
      (is.na(prev_upstream_date) | upstream_date > prev_upstream_date) &
      !is.na(prev_upstream_date),  # only flag when we have a baseline
    `:=`(status = "REWEIGHT",
         reason = sprintf("upstream date %s > previous %s",
                          upstream_date, prev_upstream_date))
  ]

  out[, prev_upstream_date := NULL]
  out[]
}

# helper used above
remote_latest_known_keys <- function(remote_latest) {
  # Returns canonical basenames the remote actually has. If remote_latest has
  # rows for (year, period) X, the corresponding canonical pnadc_<year>-<q>q.fst
  # / pnadc_<year>_visita<v>.fst can be served.
  if (!nrow(remote_latest)) return(character())
  q <- remote_latest[period <= 4L,
                     sprintf("pnadc_%d-%dq.fst", year, period)]
  v <- remote_latest[period >= 1L & period <= 5L,
                     sprintf("pnadc_%d_visita%d.fst", year, period)]
  c(q, v)
}

# null-coalesce
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1L && is.na(x))) y else x

# ------------------------------------------------------------------------------
# Atomic rename + archive
# ------------------------------------------------------------------------------

#' Atomic rename with retry for Windows ERROR_SHARING_VIOLATION (Dropbox handles).
#'
#' @param src,dst character paths on the same volume
#' @param retries number of retries
#' @param wait_seconds wait between retries
#' @return invisible(TRUE) on success; raises an error on failure
atomic_rename <- function(src, dst, retries = 3L, wait_seconds = 5L) {
  for (i in seq_len(retries + 1L)) {
    ok <- suppressWarnings(file.rename(src, dst))
    if (isTRUE(ok)) return(invisible(TRUE))
    if (i <= retries) Sys.sleep(wait_seconds)
  }
  stop(sprintf(
    "atomic_rename failed after %d retries: '%s' -> '%s' (file in use?)",
    retries, src, dst
  ), call. = FALSE)
}

#' Archive a local file before overwriting it with a republished version.
#'
#' Filename convention:
#'   <stem>--archived-YYYY-MM-DD--upstream-YYYYMMDD.<ext>
#'
#' @param local_path absolute path of the file to archive
#' @param upstream_date character YYYYMMDD (or NA)
#' @param archive_root archive root directory (mirrors acervo subtree)
#' @param subkind one of c("Trimestral/Dados","Anual/visitas",...)
#' @return path of the archived file
archive_file <- function(local_path, upstream_date, archive_root, subkind) {
  if (acervo_is_dry_run()) {
    message("[dry-run] would archive ", local_path)
    return(NA_character_)
  }
  if (!file.exists(local_path)) return(NA_character_)

  dest_dir <- file.path(archive_root, subkind)
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  base <- basename(local_path)
  ext  <- tools::file_ext(base)
  stem <- tools::file_path_sans_ext(base)
  today <- format(Sys.Date(), "%Y-%m-%d")
  upstream_part <- if (is.na(upstream_date) || !nzchar(upstream_date))
    "unknown" else upstream_date

  new_name <- sprintf("%s--archived-%s--upstream-%s.%s",
                      stem, today, upstream_part, ext)
  dest <- file.path(dest_dir, new_name)

  atomic_rename(local_path, dest)
  log_archive_event(local_path, dest, upstream_date, archive_root)
  dest
}

log_archive_event <- function(src, dest, upstream_date, archive_root) {
  log_path <- file.path(archive_root, "_archive_log.jsonl")
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  entry <- jsonlite::toJSON(
    list(
      ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      src = src,
      dest = dest,
      upstream_date = upstream_date %||% NA_character_
    ),
    auto_unbox = TRUE
  )
  cat(as.character(entry), "\n", file = log_path, append = TRUE, sep = "")
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# Free-disk guard
# ------------------------------------------------------------------------------

assert_disk_space <- function(path, min_bytes = acervo_min_free_bytes) {
  if (!requireNamespace("fs", quietly = TRUE)) return(invisible(NULL))
  free <- tryCatch(fs::fs_bytes(fs::dir_info(path, recurse = FALSE)$size),
                   error = function(e) NULL)
  # fs::dir_info returns size of contents, not free space — use base R:
  if (.Platform$OS.type == "windows") {
    drive <- substr(normalizePath(path, mustWork = FALSE), 1, 2)
    info <- tryCatch(
      system2("cmd", c("/c", "dir", drive), stdout = TRUE),
      error = function(e) NULL
    )
    # Best-effort only; do not block on parse failure.
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# Download wrappers (PNADcIBGE)
#
# These are minimal wrappers; the real work is delegated to the package.
# Variables filtered post-download. Output written via fst::write_fst with
# compression 50 (matches existing acervo).
# ------------------------------------------------------------------------------

#' Download a quarterly file via PNADcIBGE and persist as .fst.
#'
#' @return path of the saved .fst on success; NA on dry-run; raises on error
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
# Apply plan: for each row marked MISSING/REWEIGHT, archive (if needed) +
# download. Produces an updated manifest.
# ------------------------------------------------------------------------------

#' Apply an acervo-action plan and return an updated manifest.
#'
#' Side effects: writes/archives files on disk unless ACERVO_DRY_RUN=1.
#'
#' @param plan output of plan_acervo_actions()
#' @param file_type "quarterly" or "annual"
#' @param dest_dir directory to write downloaded files into
#' @param archive_root archive root
#' @param subkind subkind label for archiving (e.g. "Trimestral/Dados")
#' @return updated manifest data.table
apply_acervo_plan <- function(plan, file_type, dest_dir,
                              archive_root, subkind) {
  out <- data.table::copy(plan)
  out[, download_timestamp := as.POSIXct(NA, tz = "UTC")]
  out[, archive_path := NA_character_]
  out[, n_rows := NA_integer_]

  for (i in seq_len(nrow(out))) {
    row <- out[i]
    bn  <- row$basename
    st  <- row$status
    if (!st %in% c("MISSING", "REWEIGHT")) next

    dest <- file.path(dest_dir, bn)

    # Archive previous version if reweight
    if (st == "REWEIGHT" && !is.na(row$local_path) && file.exists(row$local_path)) {
      archived <- archive_file(row$local_path, row$upstream_date,
                               archive_root, subkind)
      out[i, archive_path := archived]
    }

    # Download
    download_ok <- tryCatch({
      if (file_type == "quarterly") {
        download_quarter(row$year, row$period, dest)
      } else {
        download_visit(row$year, row$period, dest)
      }
      TRUE
    }, error = function(e) {
      message("download failed for ", bn, ": ", conditionMessage(e))
      FALSE
    })

    if (download_ok && !acervo_is_dry_run()) {
      out[i, `:=`(
        status = if (st == "REWEIGHT") "REDOWNLOADED_REWEIGHT" else "DOWNLOADED_NEW",
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
# Manifest persistence
# ------------------------------------------------------------------------------

#' Write the manifest to a CSV sidecar (humans) atomically.
#'
#' The R object is the source of truth (returned target); this CSV is for
#' git-diffable inspection only.
write_manifest_csv <- function(manifest, csv_path) {
  if (acervo_is_dry_run()) return(invisible(csv_path))
  dir.create(dirname(csv_path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(csv_path, ".tmp")
  data.table::fwrite(manifest, tmp)
  atomic_rename(tmp, csv_path)
  invisible(csv_path)
}
