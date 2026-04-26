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

#' List expected annual files.
#'
#' By default (`visits = NULL`) returns one row per (year, default_visit)
#' where default_visit follows `get_default_visit()` (visit 5 for
#' 2020-2021, visit 1 otherwise) — this is the historical pipeline
#' default used by `prepared_microdata_fst`.
#'
#' Pass `visits = 1:5` to enumerate ALL visits per year (used to populate
#' the local acervo even with visits the dashboard pipeline doesn't
#' consume — they're cheap to keep around for downstream research).
#'
#' @param up_to_year integer current year (annual data lags ~1 year)
#' @param visits integer vector of visits (1..5) to enumerate, or NULL
#'   (default) to use `get_default_visit(year)` per year.
list_expected_visits <- function(up_to_year, visits = NULL) {
  ys <- 2012L:(up_to_year - 1L)
  if (is.null(visits)) {
    out <- data.table::data.table(year = ys)
    out[, visit := get_default_visit(year)]
  } else {
    stopifnot(all(visits %in% 1L:5L))
    out <- data.table::CJ(year = ys, visit = as.integer(visits))
  }
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

#' Compare expected files against local inventory and the IBGE FTP catalog.
#'
#' Returns a data.table with columns:
#'   file_type, year, period, basename, local_path, size_bytes, status,
#'   upstream_filename, upstream_last_modified,
#'   prev_upstream_filename, prev_upstream_last_modified, reason
#'
#' Status values:
#'   OK               — local present; IBGE state matches sidecar (or sidecar absent and remote unavailable)
#'   MISSING          — local absent but FTP has the file (download)
#'   OUTDATED         — local present but IBGE filename or Last-Modified
#'                      changed since last download (rename + re-download)
#'   MISSING_UPSTREAM — expected but FTP also doesn't have it (IBGE not yet
#'                      published) — apply will skip
#'
#' Backwards-compatible: if `remote_catalog` is NULL, falls back to OK/MISSING
#' only (legacy behavior).
#'
#' @param file_type "quarterly" or "annual"
#' @param expected output of list_expected_quarters() or list_expected_visits()
#' @param local_inventory output of inventory_local()
#' @param remote_catalog list-by-key (year for quarterly; visit for annual)
#'   of data.tables with columns (filename, last_modified, year,
#'   period|quarter|visit, upstream_date). Pass NULL to skip remote lookup.
#' @param catalog_sidecar named list keyed by basename, each with
#'   `upstream_filename` and `upstream_last_modified` (POSIXct or
#'   character ISO-8601). Pass NULL on first use; missing entries
#'   default to OUTDATED (defensive).
plan_acervo_actions <- function(file_type, expected, local_inventory,
                                 remote_catalog = NULL,
                                 catalog_sidecar = NULL) {
  stopifnot(file_type %in% c("quarterly", "annual"))

  expected <- data.table::copy(expected)
  out <- merge(
    expected,
    local_inventory[, .(basename, local_path = path,
                        local_mtime = mtime_utc, size_bytes)],
    by = "basename",
    all.x = TRUE
  )

  # Pre-fill new columns
  out[, upstream_filename := NA_character_]
  out[, upstream_last_modified := as.POSIXct(NA, tz = "UTC")]
  out[, prev_upstream_filename := NA_character_]
  out[, prev_upstream_last_modified := as.POSIXct(NA, tz = "UTC")]
  out[, status := NA_character_]
  out[, reason := NA_character_]

  ftp_provided <- !is.null(remote_catalog)

  for (i in seq_len(nrow(out))) {
    row <- out[i]
    remote <- .lookup_remote(remote_catalog, file_type, row$year, row$period)
    prev   <- .lookup_sidecar(catalog_sidecar, row$basename)

    if (!is.null(remote)) {
      out[i, upstream_filename := remote$filename]
      out[i, upstream_last_modified := remote$last_modified]
    }
    if (!is.null(prev)) {
      out[i, prev_upstream_filename := prev$upstream_filename %||% NA_character_]
      lm <- prev$upstream_last_modified
      if (is.character(lm)) lm <- as.POSIXct(lm, format = "%Y-%m-%dT%H:%M:%SZ",
                                              tz = "UTC")
      out[i, prev_upstream_last_modified := lm]
    }

    has_local  <- !is.na(row$local_path)
    has_remote <- !is.null(remote)

    # Backwards-compat: if no remote_catalog was passed at all, fall back
    # to the original simple OK/MISSING semantics. Callers that pass a
    # catalog opt into the richer MISSING_UPSTREAM / OUTDATED logic.
    if (!ftp_provided) {
      if (has_local) {
        out[i, status := "OK"]
      } else {
        out[i, status := "MISSING"]
      }
      next
    }

    if (!has_remote && !has_local) {
      out[i, status := "MISSING_UPSTREAM"]
      out[i, reason := "expected but neither local nor IBGE FTP has it"]
    } else if (!has_remote && has_local) {
      out[i, status := "OK"]
      out[i, reason := "local present; FTP listing unavailable for this entry"]
    } else if (has_remote && !has_local) {
      out[i, status := "MISSING"]
      out[i, reason := sprintf("FTP has %s; local absent", remote$filename)]
    } else {
      # Both present: compare against sidecar (NOT against local mtime —
      # local mtime reflects download wall-clock, not IBGE publication date)
      if (is.null(prev)) {
        # Bootstrap path: no sidecar entry yet (first run after FTP-watcher
        # rollout, or sidecar lost). Treat as OK and emit a "register"
        # recommendation in the reason; apply_acervo_plan will write the
        # sidecar entry on next download cycle. Re-downloading every
        # local file just to capture state would be destructive.
        out[i, status := "OK"]
        out[i, reason := "no sidecar; bootstrap from current FTP state"]
        # Capture so apply_acervo_plan can update sidecar without download
        out[i, upstream_filename := remote$filename]
        out[i, upstream_last_modified := remote$last_modified]
      } else {
        name_changed <- !identical(prev$upstream_filename, remote$filename)
        lm0 <- prev$upstream_last_modified
        prev_lm <- if (is.character(lm0)) {
          as.POSIXct(lm0, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
        } else lm0
        date_changed <- is.na(prev_lm) ||
          (!is.na(remote$last_modified) && remote$last_modified > prev_lm)
        if (name_changed || date_changed) {
          out[i, status := "OUTDATED"]
          if (name_changed) {
            out[i, reason := sprintf("filename changed: %s -> %s",
                                      prev$upstream_filename, remote$filename)]
          } else {
            out[i, reason := sprintf("Last-Modified advanced: %s -> %s",
                                      format(prev_lm, "%Y-%m-%d %H:%M"),
                                      format(remote$last_modified,
                                              "%Y-%m-%d %H:%M"))]
          }
        } else {
          out[i, status := "OK"]
        }
      }
    }
  }
  out[]
}

#' Plan deflator actions (separate from data plans).
#'
#' Two flavours:
#'   - "trimestral_bundle": single Deflatores.zip (one row plan)
#'   - "anual_xls": multiple deflator_PNADC_YYYY.xls entries
#'
#' @param kind one of "trimestral_bundle" / "anual_xls"
#' @param remote either a 1-row data.table (for bundle) or multi-row
#'   (for anual). NULL means FTP unavailable.
#' @param local_inventory output of inventory_local on the deflator dir
#'   (anual flavour only).
#' @param local_path absolute path to the local Deflatores.zip (bundle
#'   flavour only).
#' @param catalog_sidecar named list keyed by deflator basename
plan_deflator_actions <- function(kind, remote = NULL,
                                   local_inventory = NULL,
                                   local_path = NULL,
                                   catalog_sidecar = NULL) {
  stopifnot(kind %in% c("trimestral_bundle", "anual_xls"))

  if (kind == "trimestral_bundle") {
    bn <- "Deflatores.zip"
    has_local <- !is.null(local_path) && file.exists(local_path)
    has_remote <- !is.null(remote) && nrow(remote) >= 1L
    prev <- .lookup_sidecar(catalog_sidecar, bn)
    plan <- data.table::data.table(
      file_type = "trimestral_deflator",
      basename = bn,
      local_path = if (has_local) local_path else NA_character_,
      upstream_filename = if (has_remote) remote$filename[1L] else NA_character_,
      upstream_last_modified = if (has_remote) remote$last_modified[1L]
                                else as.POSIXct(NA, tz = "UTC"),
      prev_upstream_filename = prev$upstream_filename %||% NA_character_,
      prev_upstream_last_modified = .as_posix(prev$upstream_last_modified),
      status = NA_character_, reason = NA_character_
    )
    plan[, status := .deflator_status(has_local, has_remote, prev,
                                       remote$last_modified[1L])]
    return(plan[])
  }

  # anual_xls
  empty <- data.table::data.table(
    file_type = character(), basename = character(),
    local_path = character(), year = integer(),
    upstream_filename = character(),
    upstream_last_modified = as.POSIXct(character(), tz = "UTC"),
    prev_upstream_filename = character(),
    prev_upstream_last_modified = as.POSIXct(character(), tz = "UTC"),
    status = character(), reason = character()
  )
  if (is.null(remote) || !nrow(remote)) return(empty)

  out <- data.table::copy(remote)
  out[, file_type := "anual_deflator"]
  out[, basename := filename]
  if (!is.null(local_inventory) && nrow(local_inventory)) {
    out <- merge(out,
                 local_inventory[, .(basename, local_path = path,
                                     local_mtime = mtime_utc)],
                 by = "basename", all.x = TRUE)
  } else {
    out[, local_path := NA_character_]
  }
  out[, prev_upstream_filename := vapply(basename, function(b) {
    p <- .lookup_sidecar(catalog_sidecar, b)
    p$upstream_filename %||% NA_character_
  }, character(1L))]
  out[, prev_upstream_last_modified := do.call(c, lapply(basename, function(b) {
    p <- .lookup_sidecar(catalog_sidecar, b)
    .as_posix(p$upstream_last_modified)
  }))]
  out[, upstream_filename := filename]
  out[, upstream_last_modified := last_modified]
  out[, status := NA_character_]
  out[, reason := NA_character_]
  for (i in seq_len(nrow(out))) {
    has_local <- !is.na(out$local_path[i])
    prev <- list(upstream_filename = out$prev_upstream_filename[i],
                 upstream_last_modified = out$prev_upstream_last_modified[i])
    out[i, status := .deflator_status(has_local, TRUE, prev,
                                       out$last_modified[i])]
  }
  data.table::setcolorder(out, c("file_type", "basename", "local_path",
                                 "year", "upstream_filename",
                                 "upstream_last_modified",
                                 "prev_upstream_filename",
                                 "prev_upstream_last_modified",
                                 "status", "reason"))
  out[]
}

# Helper: deflator status logic
.deflator_status <- function(has_local, has_remote, prev, remote_last_modified) {
  if (!has_remote && !has_local) return("MISSING_UPSTREAM")
  if (!has_remote && has_local) return("OK")
  if (has_remote && !has_local) return("MISSING")
  # Both present
  if (is.null(prev) || is.null(prev$upstream_filename) ||
      is.na(prev$upstream_filename)) return("OUTDATED")
  prev_lm <- .as_posix(prev$upstream_last_modified)
  if (is.na(prev_lm)) return("OUTDATED")
  if (!is.na(remote_last_modified) && remote_last_modified > prev_lm) {
    return("OUTDATED")
  }
  "OK"
}

# Helper: lookup remote catalog entry for a given (file_type, year, period).
# Returns list(filename, last_modified, upstream_date) or NULL.
#
# IMPORTANT: argument names `target_year`/`target_period` are deliberately
# NOT `year`/`period` to avoid data.table column-name shadowing inside the
# `rows[...]` filter (where bare `year` would resolve to the column, not
# the function argument).
.lookup_remote <- function(remote_catalog, file_type, target_year,
                            target_period) {
  if (is.null(remote_catalog)) return(NULL)
  key <- if (file_type == "quarterly") as.character(target_year)
         else as.character(target_period)
  rows <- remote_catalog[[key]]
  if (is.null(rows) || !nrow(rows)) return(NULL)
  # Pre-compute the mask in plain R so column-vs-arg shadowing can't bite
  mask <- if (file_type == "quarterly") {
    rows$year == target_year & rows$quarter == target_period
  } else {
    rows$year == target_year & rows$visit == target_period
  }
  matches <- rows[which(mask), ]
  if (!nrow(matches)) return(NULL)
  # Prefer the row with highest upstream_date (or NA if no suffix).
  ord <- order(matches$upstream_date, decreasing = TRUE, na.last = TRUE)
  m <- matches[ord[1L], ]
  list(filename = m$filename,
       last_modified = m$last_modified,
       upstream_date = m$upstream_date)
}

# Helper: lookup sidecar entry for a basename.
.lookup_sidecar <- function(sidecar, basename) {
  if (is.null(sidecar)) return(NULL)
  sidecar[[basename]]
}

# Helper: coerce char/POSIXct/NULL/NA to POSIXct (UTC) — robust to JSON IO.
.as_posix <- function(x) {
  if (is.null(x)) return(as.POSIXct(NA, tz = "UTC"))
  if (inherits(x, "POSIXct")) return(x)
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    fmts <- c("%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M")
    for (f in fmts) {
      v <- suppressWarnings(as.POSIXct(x, format = f, tz = "UTC"))
      if (!is.na(v)) return(v)
    }
  }
  as.POSIXct(NA, tz = "UTC")
}

# Sidecar I/O — JSON keyed by basename
load_acervo_sidecar <- function(path) {
  if (is.null(path) || !file.exists(path)) return(list())
  raw <- tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
                  error = function(e) list())
  raw
}

save_acervo_sidecar <- function(sidecar, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(sidecar, path, auto_unbox = TRUE, pretty = TRUE,
                       na = "null", null = "null", force = TRUE)
  invisible(path)
}

#' Update the sidecar list with a new entry after a successful download.
update_acervo_sidecar <- function(sidecar, basename, upstream_filename,
                                   upstream_last_modified) {
  lm <- if (inherits(upstream_last_modified, "POSIXct")) {
    format(upstream_last_modified, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else upstream_last_modified
  sidecar[[basename]] <- list(
    upstream_filename = upstream_filename,
    upstream_last_modified = lm
  )
  sidecar
}

# %||% local
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
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
#' Detect an empty/NULL response from PNADcIBGE::get_pnadc().
#'
#' IBGE prints "Data unavailable for selected quarter and year." (via
#' `message()`) and returns NULL or a 0-row data.frame for quarters not
#' yet published. Without this check, download_quarter would write an
#' empty .fst, validate_downloaded_file would later quarantine it, and
#' the user's acervo accumulates `<basename>.INVALID` files.
#'
#' @param df result of PNADcIBGE::get_pnadc()
#' @return logical, TRUE if NULL or 0 rows
is_empty_pnadc_response <- function(df) {
  is.null(df) || nrow(df) == 0L
}

download_quarter <- function(year, quarter, dest_path,
                             vars = quarterly_required_vars,
                             retries = 3L) {
  if (acervo_is_dry_run()) {
    message("[dry-run] would download ", basename(dest_path))
    return(NA_character_)
  }
  for (i in seq_len(retries + 1L)) {
    attempt <- tryCatch({
      df <- PNADcIBGE::get_pnadc(
        year = year,
        quarter = quarter,
        vars = vars,
        labels = FALSE,
        deflator = FALSE,
        design = FALSE
      )
      if (is_empty_pnadc_response(df)) {
        "unavailable"
      } else {
        dt <- data.table::as.data.table(df)
        tmp <- paste0(dest_path, ".tmp")
        fst::write_fst(dt, tmp, compress = 50)
        atomic_rename(tmp, dest_path)
        "ok"
      }
    }, error = function(e) {
      message(sprintf("download_quarter %d-%dq attempt %d failed: %s",
                      year, quarter, i, conditionMessage(e)))
      "error"
    })
    if (identical(attempt, "ok")) return(dest_path)
    if (identical(attempt, "unavailable")) {
      stop(sprintf("PNADcIBGE returned no data for quarter %d-%dq (likely not yet published by IBGE)",
                   year, quarter), call. = FALSE)
    }
    if (i <= retries) Sys.sleep(2L^i)
  }
  stop(sprintf("download_quarter failed for %d-%dq after %d attempts",
               year, quarter, retries + 1L), call. = FALSE)
}

#' Download the IBGE PNADC annual deflator XLS for a given year.
#'
#' Source: IBGE FTP at
#' Anual/Microdados/Visita/Documentacao_Geral/deflator_PNADC_<year>.xls
#' Each file is cumulative (covers 2011 through `year`), so only the
#' latest is needed.
#'
#' @param year integer — the deflator's reference year
#' @param dest_path character — local destination
#' @param retries integer
#' @return character path on success
download_deflator <- function(year, dest_path, retries = 3L) {
  url <- sprintf(
    "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Documentacao_Geral/deflator_PNADC_%d.xls",
    year
  )
  dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(dest_path, ".tmp")
  for (i in seq_len(retries + 1L)) {
    ok <- tryCatch({
      utils::download.file(url, destfile = tmp, mode = "wb", quiet = FALSE)
      atomic_rename(tmp, dest_path)
      TRUE
    }, error = function(e) {
      message(sprintf("download_deflator(%d) attempt %d failed: %s",
                      year, i, conditionMessage(e)))
      FALSE
    })
    if (isTRUE(ok)) return(dest_path)
    if (i <= retries) Sys.sleep(2L^i)
  }
  stop(sprintf("download_deflator failed for year %d (url: %s)", year, url),
       call. = FALSE)
}

#' Download the IBGE PNADC Trimestral deflators ZIP.
#'
#' Source: IBGE FTP at
#' Trimestral/Microdados/Documentacao/Deflatores.zip — a single ZIP
#' that bundles all currently-published quarterly deflator XLS files
#' (one per year, naming pattern
#' `deflator_PNADC_<YYYY>_trimestral_<suffix>.xls`).
#'
#' @param dest_path character — local destination for the ZIP
#' @return character `dest_path`
download_quarterly_deflators_zip <- function(dest_path) {
  url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/Deflatores.zip"
  utils::download.file(url, destfile = dest_path, mode = "wb", quiet = FALSE)
  dest_path
}

#' Extract a quarterly-deflators ZIP and sync into the destination dir.
#'
#' Compares each extracted file against the local same-name; copies only
#' when (a) local missing, or (b) sizes differ. Returns the basenames
#' that were actually written (empty if everything matched).
#'
#' @param zip_path character — path to local ZIP
#' @param dest_dir character — directory where deflators live (typically
#'   `acervo_subpaths(acervo_root)$quarterly_deflator`)
#' @return character vector of paths copied (zero-length if no-op)
extract_and_sync_deflators <- function(zip_path, dest_dir) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  tmp_extract <- tempfile("deflator_extract_")
  dir.create(tmp_extract)
  on.exit(unlink(tmp_extract, recursive = TRUE), add = TRUE)
  utils::unzip(zip_path, exdir = tmp_extract)

  extracted <- list.files(tmp_extract, full.names = TRUE, recursive = TRUE)
  copied <- character(0)
  for (src in extracted) {
    bn <- basename(src)
    dst <- file.path(dest_dir, bn)
    needs_copy <- !file.exists(dst) || (file.size(dst) != file.size(src))
    if (needs_copy) {
      message("Updating quarterly deflator: ", bn)
      file.copy(src, dst, overwrite = TRUE)
      copied <- c(copied, dst)
    }
  }
  copied
}

#' Ensure the latest quarterly deflators are synced locally.
#'
#' Idempotent: downloads the FTP ZIP, extracts to a tempdir, and only
#' overwrites local files whose size differs (or that don't exist).
#' Respects ACERVO_DRY_RUN. The download function is injected for
#' testability (default: download_quarterly_deflators_zip).
#'
#' @param acervo_root character — root of the local PNADC acervo
#' @param download_fn function(dest_path) — used to fetch the ZIP
#' @return character path to `Trimestral/Documentacao/`
ensure_quarterly_deflators_downloaded <- function(
    acervo_root,
    download_fn = download_quarterly_deflators_zip) {
  dest_dir <- acervo_subpaths(acervo_root)$quarterly_deflator
  if (acervo_is_dry_run()) {
    message("[dry-run] would check quarterly deflators ZIP")
    return(dest_dir)
  }
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)
  download_fn(tmp_zip)

  extract_and_sync_deflators(tmp_zip, dest_dir)
  dest_dir
}

#' Ensure the latest annual deflator XLS is present locally.
#'
#' Idempotent: if the file already exists, returns its path without
#' invoking `download_fn`. Respects ACERVO_DRY_RUN. The download
#' function is injected for testability (default: download_deflator).
#'
#' @param year integer — the deflator's reference year (typically
#'   `current_year - 1L`)
#' @param acervo_root character — root of the local PNADC acervo
#' @param download_fn function(year, dest_path) — used when missing
#' @return character path (may not exist if ACERVO_DRY_RUN=1)
ensure_deflator_downloaded <- function(year, acervo_root,
                                       download_fn = download_deflator) {
  dest_dir <- acervo_subpaths(acervo_root)$deflator
  basename <- sprintf("deflator_PNADC_%d.xls", year)
  dest_path <- file.path(dest_dir, basename)

  if (file.exists(dest_path)) return(dest_path)

  if (acervo_is_dry_run()) {
    message("[dry-run] would download ", basename)
    return(dest_path)
  }

  download_fn(year, dest_path)
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
    attempt <- tryCatch({
      df <- PNADcIBGE::get_pnadc(
        year = year,
        interview = visit,
        vars = vars,
        labels = FALSE,
        deflator = FALSE,
        design = FALSE
      )
      if (is_empty_pnadc_response(df)) {
        "unavailable"
      } else {
        dt <- data.table::as.data.table(df)
        tmp <- paste0(dest_path, ".tmp")
        fst::write_fst(dt, tmp, compress = 50)
        atomic_rename(tmp, dest_path)
        "ok"
      }
    }, error = function(e) {
      message(sprintf("download_visit %d-v%d attempt %d failed: %s",
                      year, visit, i, conditionMessage(e)))
      "error"
    })
    if (identical(attempt, "ok")) return(dest_path)
    if (identical(attempt, "unavailable")) {
      stop(sprintf("PNADcIBGE returned no data for visit %d-v%d (likely not yet published by IBGE)",
                   year, visit), call. = FALSE)
    }
    if (i <= retries) Sys.sleep(2L^i)
  }
  stop(sprintf("download_visit failed for %d-v%d after %d attempts",
               year, visit, retries + 1L), call. = FALSE)
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
apply_acervo_plan <- function(plan, file_type, dest_dir,
                               sidecar = list(),
                               sidecar_path = NULL) {
  out <- data.table::copy(plan)
  out[, download_timestamp := as.POSIXct(NA, tz = "UTC")]
  out[, n_rows := NA_integer_]

  # Ensure new sidecar columns exist even if plan was generated by an old
  # caller that didn't include them
  for (col in c("upstream_filename", "upstream_last_modified",
                "prev_upstream_filename", "prev_upstream_last_modified")) {
    if (!col %in% names(out)) {
      out[, (col) := if (col %in% c("upstream_last_modified",
                                     "prev_upstream_last_modified"))
                       as.POSIXct(NA, tz = "UTC") else NA_character_]
    }
  }

  for (i in seq_len(nrow(out))) {
    row <- out[i]

    # Bootstrap: row is OK but sidecar lacked an entry — capture the
    # current IBGE state without downloading. This handles first runs
    # after FTP-watcher rollout (or sidecar deletion).
    if (identical(row$status, "OK") &&
        !is.na(row$upstream_filename) &&
        is.na(row$prev_upstream_filename)) {
      sidecar <<- update_acervo_sidecar(
        sidecar,
        basename = row$basename,
        upstream_filename = row$upstream_filename,
        upstream_last_modified = row$upstream_last_modified
      )
      next
    }

    if (!row$status %in% c("MISSING", "OUTDATED")) next

    dest <- file.path(dest_dir, row$basename)
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

    # OUTDATED: rename existing local to .OUTDATED.<ts> before redownload
    is_outdated <- identical(row$status, "OUTDATED")
    if (is_outdated && !is.na(row$local_path) && file.exists(row$local_path) &&
        !acervo_is_dry_run()) {
      stamp <- format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")
      stash <- paste0(row$local_path, ".OUTDATED.", stamp)
      ok_rename <- tryCatch({
        atomic_rename(row$local_path, stash); TRUE
      }, error = function(e) {
        message("could not stash OUTDATED ", row$basename, ": ",
                conditionMessage(e)); FALSE
      })
      if (!ok_rename) {
        out[i, `:=`(status = "FAILED",
                    reason = "could not stash OUTDATED local file")]
        next
      }
    }

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
      new_status <- if (is_outdated) "DOWNLOADED_UPDATE" else "DOWNLOADED_NEW"
      out[i, `:=`(
        status = new_status,
        local_path = dest,
        download_timestamp = Sys.time()
      )]
      # Update sidecar with the upstream identity captured by the plan
      if (!is.na(row$upstream_filename)) {
        sidecar <<- update_acervo_sidecar(
          sidecar,
          basename = row$basename,
          upstream_filename = row$upstream_filename,
          upstream_last_modified = row$upstream_last_modified
        )
      }
    } else if (!download_ok) {
      out[i, `:=`(status = "FAILED",
                  reason = "download error")]
    }
  }

  # Persist sidecar at end of pass (single write)
  if (!is.null(sidecar_path) && !acervo_is_dry_run()) {
    save_acervo_sidecar(sidecar, sidecar_path)
  }

  attr(out, "sidecar") <- sidecar
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
