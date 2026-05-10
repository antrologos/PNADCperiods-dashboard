# ==== Layer 1: custody of the PNADC microdata acervo ==========================
# Inventory expected vs local; classify each as OK / MISSING / MISSING_UPSTREAM
# / OUTDATED against IBGE FTP + sidecar; download via PNADcIBGE; validate.
# Republication is auto-detected (FTP filename or Last-Modified advances past
# the sidecar entry → OUTDATED → rename + redownload).

# ==== Listing of expected files ===============================================
list_expected_quarters <- function(up_to_year, up_to_quarter) {
  stopifnot(up_to_quarter %in% 1:4)
  ys <- 2012L:up_to_year
  out <- data.table::CJ(year = ys, quarter = 1L:4L)
  out <- out[!(year == up_to_year & quarter > up_to_quarter)]
  out[, period := quarter]
  out[, basename := sprintf("pnadc_%d-%dq.fst", year, quarter)]
  out[]
}

# Pass visits = NULL to use get_default_visit(year) (visit 5 for 2020-2021,
# visit 1 otherwise — historical default for prepared_microdata_fst).
# Pass visits = 1:5 to enumerate all visits per year (populates the acervo
# even with visits the dashboard pipeline doesn't consume).
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

# ==== Local inventory =========================================================
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

# ==== Plan: classify expected files vs local + remote + sidecar ===============
# Returns data.table with status + upstream/sidecar columns. If remote_catalog
# is NULL, legacy OK/MISSING only (no OUTDATED detection).
plan_acervo_actions <- function(file_type, expected, local_inventory,
                                 remote_catalog = NULL,
                                 catalog_sidecar = NULL,
                                 required_vars = NULL) {
  out <- merge(
    data.table::copy(expected),
    local_inventory[, .(basename, local_path = path,
                        local_mtime = mtime_utc, size_bytes)],
    by = "basename", all.x = TRUE
  )

  # Per-row remote + sidecar lookups, pulled into vectors via lapply
  rmt <- lapply(seq_len(nrow(out)), function(i)
    .lookup_remote(remote_catalog, file_type, out$year[i], out$period[i]))
  prv <- lapply(out$basename, function(b) .lookup_sidecar(catalog_sidecar, b))

  out[, upstream_filename      := vapply(rmt, function(r)
        if (is.null(r)) NA_character_ else r$filename, character(1L))]
  out[, upstream_last_modified := do.call(c, lapply(rmt, function(r)
        if (is.null(r)) as.POSIXct(NA, tz = "UTC") else r$last_modified))]
  out[, prev_upstream_filename := vapply(prv, function(p)
        if (is.null(p)) NA_character_ else p$upstream_filename %||% NA_character_,
        character(1L))]
  out[, prev_upstream_last_modified := do.call(c, lapply(prv, function(p)
        .as_posix(if (is.null(p)) NULL else p$upstream_last_modified)))]
  out[, status := NA_character_]
  out[, reason := NA_character_]

  has_local  <- !is.na(out$local_path)
  has_remote <- !is.na(out$upstream_filename)

  # Legacy path: no FTP catalog → only OK/MISSING.
  if (is.null(remote_catalog)) {
    out[has_local,  status := "OK"]
    out[!has_local, status := "MISSING"]
    return(out[])
  }

  # Vectorized state assignment
  out[!has_remote & !has_local, `:=`(
        status = "MISSING_UPSTREAM",
        reason = "expected but neither local nor IBGE FTP has it")]
  out[!has_remote &  has_local, `:=`(
        status = "OK",
        reason = "local present; FTP listing unavailable for this entry")]
  out[ has_remote & !has_local, `:=`(
        status = "MISSING",
        reason = sprintf("FTP has %s; local absent", upstream_filename))]

  # Bootstrap (sidecar empty): compare local mtime to FTP Last-Modified.
  # .fst mtime is when PNADcIBGE wrote it; older than upstream by >1 day = stale.
  both      <- has_remote & has_local
  has_prev  <- !is.na(out$prev_upstream_filename)
  bootstrap <- both & !has_prev
  mtime_old <- bootstrap &
    !is.na(out$upstream_last_modified) & !is.na(out$local_mtime) &
    as.numeric(out$upstream_last_modified) - as.numeric(out$local_mtime) > 86400

  out[bootstrap & !mtime_old, `:=`(
        status = "OK",
        reason = "no sidecar; bootstrap from current FTP state")]
  out[mtime_old, `:=`(
        status = "OUTDATED",
        reason = paste0(
          "bootstrap (no sidecar): Last-Modified ",
          format(upstream_last_modified, "%Y-%m-%d %H:%M"),
          " newer than local mtime ",
          format(local_mtime, "%Y-%m-%d %H:%M")))]

  # Both present + sidecar exists — compare filename and Last-Modified.
  cmp      <- both & has_prev
  name_chg <- cmp & out$prev_upstream_filename != out$upstream_filename
  date_chg <- cmp & (is.na(out$prev_upstream_last_modified) |
                     (!is.na(out$upstream_last_modified) &
                      out$upstream_last_modified > out$prev_upstream_last_modified))

  out[name_chg, reason := sprintf("filename changed: %s -> %s",
                                  prev_upstream_filename, upstream_filename)]
  out[date_chg & !name_chg, reason := sprintf(
        "Last-Modified advanced: %s -> %s",
        format(prev_upstream_last_modified, "%Y-%m-%d %H:%M"),
        format(upstream_last_modified,      "%Y-%m-%d %H:%M"))]
  out[name_chg | date_chg,         status := "OUTDATED"]
  out[cmp & !(name_chg | date_chg), status := "OK"]

  # ---------------------------------------------------------------------------
  # Phase 3: detect drift in required_vars vs actual columns of local .fst.
  #
  # When `quarterly_required_vars` or `annual_required_vars` is expanded
  # (e.g. VD4016 added 2026-05-04, commit 3648fc3), `.fst` files downloaded
  # before the change lack the new columns. The pipeline must detect this
  # and trigger re-download — even when filename + Last-Modified haven't
  # changed.
  #
  # Loop prevention: sidecar carries `required_vars_hash` from the most
  # recent download attempt. If the current required_vars hash matches AND
  # the column is still missing, IBGE's zip itself doesn't have it — keep
  # OK so we don't redownload on every run.
  # ---------------------------------------------------------------------------
  if (!is.null(required_vars) && length(required_vars) > 0L) {
    out[, schema_drift_reason := NA_character_]
    current_hash <- digest::digest(sort(unique(as.character(required_vars))))

    has_local_idx <- which(!is.na(out$local_path))
    for (i in has_local_idx) {
      lp <- out$local_path[i]
      bn <- out$basename[i]

      # Prefer sidecar's recorded actual_columns to avoid I/O. Fall back to
      # reading the .fst metadata directly.
      sc_entry <- if (!is.null(catalog_sidecar)) catalog_sidecar[[bn]] else NULL
      actual_cols <- sc_entry$actual_columns
      if (is.null(actual_cols)) {
        actual_cols <- tryCatch(
          fst::metadata_fst(lp)$columnNames,
          error = function(e) character(0)
        )
      }
      missing_vars <- setdiff(required_vars, actual_cols)
      if (length(missing_vars) == 0L) next  # full schema present, nothing to do

      prev_hash <- sc_entry$required_vars_hash
      already_tried <- !is.null(prev_hash) &&
                       identical(as.character(prev_hash), current_hash)

      if (already_tried) {
        # IBGE upstream lacks these columns; sidecar already records the
        # attempt. Don't loop. Status already OK; just annotate reason.
        out[i, status := "OK"]
        out[i, reason := sprintf(
          "schema drift; missing %s; already attempted with current required_vars",
          paste(missing_vars, collapse = ", "))]
      } else {
        out[i, status := "OUTDATED"]
        out[i, reason := sprintf(
          "schema drift: local .fst missing %s",
          paste(missing_vars, collapse = ", "))]
      }
    }
    out[, schema_drift_reason := NULL]
  }

  out[]
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
#'
#' Phase 3 schema-drift tracking: callers should pass `actual_columns`
#' (read via `fst::metadata_fst(local_path)$columnNames`) and
#' `required_vars_hash` (= `digest::digest(sort(required_vars))`) so the
#' next run can detect when the schema requested is wider than what the
#' local file actually contains. Both are optional for backward compat;
#' missing either disables loop-prevention until the next download.
update_acervo_sidecar <- function(sidecar, basename, upstream_filename,
                                   upstream_last_modified,
                                   actual_columns = NULL,
                                   required_vars_hash = NULL) {
  lm <- if (inherits(upstream_last_modified, "POSIXct")) {
    format(upstream_last_modified, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  } else upstream_last_modified
  entry <- list(
    upstream_filename = upstream_filename,
    upstream_last_modified = lm
  )
  if (!is.null(actual_columns) && length(actual_columns) > 0L) {
    entry$actual_columns <- as.character(actual_columns)
  }
  if (!is.null(required_vars_hash) && nzchar(as.character(required_vars_hash))) {
    entry$required_vars_hash <- as.character(required_vars_hash)
  }
  sidecar[[basename]] <- entry
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

# ==== Apply plan: download each MISSING/OUTDATED row via PNADcIBGE ============
# Side effects: writes files on disk unless ACERVO_DRY_RUN=1. Updates sidecar
# Updates the local sidecar via `<-` (rebinding the parameter inside this
# function frame) and persists to sidecar_path at the end. NOTE: an earlier
# version used `<<-` which silently created a `sidecar` in globalenv and left
# the local parameter untouched, so save_acervo_sidecar always wrote `[]`.
#
# Phase 3: optionally captures `actual_columns` from each .fst (via
# fst::metadata_fst) and stores `required_vars_hash` so subsequent
# plan_acervo_actions runs detect schema drift accurately and avoid
# infinite redownload loops when IBGE's upstream lacks a requested column.
apply_acervo_plan <- function(plan, file_type, dest_dir,
                               sidecar = list(),
                               sidecar_path = NULL,
                               required_vars = NULL) {
  out <- data.table::copy(plan)
  out[, download_timestamp := as.POSIXct(NA, tz = "UTC")]
  out[, n_rows := NA_integer_]

  # Phase 3: pre-compute hash of current required_vars (NULL → no hash;
  # sidecar won't carry required_vars_hash, falling back to legacy semantics).
  current_hash <- if (!is.null(required_vars) && length(required_vars) > 0L) {
    digest::digest(sort(unique(as.character(required_vars))))
  } else NULL

  # Helper: read columns from an .fst file (NULL on any error).
  .read_fst_cols <- function(path) {
    if (is.na(path) || !file.exists(path)) return(NULL)
    tryCatch(fst::metadata_fst(path)$columnNames,
             error = function(e) NULL)
  }

  # Bootstrap: OK rows with upstream filename but no sidecar entry yet —
  # register identity without downloading (handles first FTP-watcher run).
  # Phase 3: also capture actual_columns from local .fst.
  boot <- which(out$status == "OK" & !is.na(out$upstream_filename) &
                is.na(out$prev_upstream_filename))
  for (i in boot) {
    actual_cols <- .read_fst_cols(out$local_path[i])
    sidecar <- update_acervo_sidecar(
      sidecar, out$basename[i], out$upstream_filename[i],
      out$upstream_last_modified[i],
      actual_columns = actual_cols,
      required_vars_hash = current_hash)
  }

  # Sequential I/O: download each MISSING/OUTDATED row
  to_do <- which(out$status %in% c("MISSING", "OUTDATED"))
  for (i in to_do) {
    bn          <- out$basename[i]
    is_outdated <- out$status[i] == "OUTDATED"
    dest        <- file.path(dest_dir, bn)
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)

    # OUTDATED: rename existing local to .OUTDATED.<ts> before redownload
    if (is_outdated && !is.na(out$local_path[i]) &&
        file.exists(out$local_path[i]) && !acervo_is_dry_run()) {
      stamp <- format(Sys.time(), "%Y%m%dT%H%M%S", tz = "UTC")
      stash <- paste0(out$local_path[i], ".OUTDATED.", stamp)
      ok_rename <- tryCatch({ atomic_rename(out$local_path[i], stash); TRUE },
                            error = function(e) {
                              message("could not stash ", bn, ": ",
                                      conditionMessage(e)); FALSE })
      if (!ok_rename) {
        out[i, `:=`(status = "FAILED",
                    reason = "could not stash OUTDATED local file")]
        next
      }
    }

    download_ok <- tryCatch({
      if (file_type == "quarterly")
        download_quarter(out$year[i], out$period[i], dest)
      else
        download_visit(out$year[i], out$period[i], dest)
      TRUE
    }, error = function(e) {
      message("download failed for ", bn, ": ", conditionMessage(e)); FALSE })

    if (download_ok && !acervo_is_dry_run()) {
      out[i, `:=`(
        status             = if (is_outdated) "DOWNLOADED_UPDATE" else "DOWNLOADED_NEW",
        local_path         = dest,
        download_timestamp = Sys.time()
      )]
      if (!is.na(out$upstream_filename[i])) {
        # Phase 3: capture actual columns from the just-written .fst.
        actual_cols <- .read_fst_cols(dest)
        sidecar <- update_acervo_sidecar(
          sidecar, bn, out$upstream_filename[i], out$upstream_last_modified[i],
          actual_columns = actual_cols,
          required_vars_hash = current_hash)
      }
    } else if (!download_ok) {
      out[i, `:=`(status = "FAILED", reason = "download error")]
    }
  }

  if (!is.null(sidecar_path) && !acervo_is_dry_run())
    save_acervo_sidecar(sidecar, sidecar_path)

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

# ==== Validate manifest_partial ===============================================
# Re-run schema/cardinality on each freshly downloaded file; flag failures as
# INVALID. Same shape as input with validation_ok / validation_reason / n_rows
# columns added. Used by both quarterly_manifest and annual_manifest targets.
validate_acervo_manifest <- function(manifest_partial, file_type) {
  m <- data.table::copy(manifest_partial)
  m[, validation_ok     := TRUE]
  m[, validation_reason := NA_character_]
  idx <- which(m$status %in% c("DOWNLOADED_NEW", "DOWNLOADED_UPDATE") &
                 !is.na(m$local_path))
  for (i in idx) {
    v <- validate_downloaded_file(m$local_path[i], file_type, m$year[i])
    if (!isTRUE(v$ok)) {
      m[i, `:=`(status            = "INVALID",
                validation_ok     = FALSE,
                validation_reason = v$reason)]
    } else {
      m[i, n_rows := v$n_rows]
    }
  }
  m[]
}
