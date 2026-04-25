# ==============================================================================
# Daily SIDRA fetch + mensalize + de-seasonalize (run from GitHub Actions)
# ==============================================================================
# Inputs:
#   - SIDRA API (public, no auth)
#   - PNADCperiods (CRAN >= 0.1.1)
# Outputs (written to ./output/):
#   - series_metadata.qs
#   - rolling_quarters.qs
#   - monthly_sidra.qs
#   - deseasonalized_cache.qs
#   - sidra_log.json   (always, even on failure; uploaded LAST)
# Side effect:
#   - Uploads the 5 files to release `data-latest` of $GITHUB_REPOSITORY,
#     unless FETCH_SIDRA_DRY_RUN=1 in the environment, and unless status=="failed".
# Exits with non-zero status (via stop()) if:
#   - Any retry-wrapped fetch step exhausted MAX_RETRY attempts.
#   - rolling_quarters or monthly_sidra returned empty.
#   - HEAD on the IBGE FTP probe URL fails.
#   - latest_ref_month is more than STALENESS_THRESHOLD_DAYS old (anomaly:
#     IBGE has not published for an entire quarter).
# ==============================================================================

suppressPackageStartupMessages({
  library(PNADCperiods)
  library(piggyback)
  library(qs)
  library(jsonlite)
  library(data.table)
  library(httr2)
})

# Source helpers from the dashboard repo (working dir = repo root).
source("R/constants.R")
source("R/utils_deseasonalize.R")

# ------------------------------------------------------------------------------
# Constants
# ------------------------------------------------------------------------------

# Repo is read from $GITHUB_REPOSITORY when present (Actions sets it),
# falling back to the canonical name for local dry-runs and forks.
REPO        <- Sys.getenv("GITHUB_REPOSITORY", "antrologos/PNADCperiods-dashboard")
TAG         <- "data-latest"
OUTDIR      <- "output"
MAX_RETRY   <- 3L
BACKOFF_SECONDS          <- c(10L, 60L, 180L)
STALENESS_THRESHOLD_DAYS <- 90L
IBGE_FTP_PROBE_URL <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/"
HTTP_TIMEOUT_SECONDS <- 15L

dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

# ------------------------------------------------------------------------------
# Mutable log accumulator
#
# Implemented as an environment (reference semantics) so helpers and the
# tryCatch body modify the same object with plain `$<-` (no `<<-`). The
# variable is named `runlog` to avoid colliding with base::log() in the
# `<<-` lookup chain.
# ------------------------------------------------------------------------------

t0 <- Sys.time()

runlog <- list2env(list(
  fetched_at        = format(t0, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  status            = "running",
  total_series      = length(TOP_SERIES_FOR_PRECOMPUTE),
  success           = 0L,
  failed_ids        = character(),
  warnings          = character(),
  duration_seconds  = NA_real_,
  actions_run_url   = Sys.getenv("ACTIONS_RUN_URL", ""),
  fetch_retry_count = 0L,
  latest_ref_month  = NA_character_,
  data_vintage      = NA_character_,
  ftp_ok            = NA,
  is_stale          = NA,
  asset_md5         = list()
))

write_log <- function() {
  runlog$duration_seconds <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  out_path <- file.path(OUTDIR, "sidra_log.json")
  tryCatch(
    writeLines(
      toJSON(as.list(runlog), auto_unbox = TRUE, pretty = TRUE, na = "null"),
      out_path
    ),
    error = function(e) message("write_log failed: ", conditionMessage(e))
  )
}

# Sanitise free-form error messages before they enter a public log.
# Strips absolute runner paths and anything that looks like a token.
sanitize_msg <- function(msg) {
  msg <- gsub("/home/runner/[^[:space:]]+", "<runner-path>", msg)
  msg <- gsub("gh[ps]_[A-Za-z0-9]{30,}", "<token>", msg)
  msg
}

with_retry <- function(expr_fn, label, max_retry = MAX_RETRY,
                       backoff = BACKOFF_SECONDS) {
  for (i in seq_len(max_retry)) {
    r <- tryCatch(expr_fn(), error = function(e) e)
    if (!inherits(r, "error")) return(r)
    runlog$fetch_retry_count <- runlog$fetch_retry_count + 1L
    msg <- sprintf("[%s] retry %d/%d: %s", label, i, max_retry,
                   sanitize_msg(conditionMessage(r)))
    runlog$warnings <- c(runlog$warnings, msg)
    message(msg)
    if (i < max_retry) Sys.sleep(backoff[min(i, length(backoff))])
  }
  stop(sprintf("[%s] failed after %d retries", label, max_retry))
}

# ------------------------------------------------------------------------------
# Main work, wrapped so write_log() always runs (success or failure).
# ------------------------------------------------------------------------------

DO_UPLOAD <- !identical(Sys.getenv("FETCH_SIDRA_DRY_RUN", ""), "1")

tryCatch({

  # ---- Step 1: metadata ----
  cat("Step 1: get_sidra_series_metadata()\n")
  metadata <- with_retry(function() get_sidra_series_metadata(), "metadata")
  qs::qsave(metadata, file.path(OUTDIR, "series_metadata.qs"))

  # ---- Step 2: rolling quarters ----
  cat("Step 2: fetch_sidra_rolling_quarters() (may take a few minutes)\n")
  rolling_quarters <- with_retry(
    function() fetch_sidra_rolling_quarters(verbose = TRUE, use_cache = FALSE),
    "fetch_rq"
  )
  if (is.null(rolling_quarters) || nrow(rolling_quarters) == 0L) {
    stop("[fetch_rq] returned empty data.table")
  }
  qs::qsave(rolling_quarters, file.path(OUTDIR, "rolling_quarters.qs"))

  # ---- Step 3: mensalize ----
  cat("Step 3: mensalize_sidra_series()\n")
  monthly_sidra <- with_retry(
    function() mensalize_sidra_series(rolling_quarters, verbose = TRUE),
    "mensalize"
  )
  if (is.null(monthly_sidra) || nrow(monthly_sidra) == 0L) {
    stop("[mensalize] returned empty data.table")
  }
  qs::qsave(monthly_sidra, file.path(OUTDIR, "monthly_sidra.qs"))

  # Vintage info (now safe: rolling_quarters and monthly_sidra are non-empty)
  runlog$latest_ref_month <- as.character(max(rolling_quarters$anomesfinaltrimmovel,
                                            na.rm = TRUE))
  runlog$data_vintage     <- as.character(max(monthly_sidra$anomesexato, na.rm = TRUE))

  # ---- Step 4: deseasonalize TOP_SERIES_FOR_PRECOMPUTE ----
  cat("Step 4: deseasonalize", length(TOP_SERIES_FOR_PRECOMPUTE), "series\n")
  dates <- as.Date(paste0(
    substr(monthly_sidra$anomesexato, 1, 4), "-",
    substr(monthly_sidra$anomesexato, 5, 6), "-15"
  ))

  deseasonalized_cache <- list()

  for (s in TOP_SERIES_FOR_PRECOMPUTE) {
    col <- if (paste0("m_", s) %in% names(monthly_sidra)) paste0("m_", s) else s
    if (!col %in% names(monthly_sidra)) {
      runlog$failed_ids <- c(runlog$failed_ids, s)
      runlog$warnings   <- c(runlog$warnings,
                           sprintf("[deseason:%s] series not found in monthly_sidra", s))
      next
    }
    values <- monthly_sidra[[col]]
    entry <- tryCatch({
      list(
        series_name = s,
        original    = values,
        x13         = deseasonalize_x13(values, dates),
        stl         = deseasonalize_stl(values, dates)
      )
    }, error = function(e) {
      runlog$warnings   <- c(runlog$warnings,
                           sprintf("[deseason:%s] %s", s,
                                   sanitize_msg(conditionMessage(e))))
      runlog$failed_ids <- c(runlog$failed_ids, s)
      NULL
    })
    if (!is.null(entry)) {
      deseasonalized_cache[[s]] <- entry
      runlog$success <- runlog$success + 1L
    }
  }

  qs::qsave(deseasonalized_cache, file.path(OUTDIR, "deseasonalized_cache.qs"))

  # ---- Step 5: IBGE FTP probe ----
  cat("Step 5: HEAD probe on IBGE FTP\n")
  ftp_ok <- tryCatch({
    resp <- httr2::request(IBGE_FTP_PROBE_URL) |>
      httr2::req_method("HEAD") |>
      httr2::req_timeout(HTTP_TIMEOUT_SECONDS) |>
      httr2::req_user_agent("PNADCperiods-dashboard/sidra-daily") |>
      httr2::req_perform()
    status <- httr2::resp_status(resp)
    # Treat 405 (Method Not Allowed for HEAD) as alive: some HTTPS-served FTP
    # gateways disable HEAD on directories.
    if (status >= 400L && status != 405L) {
      runlog$warnings <- c(runlog$warnings,
                         sprintf("[ibge_ftp] HEAD returned HTTP %d", status))
      FALSE
    } else {
      TRUE
    }
  }, error = function(e) {
    runlog$warnings <- c(runlog$warnings,
                       sprintf("[ibge_ftp] HEAD failed: %s",
                               sanitize_msg(conditionMessage(e))))
    FALSE
  })
  runlog$ftp_ok <- ftp_ok

  # ---- Step 6: staleness ----
  staleness_days <- NA_integer_
  if (!is.na(runlog$latest_ref_month) && nchar(runlog$latest_ref_month) >= 6L) {
    ref_yyyymm <- substr(runlog$latest_ref_month, 1L, 6L)
    ref_date   <- as.Date(paste0(substr(ref_yyyymm, 1L, 4L), "-",
                                 substr(ref_yyyymm, 5L, 6L), "-01"))
    staleness_days <- as.integer(Sys.Date() - ref_date)
  }
  runlog$is_stale <- !is.na(staleness_days) && staleness_days > STALENESS_THRESHOLD_DAYS
  if (runlog$is_stale) {
    runlog$warnings <- c(runlog$warnings, sprintf(
      "[staleness] latest_ref_month %s is %d days old (>%d)",
      runlog$latest_ref_month, staleness_days, STALENESS_THRESHOLD_DAYS
    ))
  }

  # ---- Step 7: status (data quality only; exit code reflects ftp/staleness too) ----
  n_failed <- length(runlog$failed_ids)
  runlog$status <- if (n_failed == 0L) {
    "success"
  } else if (runlog$success > 0L) {
    "partial"
  } else {
    "failed"
  }

  # ---- Step 8: hashes of .qs files for integrity verification ----
  qs_files <- c("series_metadata.qs", "rolling_quarters.qs",
                "monthly_sidra.qs", "deseasonalized_cache.qs")
  runlog$asset_md5 <- as.list(setNames(
    unname(tools::md5sum(file.path(OUTDIR, qs_files))),
    qs_files
  ))

  # Persist log so far (also re-persisted in finally{}, after upload)
  write_log()

  # ---- Step 9: upload (skip if dry-run or status=failed) ----
  upload_status <- "skipped"
  if (DO_UPLOAD && runlog$status != "failed") {
    cat("Step 9: pb_upload to", REPO, "tag=", TAG, "\n")
    # .qs first, then sidra_log.json LAST so a partial-upload leaves the
    # log pointing to the previous run (consumers should trust the log
    # only when all assets it references match by md5).
    for (f in qs_files) {
      with_retry(
        function() pb_upload(file.path(OUTDIR, f), repo = REPO, tag = TAG,
                             overwrite = TRUE),
        paste0("upload:", f)
      )
    }
    with_retry(
      function() pb_upload(file.path(OUTDIR, "sidra_log.json"),
                           repo = REPO, tag = TAG, overwrite = TRUE),
      "upload:sidra_log.json"
    )
    upload_status <- "uploaded"
  } else if (!DO_UPLOAD) {
    cat("Step 9: SKIPPED (FETCH_SIDRA_DRY_RUN=1)\n")
  } else {
    cat("Step 9: SKIPPED (status=failed; release not updated)\n")
  }

  # ---- Step 10: decide exit code ----
  cat("\n=== Final status:", runlog$status, "===\n")
  cat("success / total:", runlog$success, "/", runlog$total_series, "\n")
  if (length(runlog$failed_ids) > 0L) {
    cat("failed_ids:", paste(runlog$failed_ids, collapse = ", "), "\n")
  }
  cat("latest_ref_month:", runlog$latest_ref_month, "\n")
  cat("staleness_days:  ", staleness_days, "\n")
  cat("ftp_ok:          ", ftp_ok, "\n")
  cat("upload:          ", upload_status, "\n")
  cat("duration_seconds:", round(if (is.null(runlog$duration_seconds) || is.na(runlog$duration_seconds)) 0 else runlog$duration_seconds, 1L), "\n")

  if (identical(runlog$status, "failed")) {
    stop(sprintf("status=failed: %d/%d series deseasonalized",
                 runlog$success, runlog$total_series))
  }
  if (!isTRUE(ftp_ok)) stop("IBGE FTP probe failed")
  if (isTRUE(runlog$is_stale)) {
    stop(sprintf("staleness %d days exceeds threshold %d",
                 staleness_days, STALENESS_THRESHOLD_DAYS))
  }

  invisible("ok")

}, finally = {
  # Belt-and-suspenders: ensure the log is on disk even if an early step
  # above stopped before reaching the explicit write_log() call.
  write_log()
})
