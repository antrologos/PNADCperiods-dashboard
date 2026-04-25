# ==============================================================================
# Local dry-run for fetch_sidra_daily.R
# ==============================================================================
# Run from PNADCperiods-dashboard repo root:
#   Rscript .github/scripts/test_fetch_locally.R          # dry-run, NO upload
#   Rscript .github/scripts/test_fetch_locally.R --upload # uploads to release
# Requires (when --upload):
#   - GITHUB_PAT or GITHUB_TOKEN environment variable
# ==============================================================================

args <- commandArgs(trailingOnly = TRUE)
do_upload <- "--upload" %in% args

if (do_upload) {
  if (Sys.getenv("GITHUB_PAT") == "" && Sys.getenv("GITHUB_TOKEN") == "") {
    stop("Set GITHUB_PAT or GITHUB_TOKEN before using --upload")
  }
  Sys.setenv(FETCH_SIDRA_DRY_RUN = "0")
  cat("Running with --upload (will pb_upload to release `data-latest`).\n\n")
} else {
  Sys.setenv(FETCH_SIDRA_DRY_RUN = "1")
  cat("Running in DRY-RUN mode (no upload). Use --upload to push to release.\n\n")
}

# Ensure CWD is the dashboard repo root (the fetch script sources R/...).
expected_files <- c("R/constants.R", "R/utils_deseasonalize.R",
                    ".github/scripts/fetch_sidra_daily.R")
missing <- expected_files[!file.exists(expected_files)]
if (length(missing) > 0L) {
  stop("Run this script from the PNADCperiods-dashboard repo root. Missing: ",
       paste(missing, collapse = ", "))
}

# Run the fetch under a tryCatch so that the verification block below
# always runs, even if the fetch script raises (e.g. stop() on staleness,
# FTP probe failure, or status="failed").
fetch_err <- tryCatch({
  source(".github/scripts/fetch_sidra_daily.R")
  NULL
}, error = function(e) e)

# Verify outputs
cat("\n=== Local verification ===\n")
if (!is.null(fetch_err)) {
  cat("Fetch reported error:", conditionMessage(fetch_err), "\n")
  cat("(Continuing to verify whatever was written before the error.)\n\n")
}
expected_outputs <- c(
  "output/series_metadata.qs2",
  "output/rolling_quarters.qs2",
  "output/monthly_sidra.qs2",
  "output/deseasonalized_cache.qs2",
  "output/sidra_log.json"
)
missing_outputs <- character()
for (f in expected_outputs) {
  exists <- file.exists(f)
  size <- if (exists) file.size(f) else 0L
  cat(sprintf("  [%s] %s (%d bytes)\n",
              if (exists) " OK " else "MISS", f, size))
  if (!exists) missing_outputs <- c(missing_outputs, f)
}

if (file.exists("output/sidra_log.json")) {
  log <- jsonlite::fromJSON("output/sidra_log.json")
  cat(sprintf("\nlog$status            = %s\n", log$status))
  cat(sprintf("log$success / total   = %d / %d\n",
              if (is.null(log$success)) 0L else log$success,
              if (is.null(log$total_series)) 0L else log$total_series))
  cat(sprintf("log$latest_ref_month  = %s\n", log$latest_ref_month))
  cat(sprintf("log$data_vintage      = %s\n", log$data_vintage))
  if (!is.null(log$duration_seconds) && !is.na(log$duration_seconds)) {
    cat(sprintf("log$duration_seconds  = %.1f\n", log$duration_seconds))
  }
  if (length(log$failed_ids) > 0L) {
    cat(sprintf("log$failed_ids        = %s\n", paste(log$failed_ids, collapse = ", ")))
  }
  if (length(log$warnings) > 0L) {
    cat("\nWarnings:\n")
    for (w in log$warnings) cat("  -", w, "\n")
  }
}

# Exit non-zero if anything went wrong, so CI / shell wrappers see the failure.
if (!is.null(fetch_err) || length(missing_outputs) > 0L) {
  cat("\n=== Test FAILED ===\n")
  if (length(missing_outputs) > 0L) {
    cat("Missing outputs: ", paste(missing_outputs, collapse = ", "), "\n", sep = "")
  }
  quit(status = 1L)
}
cat("\n=== Test OK ===\n")
