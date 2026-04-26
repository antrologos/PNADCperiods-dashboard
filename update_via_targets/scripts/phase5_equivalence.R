#!/usr/bin/env Rscript
# =============================================================================
# Phase 5 Equivalence Test — entry point
#
# Compares LEGACY (data/) vs NEW (data/_new/) dashboard assets and writes a
# verdict report to scripts/phase5_report.{md,csv,json}.
#
# Usage (from update_via_targets/):
#   Rscript scripts/phase5_equivalence.R
#   Rscript scripts/phase5_equivalence.R --strict
#   Rscript scripts/phase5_equivalence.R --legacy-dir=... --new-dir=...
#
# Exit codes:
#   0 — overall PASS (and PASS|WARN under strict=FALSE)
#   1 — overall WARN/FAIL under --strict, or any uncaught error
#
# Designed to be run AFTER `tar_make()` finishes (so data/_new/ is populated).
# Reads the legacy baseline directly from data/ — does NOT regenerate it.
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(yaml)
  library(jsonlite)
  library(fst)
  library(sf)
})

# -----------------------------------------------------------------------------
# Path resolution: assumes invocation from update_via_targets/ (cwd) or that
# THIS_DIR is correctly resolved via commandArgs.
# -----------------------------------------------------------------------------

resolve_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    p <- sub("^--file=", "", file_arg[1L])
    return(normalizePath(dirname(p)))
  }
  normalizePath(getwd())
}

script_dir <- resolve_script_dir()
project_dir <- normalizePath(file.path(script_dir, ".."))
dashboard_dir <- normalizePath(file.path(project_dir, ".."))
proj_root <- normalizePath(file.path(dashboard_dir, ".."))

# Source the engine
source(file.path(project_dir, "R", "phase5_compare.R"), chdir = FALSE)

# -----------------------------------------------------------------------------
# CLI argument parsing (minimal — no external dep)
# -----------------------------------------------------------------------------

parse_arg <- function(args, key, default = NULL) {
  hit <- grep(paste0("^--", key, "="), args, value = TRUE)
  if (length(hit)) sub(paste0("^--", key, "="), "", hit[1L]) else default
}

cli_args <- commandArgs(trailingOnly = TRUE)
strict <- "--strict" %in% cli_args

# Default paths (overridable)
legacy_dir <- parse_arg(cli_args, "legacy-dir",
                        file.path(dashboard_dir, "data"))
new_dir <- parse_arg(cli_args, "new-dir",
                     file.path(dashboard_dir, "data", "_new"))
legacy_proc <- parse_arg(cli_args, "legacy-proc",
                         file.path(proj_root, "data", "processed"))
new_proc <- parse_arg(cli_args, "new-proc",
                      file.path(proj_root, "data", "processed", "_new"))

config_path <- parse_arg(cli_args, "config",
                         file.path(script_dir, "phase5_specs.yaml"))
report_dir <- parse_arg(cli_args, "report-dir", script_dir)

# -----------------------------------------------------------------------------
# Sanity checks
# -----------------------------------------------------------------------------

if (!dir.exists(legacy_dir)) {
  stop("legacy_dir does not exist: ", legacy_dir, call. = FALSE)
}
if (!dir.exists(new_dir)) {
  stop("new_dir does not exist: ", new_dir, call. = FALSE)
}
if (!file.exists(config_path)) {
  stop("config_path does not exist: ", config_path, call. = FALSE)
}

cat("Phase 5 Equivalence Test\n")
cat("------------------------\n")
cat("legacy_dir : ", legacy_dir, "\n", sep = "")
cat("new_dir    : ", new_dir, "\n", sep = "")
cat("legacy_proc: ", legacy_proc, "\n", sep = "")
cat("new_proc   : ", new_proc, "\n", sep = "")
cat("config     : ", config_path, "\n", sep = "")
cat("report_dir : ", report_dir, "\n", sep = "")
cat("strict     : ", strict, "\n\n", sep = "")

# -----------------------------------------------------------------------------
# Run
# -----------------------------------------------------------------------------

t0 <- Sys.time()
out <- run_phase5_equivalence(
  legacy_dir  = legacy_dir,
  new_dir     = new_dir,
  legacy_proc = legacy_proc,
  new_proc    = new_proc,
  config_path = config_path,
  report_dir  = report_dir,
  strict      = strict
)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

# -----------------------------------------------------------------------------
# Console summary
# -----------------------------------------------------------------------------

cat("\n", strrep("=", 70), "\n", sep = "")
cat("Verdict: **", out$overall, "**  (", sprintf("%.1f", elapsed),
    "s)\n", sep = "")
cat(strrep("=", 70), "\n\n", sep = "")

cat("Per-asset:\n")
for (r in out$results) {
  cat(sprintf("  %-30s  %-5s  matched=%s  legacy_only=%s  new_only=%s  hits=%s\n",
              r$asset, r$status,
              format(r$matched_rows %||% 0L, big.mark = ","),
              format(r$legacy_only %||% 0L, big.mark = ","),
              format(r$new_only %||% 0L, big.mark = ","),
              length(r$allowlist_hits %||% list())))
}

cat("\nReports:\n")
cat("  ", file.path(report_dir, "phase5_report.md"), "\n", sep = "")
cat("  ", file.path(report_dir, "phase5_report.csv"), "\n", sep = "")
cat("  ", file.path(report_dir, "phase5_report.json"), "\n", sep = "")

quit(status = out$exit_code)
