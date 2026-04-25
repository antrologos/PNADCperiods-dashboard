# ==============================================================================
# tar-export.R — atomic export of dashboard assets + migration safety gate
#
# During the 4-week coexistence window, set PNADC_PIPELINE_MODE = "staging"
# to make builders write to <dest>/_new/ instead of the live <dest>/. This
# allows the migration equivalence test to compare old vs. new outputs
# without disturbing the live dashboard.
#
# Once equivalence passes, set PNADC_PIPELINE_MODE = "live" (or unset) so
# subsequent tar_make() runs write directly to the live folder.
# ==============================================================================

#' Resolve where Layer 3 outputs should be written (staging or live).
#'
#' @param base directory like PNADCperiods-dashboard/data
#' @return base or base/_new/
resolve_dest_dir <- function(base) {
  mode <- Sys.getenv("PNADC_PIPELINE_MODE", "staging")
  out <- if (mode == "live") base else file.path(base, "_new")
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  out
}

#' Pre-pipeline backup of any pre-existing dashboard assets and Layer 2 cache.
#'
#' Idempotent: skips a backup if an identical (md5-equal) backup already
#' exists. Writes a small JSON manifest under <archive>/backup_log.json
#' for auditing.
#'
#' @param paths character vector of files to back up
#' @param archive_dir destination archive directory
#' @return character vector of backup paths (NA for files not present)
t0_migration_check <- function(paths, archive_dir) {
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)
  results <- character(length(paths))
  log <- list()
  for (i in seq_along(paths)) {
    src <- paths[i]
    if (!file.exists(src)) {
      results[i] <- NA_character_
      next
    }
    src_md5 <- tools::md5sum(src)
    bn <- basename(src)
    today <- format(Sys.Date(), "%Y-%m-%d")
    dst <- file.path(archive_dir, sprintf("%s.%s.bak", bn, today))
    if (file.exists(dst)) {
      dst_md5 <- tools::md5sum(dst)
      if (identical(unname(src_md5), unname(dst_md5))) {
        results[i] <- dst
        next
      }
      # md5 differs → keep the existing backup, write a fresh one with timestamp
      ts <- format(Sys.time(), "%Y-%m-%dT%H%M%SZ", tz = "UTC")
      dst <- file.path(archive_dir, sprintf("%s.%s.bak", bn, ts))
    }
    file.copy(src, dst, overwrite = FALSE)
    results[i] <- dst
    log[[length(log) + 1L]] <- list(
      src = src, dst = dst, md5 = unname(src_md5),
      ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    )
  }
  if (length(log)) {
    log_path <- file.path(archive_dir, "backup_log.json")
    prev <- if (file.exists(log_path))
      jsonlite::fromJSON(log_path, simplifyVector = FALSE) else list()
    all_log <- c(prev, log)
    jsonlite::write_json(all_log, log_path, pretty = TRUE, auto_unbox = TRUE)
  }
  results
}

# NOTE: an earlier prototype shipped a `promote_staged_assets()` helper that
# manually moved files from `<dest>/_new/` to `<dest>/`. With the cue=always
# pattern on `dashboard_data_dest` and `processed_cache_dest` in `_targets.R`,
# cutover happens automatically: setting `PNADC_PIPELINE_MODE=live` on the
# next `tar_make()` rebuilds Layer 3 directly at the live path. The manual
# helper is therefore unused.
