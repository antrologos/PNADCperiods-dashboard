# ==============================================================================
# Test helpers for the targets pipeline
#
# Tests run from update_via_targets/ as working directory; helpers source
# the R/tar-*.R files exactly the way _targets.R does.
# ==============================================================================

source_pipeline_R <- function() {
  pipeline_root <- testthat::test_path("..", "..")
  for (f in list.files(file.path(pipeline_root, "R"),
                       pattern = "^tar-.*\\.R$", full.names = TRUE)) {
    sys.source(f, envir = globalenv())
  }
}

# Minimal local inventory data.table (in-memory, no disk)
fixture_local_inventory <- function(basenames, dir = "/tmp/fixture") {
  data.table::data.table(
    basename = basenames,
    path = file.path(dir, basenames),
    size_bytes = rep(800e6, length(basenames)),
    mtime_utc = rep(as.POSIXct("2024-01-01", tz = "UTC"), length(basenames))
  )
}
