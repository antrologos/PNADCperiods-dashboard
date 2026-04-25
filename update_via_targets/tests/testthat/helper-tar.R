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

# Minimal IBGE FTP HTML listing
fixture_ibge_listing_html <- function() {
  '<html><body><pre><a href="../">../</a>
  <a href="PNADC_012024.zip">PNADC_012024.zip</a>      24-Feb-2024 14:23
  <a href="PNADC_022024.zip">PNADC_022024.zip</a>      24-May-2024 09:11
  <a href="PNADC_022024_20260324.zip">PNADC_022024_20260324.zip</a>  24-Mar-2026 09:11
  <a href="PNADC_032024.zip">PNADC_032024.zip</a>      24-Aug-2024 11:02
  <a href="PNADC_2020_visita5_20250822.zip">PNADC_2020_visita5_20250822.zip</a>  22-Aug-2025 16:00
  </pre></body></html>'
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
