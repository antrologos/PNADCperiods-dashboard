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

# Synthetic annual microdata respecting the two real-world schemas.
# `schema = "full"` mirrors 2024-and-earlier visit 1 (with VD5008 + V5*A2).
# `schema = "simplified_2025"` mirrors PNADC anual 2025 visita 1 (no VD5008,
# no V5*A2 — only VD4019/VD4020 among income variables). Lower-case names
# match the convention set by load_annual_with_income_harmonization.
fixture_annual_microdata <- function(year,
                                     n = 200L,
                                     schema = c("full", "simplified_2025"),
                                     seed = 1L) {
  schema <- match.arg(schema)
  set.seed(seed)
  ufs <- c(11L, 33L, 35L, 41L, 53L)
  d <- data.table::data.table(
    ano = as.integer(year),
    trimestre = sample.int(4L, n, replace = TRUE),
    upa = sample.int(50L, n, replace = TRUE) + 110000L,
    v1008 = sample.int(20L, n, replace = TRUE),
    v1014 = sample.int(5L, n, replace = TRUE),
    v2005 = sample(c(1L, 4L, 5L, 6L), n, replace = TRUE),
    v2007 = sample(c(1L, 2L), n, replace = TRUE),
    v2009 = sample(0:90, n, replace = TRUE),
    v2010 = sample(c(1L, 2L, 4L), n, replace = TRUE),
    vd3004 = sample.int(7L, n, replace = TRUE),
    v1022 = sample(c(1L, 2L), n, replace = TRUE),
    uf = sample(ufs, n, replace = TRUE),
    estrato = sample.int(20L, n, replace = TRUE) + 110000L,
    v1032 = runif(n, 50, 500),
    posest = "1",
    posest_sxi = "1",
    vd4019 = ifelse(runif(n) < 0.55, NA_real_, runif(n, 800, 8000)),
    vd4020 = ifelse(runif(n) < 0.55, NA_real_, runif(n, 800, 8000))
  )
  if (schema == "full") {
    d[, vd5008 := runif(n, 200, 5000)]
    for (v in c("v5001a2", "v5002a2", "v5003a2", "v5004a2",
                "v5005a2", "v5006a2", "v5007a2", "v5008a2")) {
      d[, (v) := ifelse(runif(n) < 0.85, 0, runif(n, 50, 1500))]
    }
  }
  d[]
}
