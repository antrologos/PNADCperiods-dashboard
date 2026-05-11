test_that("list_expected_quarters covers 2012-Q1 through up_to (year, q)", {
  source_pipeline_R()
  out <- list_expected_quarters(2024L, 2L)
  expect_equal(out[year == 2012L & quarter == 1L]$basename, "pnadc_2012-1q.fst")
  expect_equal(out[year == 2024L & quarter == 2L]$basename, "pnadc_2024-2q.fst")
  expect_equal(nrow(out[year == 2024L]), 2L)  # only Q1+Q2
  expect_false(any(out$year == 2024L & out$quarter > 2L))
  # canonical join key
  expect_true("period" %in% names(out))
  expect_true(all(out$period == out$quarter))
})

test_that("list_expected_visits applies COVID visit rule", {
  source_pipeline_R()
  out <- list_expected_visits(2025L)
  expect_equal(out[year == 2019]$visit, 1L)
  expect_equal(out[year == 2020]$visit, 5L)
  expect_equal(out[year == 2021]$visit, 5L)
  expect_equal(out[year == 2022]$visit, 1L)
  expect_equal(out[year == 2020]$basename, "pnadc_2020_visita5.fst")
  expect_true("period" %in% names(out))
  expect_true(all(out$period == out$visit))
})

test_that("list_expected_visits with visits=1:5 enumerates all visits per year", {
  source_pipeline_R()
  out <- list_expected_visits(2025L, visits = 1L:5L)
  # 13 years (2012..2024) x 5 visits = 65 rows
  expect_equal(nrow(out), 13L * 5L)
  expect_equal(out[year == 2020 & visit == 1L]$basename,
               "pnadc_2020_visita1.fst")
  expect_equal(out[year == 2020 & visit == 5L]$basename,
               "pnadc_2020_visita5.fst")
  # All 5 visits for 2024
  expect_setequal(out[year == 2024]$visit, 1L:5L)
})

test_that("inventory_local returns empty data.table when directory absent", {
  source_pipeline_R()
  out <- inventory_local("/nonexistent/path/xyz", pattern = ".*")
  expect_s3_class(out, "data.table")
  expect_equal(nrow(out), 0L)
  expect_named(out, c("basename", "path", "size_bytes", "mtime_utc"))
})

test_that("inventory_local lists matching files with metadata", {
  source_pipeline_R()
  tmp <- tempfile("inv_dir_")
  dir.create(tmp)
  f1 <- file.path(tmp, "pnadc_2024-1q.fst")
  f2 <- file.path(tmp, "pnadc_2024-2q.fst")
  writeLines("dummy", f1)
  writeLines("dummy", f2)
  # Decoy file that should not match
  writeLines("dummy", file.path(tmp, "deflator_pnadc_2024.xls"))

  out <- inventory_local(tmp, pattern = "^pnadc_\\d{4}-[1-4]q\\.fst$")
  expect_equal(nrow(out), 2L)
  expect_setequal(out$basename, c("pnadc_2024-1q.fst", "pnadc_2024-2q.fst"))
  expect_true(all(out$size_bytes > 0))

  unlink(tmp, recursive = TRUE)
})

test_that("plan_acervo_actions flags MISSING when local file absent", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 2L)
  expected <- expected[year == 2024L]
  inv <- fixture_local_inventory("pnadc_2024-1q.fst")
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv
  )
  q1 <- plan[basename == "pnadc_2024-1q.fst"]
  q2 <- plan[basename == "pnadc_2024-2q.fst"]
  expect_equal(q1$status, "OK")
  expect_equal(q2$status, "MISSING")
  # OK row should carry local_path; MISSING row should not
  expect_false(is.na(q1$local_path))
  expect_true(is.na(q2$local_path))
})

test_that("plan_acervo_actions returns all OK when every expected file present", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 2L)
  expected <- expected[year == 2024L]
  inv <- fixture_local_inventory(c("pnadc_2024-1q.fst", "pnadc_2024-2q.fst"))
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv
  )
  expect_true(all(plan$status == "OK"))
})

test_that("plan_acervo_actions returns all MISSING when local inventory empty", {
  source_pipeline_R()
  expected <- list_expected_visits(2025L)
  expected <- expected[year %in% 2020:2021]
  inv <- data.table::data.table(
    basename = character(), path = character(),
    size_bytes = numeric(),
    mtime_utc = as.POSIXct(character(), tz = "UTC")
  )
  plan <- plan_acervo_actions(
    file_type = "annual",
    expected = expected,
    local_inventory = inv
  )
  expect_true(all(plan$status == "MISSING"))
  # Visit rule still applied
  expect_equal(plan[year == 2020]$period, 5L)
  expect_equal(plan[year == 2021]$period, 5L)
})

# -----------------------------------------------------------------------------
# Tests: plan_acervo_actions with FTP catalog (MISSING_UPSTREAM, OUTDATED)
# -----------------------------------------------------------------------------

test_that("remote_catalog with new file flags MISSING when local absent", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  inv <- data.table::data.table(
    basename = character(), path = character(),
    size_bytes = numeric(),
    mtime_utc = as.POSIXct(character(), tz = "UTC")
  )
  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024_20250815.zip",
      last_modified = as.POSIXct("2025-08-15 10:23", tz = "UTC"),
      size_bytes = 2.1e8,
      year = 2024L, quarter = 1L,
      upstream_date = "20250815"
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote
  )
  expect_equal(plan$status, "MISSING")
  expect_equal(plan$upstream_filename, "PNADC_012024_20250815.zip")
})

test_that("remote_catalog absent for expected entry => MISSING_UPSTREAM", {
  source_pipeline_R()
  expected <- list_expected_quarters(2099L, 1L)  # impossible far-future Q
  expected <- expected[year == 2099L]
  inv <- data.table::data.table(
    basename = character(), path = character(),
    size_bytes = numeric(),
    mtime_utc = as.POSIXct(character(), tz = "UTC")
  )
  remote <- list()  # no entry for 2099
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote
  )
  expect_equal(plan$status, "MISSING_UPSTREAM")
})

test_that("OUTDATED detected when remote filename differs from sidecar", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  # Local present
  tmp <- tempfile()
  dir.create(tmp)
  fst_path <- file.path(tmp, "pnadc_2024-1q.fst")
  writeLines("x", fst_path)
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024_20260424.zip",   # NEW reweighted name
      last_modified = as.POSIXct("2026-04-24 09:00", tz = "UTC"),
      size_bytes = 2.1e8, year = 2024L, quarter = 1L,
      upstream_date = "20260424"
    )
  )
  sidecar <- list(
    "pnadc_2024-1q.fst" = list(
      upstream_filename = "PNADC_012024_20250815.zip",  # OLD
      upstream_last_modified = "2025-08-15T10:23:00Z"
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote,
    catalog_sidecar = sidecar
  )
  expect_equal(plan$status, "OUTDATED")
  expect_match(plan$reason, "filename changed", fixed = TRUE)

  unlink(tmp, recursive = TRUE)
})

test_that("OUTDATED detected when filename same but Last-Modified advanced", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  tmp <- tempfile(); dir.create(tmp)
  writeLines("x", file.path(tmp, "pnadc_2024-1q.fst"))
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024.zip",     # SAME name
      last_modified = as.POSIXct("2026-04-24 09:00", tz = "UTC"),  # NEW date
      size_bytes = 2.1e8, year = 2024L, quarter = 1L,
      upstream_date = NA_character_
    )
  )
  sidecar <- list(
    "pnadc_2024-1q.fst" = list(
      upstream_filename = "PNADC_012024.zip",     # same name in sidecar
      upstream_last_modified = "2025-08-15T10:23:00Z"  # OLD date
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote,
    catalog_sidecar = sidecar
  )
  expect_equal(plan$status, "OUTDATED")
  expect_match(plan$reason, "Last-Modified advanced", fixed = TRUE)

  unlink(tmp, recursive = TRUE)
})

test_that("bootstrap: no sidecar + local present + remote present => OK + capture", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  tmp <- tempfile(); dir.create(tmp)
  writeLines("x", file.path(tmp, "pnadc_2024-1q.fst"))
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024_20250815.zip",
      last_modified = as.POSIXct("2025-08-15 10:23", tz = "UTC"),
      size_bytes = 2.1e8, year = 2024L, quarter = 1L,
      upstream_date = "20250815"
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote,
    catalog_sidecar = NULL  # empty / first-run
  )
  expect_equal(plan$status, "OK")
  expect_match(plan$reason, "bootstrap", fixed = TRUE)
  # Bootstrap captures upstream identity for sidecar update by apply step
  expect_equal(plan$upstream_filename, "PNADC_012024_20250815.zip")

  unlink(tmp, recursive = TRUE)
})

# -----------------------------------------------------------------------------
# Bootstrap mtime-based comparison: detect outdated local files when sidecar
# is empty (first run after a sidecar reset).
# -----------------------------------------------------------------------------

test_that("bootstrap: local mtime older than FTP Last-Modified => OUTDATED", {
  source_pipeline_R()
  expected <- list_expected_visits(2026L, visits = 1L)
  expected <- expected[year == 2025L]
  tmp <- tempfile(); dir.create(tmp)
  fpath <- file.path(tmp, "pnadc_2025_visita1.fst")
  writeLines("x", fpath)
  # Force local mtime to 2026-04-24 (the OLD simplified IBGE publication)
  Sys.setFileTime(fpath, as.POSIXct("2026-04-24 10:00", tz = "UTC"))
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  # FTP has the NEW republication from 2026-05-08
  remote <- list(
    "1" = data.table::data.table(
      filename = "PNADC_2025_visita1_20260508.zip",
      last_modified = as.POSIXct("2026-05-08 10:00", tz = "UTC"),
      size_bytes = 1.9e8, year = 2025L, visit = 1L,
      upstream_date = "20260508"
    )
  )

  plan <- plan_acervo_actions(
    file_type = "annual",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote,
    catalog_sidecar = NULL  # empty sidecar
  )
  expect_equal(plan$status, "OUTDATED")
  expect_match(plan$reason, "bootstrap.*Last-Modified")
  # Upstream identity captured so apply step can rename + redownload
  expect_equal(plan$upstream_filename, "PNADC_2025_visita1_20260508.zip")

  unlink(tmp, recursive = TRUE)
})

test_that("bootstrap: local mtime within 1d of FTP Last-Modified => OK (no false positive)", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  tmp <- tempfile(); dir.create(tmp)
  fpath <- file.path(tmp, "pnadc_2024-1q.fst")
  writeLines("x", fpath)
  # local mtime exactly 12 hours before FTP last_modified — within epsilon
  Sys.setFileTime(fpath, as.POSIXct("2025-08-14 22:00", tz = "UTC"))
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024.zip",
      last_modified = as.POSIXct("2025-08-15 10:00", tz = "UTC"),
      size_bytes = 2.1e8, year = 2024L, quarter = 1L,
      upstream_date = NA_character_
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote,
    catalog_sidecar = NULL
  )
  expect_equal(plan$status, "OK")
  expect_match(plan$reason, "bootstrap", fixed = TRUE)

  unlink(tmp, recursive = TRUE)
})

test_that("bootstrap: missing FTP Last-Modified => OK (cannot compare, fallback)", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  tmp <- tempfile(); dir.create(tmp)
  writeLines("x", file.path(tmp, "pnadc_2024-1q.fst"))
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  # FTP entry exists but Last-Modified is NA (rare — server quirk)
  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024.zip",
      last_modified = as.POSIXct(NA, tz = "UTC"),
      size_bytes = 2.1e8, year = 2024L, quarter = 1L,
      upstream_date = NA_character_
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote,
    catalog_sidecar = NULL
  )
  expect_equal(plan$status, "OK")
  expect_match(plan$reason, "bootstrap", fixed = TRUE)

  unlink(tmp, recursive = TRUE)
})

# -----------------------------------------------------------------------------
# Phase 3: detect schema drift in local .fst when required_vars expanded
# (e.g. VD4016 added to quarterly_required_vars on 2026-05-04 — old .fst
# files don't have the new column; pipeline must detect and re-download).
# -----------------------------------------------------------------------------

# Helper: write a tiny but valid .fst file with the given columns
.write_test_fst <- function(path, columns) {
  d <- data.table::as.data.table(setNames(
    lapply(columns, function(.) integer(1L)),
    columns
  ))
  fst::write_fst(d, path, compress = 50)
}

test_that("schema drift bootstrap: local lacks required column => OUTDATED", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  tmp <- tempfile(); dir.create(tmp)
  fpath <- file.path(tmp, "pnadc_2024-1q.fst")
  # OLD-schema .fst: lacks VD4016
  .write_test_fst(fpath, c("Ano","Trimestre","UPA","V1008","V2009"))
  Sys.setFileTime(fpath, as.POSIXct("2026-04-25 20:00", tz = "UTC"))
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024.zip",
      last_modified = as.POSIXct("2025-08-15 10:00", tz = "UTC"),
      size_bytes = 2.1e8, year = 2024L, quarter = 1L,
      upstream_date = NA_character_
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote,
    catalog_sidecar = NULL,
    required_vars = c("Ano","Trimestre","UPA","V1008","V2009","VD4016")
  )
  expect_equal(plan$status, "OUTDATED")
  expect_match(plan$reason, "schema drift", ignore.case = TRUE)
  expect_match(plan$reason, "VD4016")

  unlink(tmp, recursive = TRUE)
})

test_that("schema OK: local has all required columns => OK", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  tmp <- tempfile(); dir.create(tmp)
  fpath <- file.path(tmp, "pnadc_2024-1q.fst")
  .write_test_fst(fpath, c("Ano","Trimestre","UPA","V1008","V2009","VD4016"))
  Sys.setFileTime(fpath, as.POSIXct("2026-04-25 20:00", tz = "UTC"))
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024.zip",
      last_modified = as.POSIXct("2025-08-15 10:00", tz = "UTC"),
      size_bytes = 2.1e8, year = 2024L, quarter = 1L,
      upstream_date = NA_character_
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote,
    catalog_sidecar = NULL,
    required_vars = c("Ano","Trimestre","UPA","V1008","V2009","VD4016")
  )
  expect_equal(plan$status, "OK")

  unlink(tmp, recursive = TRUE)
})

test_that("schema drift normal: sidecar with stale required_vars_hash => OUTDATED", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  tmp <- tempfile(); dir.create(tmp)
  fpath <- file.path(tmp, "pnadc_2024-1q.fst")
  .write_test_fst(fpath, c("Ano","Trimestre","UPA","V1008","V2009"))  # no VD4016
  Sys.setFileTime(fpath, as.POSIXct("2026-04-25 20:00", tz = "UTC"))
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024.zip",
      last_modified = as.POSIXct("2025-08-15 10:00", tz = "UTC"),
      size_bytes = 2.1e8, year = 2024L, quarter = 1L,
      upstream_date = NA_character_
    )
  )
  # Sidecar entry exists with OLD required_vars_hash (no VD4016)
  old_required <- c("Ano","Trimestre","UPA","V1008","V2009")
  sidecar <- list(
    "pnadc_2024-1q.fst" = list(
      upstream_filename = "PNADC_012024.zip",
      upstream_last_modified = "2025-08-15T10:00:00Z",
      actual_columns = old_required,
      required_vars_hash = digest::digest(sort(old_required))
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote,
    catalog_sidecar = sidecar,
    required_vars = c(old_required, "VD4016")  # expanded
  )
  expect_equal(plan$status, "OUTDATED")
  expect_match(plan$reason, "schema drift", ignore.case = TRUE)

  unlink(tmp, recursive = TRUE)
})

test_that("schema drift already tried: same required_vars_hash, still missing => OK (no loop)", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  tmp <- tempfile(); dir.create(tmp)
  fpath <- file.path(tmp, "pnadc_2024-1q.fst")
  .write_test_fst(fpath, c("Ano","Trimestre","UPA","V1008","V2009"))  # no VD4016
  Sys.setFileTime(fpath, as.POSIXct("2026-05-10 12:00", tz = "UTC"))
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024.zip",
      last_modified = as.POSIXct("2025-08-15 10:00", tz = "UTC"),
      size_bytes = 2.1e8, year = 2024L, quarter = 1L,
      upstream_date = NA_character_
    )
  )
  # Sidecar already records: "tried with current required_vars, IBGE doesn't have VD4016"
  current_required <- c("Ano","Trimestre","UPA","V1008","V2009","VD4016")
  sidecar <- list(
    "pnadc_2024-1q.fst" = list(
      upstream_filename = "PNADC_012024.zip",
      upstream_last_modified = "2025-08-15T10:00:00Z",
      actual_columns = c("Ano","Trimestre","UPA","V1008","V2009"),  # without VD4016
      required_vars_hash = digest::digest(sort(current_required))
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote,
    catalog_sidecar = sidecar,
    required_vars = current_required
  )
  expect_equal(plan$status, "OK")
  expect_match(plan$reason, "already attempted", ignore.case = TRUE)

  unlink(tmp, recursive = TRUE)
})

test_that("schema OK: required_vars=NULL preserves backward-compat (no schema check)", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  tmp <- tempfile(); dir.create(tmp)
  # writeLines (text file, NOT a real .fst) — schema check would fail to read.
  # With required_vars=NULL the plan must NOT attempt fst::metadata_fst.
  writeLines("x", file.path(tmp, "pnadc_2024-1q.fst"))
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024.zip",
      last_modified = as.POSIXct("2025-08-15 10:00", tz = "UTC"),
      size_bytes = 2.1e8, year = 2024L, quarter = 1L,
      upstream_date = NA_character_
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote,
    catalog_sidecar = NULL
    # required_vars omitted → defaults to NULL
  )
  expect_equal(plan$status, "OK")

  unlink(tmp, recursive = TRUE)
})

test_that("OK when sidecar matches remote (filename + Last-Modified)", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  tmp <- tempfile(); dir.create(tmp)
  writeLines("x", file.path(tmp, "pnadc_2024-1q.fst"))
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024.zip",
      last_modified = as.POSIXct("2025-08-15 10:23", tz = "UTC"),
      size_bytes = 2.1e8, year = 2024L, quarter = 1L,
      upstream_date = NA_character_
    )
  )
  sidecar <- list(
    "pnadc_2024-1q.fst" = list(
      upstream_filename = "PNADC_012024.zip",
      upstream_last_modified = "2025-08-15T10:23:00Z"
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_catalog = remote,
    catalog_sidecar = sidecar
  )
  expect_equal(plan$status, "OK")

  unlink(tmp, recursive = TRUE)
})

# -----------------------------------------------------------------------------
# Tests: sidecar I/O
# -----------------------------------------------------------------------------

test_that("acervo_sidecar round-trips through JSON", {
  source_pipeline_R()
  tmp <- tempfile(fileext = ".json")
  s <- list(
    "pnadc_2024-1q.fst" = list(
      upstream_filename = "PNADC_012024_20250815.zip",
      upstream_last_modified = "2025-08-15T10:23:00Z"
    )
  )
  save_acervo_sidecar(s, tmp)
  back <- load_acervo_sidecar(tmp)
  expect_equal(back$`pnadc_2024-1q.fst`$upstream_filename,
               "PNADC_012024_20250815.zip")
  unlink(tmp)
})

test_that("update_acervo_sidecar adds and overwrites entries", {
  source_pipeline_R()
  s <- list()
  s <- update_acervo_sidecar(s, "x.fst", "X_v1.zip",
                              as.POSIXct("2025-01-01 00:00", tz = "UTC"))
  expect_equal(s$x.fst$upstream_filename, "X_v1.zip")
  s <- update_acervo_sidecar(s, "x.fst", "X_v2.zip",
                              as.POSIXct("2026-01-01 00:00", tz = "UTC"))
  expect_equal(s$x.fst$upstream_filename, "X_v2.zip")
})

test_that("atomic_rename succeeds in happy path", {
  source_pipeline_R()
  src <- tempfile(fileext = ".tmp")
  dst <- sub("\\.tmp$", ".final", src)
  writeLines("x", src)
  expect_true(file.exists(src))
  atomic_rename(src, dst)
  expect_false(file.exists(src))
  expect_true(file.exists(dst))
  unlink(dst)
})

test_that("atomic_rename overwrites pre-existing destination (Windows-safe)", {
  source_pipeline_R()
  src <- tempfile(fileext = ".tmp")
  dst <- sub("\\.tmp$", ".final", src)
  writeLines("new", src)
  writeLines("old", dst)
  expect_true(file.exists(dst))
  atomic_rename(src, dst)
  expect_false(file.exists(src))
  expect_true(file.exists(dst))
  expect_equal(readLines(dst), "new")
  unlink(dst)
})

test_that("is_empty_pnadc_response detects NULL", {
  source_pipeline_R()
  expect_true(is_empty_pnadc_response(NULL))
})

test_that("is_empty_pnadc_response detects 0-row data.frame", {
  source_pipeline_R()
  expect_true(is_empty_pnadc_response(data.frame()))
  expect_true(is_empty_pnadc_response(data.table::data.table()))
  expect_true(is_empty_pnadc_response(data.frame(x = integer(0))))
})

test_that("is_empty_pnadc_response returns FALSE for populated data.frame", {
  source_pipeline_R()
  expect_false(is_empty_pnadc_response(data.frame(x = 1L)))
  expect_false(is_empty_pnadc_response(data.table::data.table(x = 1:5)))
})

test_that("download_quarter and download_visit reference is_empty_pnadc_response (regression)", {
  source_pipeline_R()
  q_body <- paste(deparse(body(download_quarter)), collapse = "\n")
  v_body <- paste(deparse(body(download_visit)), collapse = "\n")
  expect_match(q_body, "is_empty_pnadc_response")
  expect_match(v_body, "is_empty_pnadc_response")
})

test_that("ensure_deflator_downloaded is idempotent when file already present", {
  source_pipeline_R()
  tmp_root <- tempfile("acervo_")
  dir.create(file.path(tmp_root, "Anual", "visitas", "documentacao"),
             recursive = TRUE)
  existing <- file.path(tmp_root, "Anual", "visitas", "documentacao",
                        "deflator_PNADC_2025.xls")
  writeLines("dummy-xls-content", existing)

  called <- FALSE
  fake_download <- function(year, dest_path) {
    called <<- TRUE
    writeLines("should-not-overwrite", dest_path)
    dest_path
  }

  out <- ensure_deflator_downloaded(2025L, tmp_root,
                                    download_fn = fake_download)
  expect_equal(out, existing)
  expect_false(called)
  expect_equal(readLines(existing), "dummy-xls-content")

  unlink(tmp_root, recursive = TRUE)
})

test_that("ensure_deflator_downloaded skips download in dry-run mode", {
  source_pipeline_R()
  tmp_root <- tempfile("acervo_")

  called <- FALSE
  fake_download <- function(year, dest_path) {
    called <<- TRUE
    dest_path
  }

  withr::with_envvar(c(ACERVO_DRY_RUN = "1"), {
    out <- ensure_deflator_downloaded(2025L, tmp_root,
                                      download_fn = fake_download)
  })
  expect_match(out, "deflator_PNADC_2025\\.xls$")
  expect_false(called)
  expect_false(file.exists(out))

  unlink(tmp_root, recursive = TRUE)
})

test_that("ensure_deflator_downloaded calls download_fn when file missing", {
  source_pipeline_R()
  tmp_root <- tempfile("acervo_")

  called <- FALSE
  call_args <- list()
  fake_download <- function(year, dest_path) {
    called <<- TRUE
    call_args <<- list(year = year, dest_path = dest_path)
    dir.create(dirname(dest_path), recursive = TRUE, showWarnings = FALSE)
    writeLines("downloaded-content", dest_path)
    dest_path
  }

  out <- ensure_deflator_downloaded(2025L, tmp_root,
                                    download_fn = fake_download)
  expect_true(called)
  expect_equal(call_args$year, 2025L)
  expect_match(call_args$dest_path,
               "Anual.*visitas.*documentacao.*deflator_PNADC_2025\\.xls$")
  expect_true(file.exists(out))

  unlink(tmp_root, recursive = TRUE)
})

test_that("extract_and_sync_deflators copies new files into empty dest", {
  source_pipeline_R()
  zip_src <- tempfile("zsrc_"); dir.create(zip_src)
  on.exit(unlink(zip_src, recursive = TRUE), add = TRUE)
  writeLines("contents-A", file.path(zip_src, "deflator_PNADC_2024_trimestral_a.xls"))
  writeLines("contents-B", file.path(zip_src, "deflator_PNADC_2025_trimestral_b.xls"))
  zip_path <- tempfile(fileext = ".zip")
  withr::with_dir(zip_src, utils::zip(zip_path, list.files(".")))
  on.exit(unlink(zip_path), add = TRUE)

  dest_dir <- tempfile("dest_"); dir.create(dest_dir)
  on.exit(unlink(dest_dir, recursive = TRUE), add = TRUE)

  copied <- extract_and_sync_deflators(zip_path, dest_dir)
  expect_setequal(basename(copied),
                  c("deflator_PNADC_2024_trimestral_a.xls",
                    "deflator_PNADC_2025_trimestral_b.xls"))
  expect_true(file.exists(file.path(dest_dir, "deflator_PNADC_2024_trimestral_a.xls")))
})

test_that("extract_and_sync_deflators replaces local file when size differs", {
  source_pipeline_R()
  zip_src <- tempfile("zsrc_"); dir.create(zip_src)
  on.exit(unlink(zip_src, recursive = TRUE), add = TRUE)
  writeLines("new-content-with-more-bytes",
             file.path(zip_src, "deflator_PNADC_2025_trimestral_x.xls"))
  zip_path <- tempfile(fileext = ".zip")
  withr::with_dir(zip_src, utils::zip(zip_path, list.files(".")))
  on.exit(unlink(zip_path), add = TRUE)

  dest_dir <- tempfile("dest_"); dir.create(dest_dir)
  on.exit(unlink(dest_dir, recursive = TRUE), add = TRUE)
  writeLines("old", file.path(dest_dir, "deflator_PNADC_2025_trimestral_x.xls"))
  before_size <- file.size(file.path(dest_dir, "deflator_PNADC_2025_trimestral_x.xls"))

  copied <- extract_and_sync_deflators(zip_path, dest_dir)
  expect_length(copied, 1L)
  after_size <- file.size(file.path(dest_dir, "deflator_PNADC_2025_trimestral_x.xls"))
  expect_gt(after_size, before_size)
})

test_that("extract_and_sync_deflators skips when local matches", {
  source_pipeline_R()
  zip_src <- tempfile("zsrc_"); dir.create(zip_src)
  on.exit(unlink(zip_src, recursive = TRUE), add = TRUE)
  payload <- "identical-bytes"
  writeLines(payload, file.path(zip_src, "deflator_PNADC_2025_trimestral_y.xls"))
  zip_path <- tempfile(fileext = ".zip")
  withr::with_dir(zip_src, utils::zip(zip_path, list.files(".")))
  on.exit(unlink(zip_path), add = TRUE)

  dest_dir <- tempfile("dest_"); dir.create(dest_dir)
  on.exit(unlink(dest_dir, recursive = TRUE), add = TRUE)
  local_path <- file.path(dest_dir, "deflator_PNADC_2025_trimestral_y.xls")
  writeLines(payload, local_path)
  before_mtime <- file.mtime(local_path)
  Sys.sleep(1.1)  # ensure detectable mtime delta if file is rewritten

  copied <- extract_and_sync_deflators(zip_path, dest_dir)
  expect_length(copied, 0L)
  expect_equal(file.mtime(local_path), before_mtime)
})

test_that("ensure_quarterly_deflators_downloaded honours dry-run", {
  source_pipeline_R()
  tmp_root <- tempfile("acervo_")
  called <- FALSE
  fake_download <- function(dest_path) {
    called <<- TRUE
    dest_path
  }
  withr::with_envvar(c(ACERVO_DRY_RUN = "1"), {
    out <- ensure_quarterly_deflators_downloaded(tmp_root,
                                                 download_fn = fake_download)
  })
  expect_match(out, "Trimestral.*Documentacao$")
  expect_false(called)
})

test_that("ensure_quarterly_deflators_downloaded calls download_fn and syncs", {
  source_pipeline_R()
  tmp_root <- tempfile("acervo_")
  on.exit(unlink(tmp_root, recursive = TRUE), add = TRUE)

  # fake_download will be invoked with a temp .zip dest_path; produce a
  # tiny synthetic ZIP at that location
  fake_download <- function(dest_path) {
    src <- tempfile("zsrc_"); dir.create(src)
    on.exit(unlink(src, recursive = TRUE), add = TRUE)
    writeLines("payload",
               file.path(src, "deflator_PNADC_2025_trimestral_z.xls"))
    withr::with_dir(src, utils::zip(dest_path, list.files(".")))
    dest_path
  }

  out <- ensure_quarterly_deflators_downloaded(tmp_root,
                                               download_fn = fake_download)
  expect_true(dir.exists(out))
  expect_true(file.exists(file.path(out, "deflator_PNADC_2025_trimestral_z.xls")))
})

test_that("apply_acervo_plan persists bootstrap-OK rows to sidecar (regression: <<- vs <- bug)", {
  # Earlier version used `sidecar <<- update_acervo_sidecar(...)` inside the
  # bootstrap loop. `<<-` searches the parent scope (globalenv), not the
  # function's parameter frame — so the LOCAL `sidecar` was never updated
  # and `save_acervo_sidecar(sidecar, sidecar_path)` always wrote `[]`.
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 1L)
  expected <- expected[year == 2024L]
  tmp <- tempfile(); dir.create(tmp)
  fpath <- file.path(tmp, "pnadc_2024-1q.fst")
  writeLines("x", fpath)
  inv <- inventory_local(tmp, "^pnadc_.*\\.fst$")

  remote <- list(
    "2024" = data.table::data.table(
      filename = "PNADC_012024.zip",
      last_modified = as.POSIXct("2025-08-15 10:00", tz = "UTC"),
      size_bytes = 2.1e8, year = 2024L, quarter = 1L,
      upstream_date = NA_character_
    )
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly", expected = expected,
    local_inventory = inv, remote_catalog = remote,
    catalog_sidecar = NULL  # empty sidecar
  )
  expect_equal(plan$status, "OK")  # bootstrap classified as OK

  sidecar_path <- file.path(tmp, ".acervo_catalog.json")
  result <- apply_acervo_plan(
    plan = plan,
    file_type = "quarterly",
    dest_dir = tmp,
    sidecar = list(),
    sidecar_path = sidecar_path
  )

  expect_true(file.exists(sidecar_path))
  saved <- jsonlite::read_json(sidecar_path, simplifyVector = FALSE)
  expect_length(saved, 1L)
  expect_true("pnadc_2024-1q.fst" %in% names(saved))
  expect_equal(saved[["pnadc_2024-1q.fst"]]$upstream_filename,
               "PNADC_012024.zip")

  unlink(tmp, recursive = TRUE)
})

test_that("apply_acervo_plan honours dry-run (no downloads, status preserved)", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 2L)
  expected <- expected[year == 2024L]
  inv <- data.table::data.table(
    basename = character(), path = character(),
    size_bytes = numeric(),
    mtime_utc = as.POSIXct(character(), tz = "UTC")
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv
  )
  withr::with_envvar(c(ACERVO_DRY_RUN = "1"), {
    out <- apply_acervo_plan(
      plan = plan,
      file_type = "quarterly",
      dest_dir = tempfile("dest_")
    )
  })
  expect_true(all(out$status == "MISSING"))
  expect_true(all(is.na(out$download_timestamp)))
})
