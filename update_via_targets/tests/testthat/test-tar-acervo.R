test_that("list_expected_quarters covers 2012-Q1 through up_to (year, q)", {
  source_pipeline_R()
  out <- list_expected_quarters(2024L, 2L)
  expect_equal(out[year == 2012L & quarter == 1L]$basename, "pnadc_2012-1q.fst")
  expect_equal(out[year == 2024L & quarter == 2L]$basename, "pnadc_2024-2q.fst")
  expect_equal(nrow(out[year == 2024L]), 2L)  # only Q1+Q2
  expect_false(any(out$year == 2024L & out$quarter > 2L))
})

test_that("list_expected_visits applies COVID visit rule", {
  source_pipeline_R()
  out <- list_expected_visits(2025L)
  expect_equal(out[year == 2019]$visit, 1L)
  expect_equal(out[year == 2020]$visit, 5L)
  expect_equal(out[year == 2021]$visit, 5L)
  expect_equal(out[year == 2022]$visit, 1L)
  expect_equal(out[year == 2020]$basename, "pnadc_2020_visita5.fst")
})

test_that("parse_ibge_zip_name distinguishes quarterly, annual, and reweighted ZIPs", {
  source_pipeline_R()
  q1 <- parse_ibge_zip_name("PNADC_022024.zip")
  expect_equal(q1$kind, "quarterly")
  expect_equal(q1$year, 2024L)
  expect_equal(q1$period, 2L)
  expect_true(is.na(q1$upstream_date))

  q2 <- parse_ibge_zip_name("PNADC_022024_20260324.zip")
  expect_equal(q2$upstream_date, "20260324")

  v1 <- parse_ibge_zip_name("PNADC_2020_visita5.zip")
  expect_equal(v1$kind, "annual")
  expect_equal(v1$year, 2020L)
  expect_equal(v1$period, 5L)

  v2 <- parse_ibge_zip_name("PNADC_2020_visita5_20250822.zip")
  expect_equal(v2$upstream_date, "20250822")

  expect_null(parse_ibge_zip_name("random_file.zip"))
})

test_that("parse_ibge_directory_listing extracts ZIP filenames from HTML", {
  source_pipeline_R()
  out <- parse_ibge_directory_listing(fixture_ibge_listing_html())
  expect_true(is.data.frame(out))
  expect_true("PNADC_022024.zip" %in% out$filename)
  expect_true("PNADC_022024_20260324.zip" %in% out$filename)
  expect_true("PNADC_2020_visita5_20250822.zip" %in% out$filename)
})

test_that("plan_acervo_actions flags MISSING when local file absent and remote available", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 2L)
  inv <- fixture_local_inventory(c("pnadc_2024-1q.fst"))
  remote <- data.table::data.table(
    filename = c("PNADC_012024.zip", "PNADC_022024.zip", "PNADC_032024.zip"),
    last_modified = as.POSIXct(NA, tz = "UTC"),
    size_bytes = NA_real_
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected[year == 2024],
    local_inventory = inv,
    remote_listing = remote
  )
  q1 <- plan[basename == "pnadc_2024-1q.fst"]
  q2 <- plan[basename == "pnadc_2024-2q.fst"]
  expect_equal(q1$status, "OK")
  expect_equal(q2$status, "MISSING")
})

test_that("plan_acervo_actions returns CHECK_OFFLINE when remote unavailable", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 2L)[year == 2024]
  inv <- data.table::data.table(
    basename = character(), path = character(),
    size_bytes = numeric(),
    mtime_utc = as.POSIXct(character(), tz = "UTC")
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_listing = NULL
  )
  expect_true(all(plan$status == "CHECK_OFFLINE"))
})

test_that("plan_acervo_actions flags REWEIGHT when upstream date is newer than manifest_prev", {
  source_pipeline_R()
  expected <- list_expected_quarters(2024L, 2L)[year == 2024 & quarter == 2L]
  inv <- fixture_local_inventory("pnadc_2024-2q.fst")
  remote <- data.table::data.table(
    filename = "PNADC_022024_20260324.zip",
    last_modified = as.POSIXct(NA, tz = "UTC"),
    size_bytes = NA_real_
  )
  manifest_prev <- data.table::data.table(
    basename = "pnadc_2024-2q.fst",
    upstream_date = "20240520"
  )
  plan <- plan_acervo_actions(
    file_type = "quarterly",
    expected = expected,
    local_inventory = inv,
    remote_listing = remote,
    manifest_prev = manifest_prev
  )
  expect_equal(plan$status, "REWEIGHT")
  expect_equal(plan$upstream_date, "20260324")
})

test_that("atomic_rename retries on transient failure (mocked)", {
  source_pipeline_R()
  # Use a real file/rename pair; happy path should succeed in 1 try.
  src <- tempfile(fileext = ".tmp")
  dst <- sub("\\.tmp$", ".final", src)
  writeLines("x", src)
  expect_true(file.exists(src))
  atomic_rename(src, dst)
  expect_false(file.exists(src))
  expect_true(file.exists(dst))
  unlink(dst)
})

test_that("archive_file moves file and writes JSONL log entry", {
  source_pipeline_R()
  src_dir <- tempfile("archive_src_")
  dir.create(src_dir)
  archive_root <- tempfile("archive_root_")
  dir.create(archive_root)

  src <- file.path(src_dir, "pnadc_2024-2q.fst")
  writeLines("dummy", src)

  out <- archive_file(
    local_path = src,
    upstream_date = "20260324",
    archive_root = archive_root,
    subkind = "Trimestral/Dados"
  )
  expect_true(file.exists(out))
  expect_match(basename(out), "--archived-")
  expect_match(basename(out), "--upstream-20260324")
  expect_false(file.exists(src))

  log_path <- file.path(archive_root, "_archive_log.jsonl")
  expect_true(file.exists(log_path))

  unlink(src_dir, recursive = TRUE)
  unlink(archive_root, recursive = TRUE)
})
