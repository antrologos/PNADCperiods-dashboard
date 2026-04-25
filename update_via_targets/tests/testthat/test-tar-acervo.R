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
