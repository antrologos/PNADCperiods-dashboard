test_that("validate_dashboard_asset accepts a well-formed inequality stub", {
  source_pipeline_R()
  d <- data.table::data.table(
    ref_month_yyyymm = rep(202301L, 1100L),
    breakdown_type = "overall",
    breakdown_value = "Nacional",
    measure = "gini",
    value = runif(1100L),
    n_obs = 100L,
    period = as.Date("2023-01-15")
  )
  tmp <- tempfile(fileext = ".rds")
  saveRDS(d, tmp)
  res <- validate_dashboard_asset(tmp, "inequality_data")
  expect_true(res$ok)
  unlink(tmp)
})

test_that("validate_dashboard_asset rejects when columns are missing", {
  source_pipeline_R()
  d <- data.frame(x = 1:10)
  tmp <- tempfile(fileext = ".rds")
  saveRDS(d, tmp)
  res <- validate_dashboard_asset(tmp, "poverty_data")
  expect_false(res$ok)
  expect_match(res$reason, "missing columns")
  unlink(tmp)
})

test_that("validate_dashboard_asset rejects below-min-rows", {
  source_pipeline_R()
  d <- data.table::data.table(
    ref_month_yyyymm = 202301L,
    breakdown_type = "overall",
    breakdown_value = "Nacional",
    measure = "gini",
    value = 0.5,
    n_obs = 10L,
    period = as.Date("2023-01-15")
  )
  tmp <- tempfile(fileext = ".rds")
  saveRDS(d, tmp)
  res <- validate_dashboard_asset(tmp, "inequality_data")
  expect_false(res$ok)
  expect_match(res$reason, "rows")
  unlink(tmp)
})

test_that("validate_all_assets aborts with detailed message on failure", {
  source_pipeline_R()
  good_path <- tempfile(fileext = ".rds")
  saveRDS(
    data.table::data.table(
      uf_code = "11", uf_abbrev = "RO", uf_name = "Rondonia",
      geometry = list(NULL)
    )[rep(1L, 27L)],
    good_path
  )
  bad_path <- tempfile(fileext = ".rds")
  saveRDS(data.frame(z = 1), bad_path)

  expect_error(
    validate_all_assets(list(
      brazil_states_sf = good_path,
      poverty_data = bad_path
    )),
    "validation failed"
  )

  unlink(c(good_path, bad_path))
})
