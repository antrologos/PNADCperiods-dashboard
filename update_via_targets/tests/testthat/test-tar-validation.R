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

test_that("validate_dashboard_asset enforces measure_levels for inequality_data", {
  source_pipeline_R()
  d <- data.table::data.table(
    ref_month_yyyymm = rep(202301L, 1100L),
    breakdown_type = "overall",
    breakdown_value = "Nacional",
    measure = c(rep("gini", 1099L), "g_index"),  # one unknown measure
    value = runif(1100L),
    n_obs = 100L,
    period = as.Date("2023-01-15")
  )
  tmp <- tempfile(fileext = ".rds")
  saveRDS(d, tmp)
  res <- validate_dashboard_asset(tmp, "inequality_data")
  expect_false(res$ok)
  expect_match(res$reason, "unknown measure level")
  unlink(tmp)
})

test_that("validate_dashboard_asset accepts lorenz_data with p+lorenz columns", {
  source_pipeline_R()
  d <- data.table::data.table(
    ref_month_yyyymm = rep(202301L, 1100L),
    breakdown_type = "overall",
    breakdown_value = "Nacional",
    p = seq(0, 1, length.out = 1100L),
    lorenz = seq(0, 1, length.out = 1100L)^2
  )
  tmp <- tempfile(fileext = ".rds")
  saveRDS(d, tmp)
  res <- validate_dashboard_asset(tmp, "lorenz_data")
  expect_true(res$ok)
  unlink(tmp)
})

test_that("validate_dashboard_asset rejects lorenz_data missing p+lorenz columns", {
  source_pipeline_R()
  d <- data.table::data.table(
    ref_month_yyyymm = rep(202301L, 1100L),
    breakdown_type = "overall",
    breakdown_value = "Nacional"
    # missing p, lorenz
  )
  tmp <- tempfile(fileext = ".rds")
  saveRDS(d, tmp)
  res <- validate_dashboard_asset(tmp, "lorenz_data")
  expect_false(res$ok)
  expect_match(res$reason, "missing columns")
  unlink(tmp)
})

# ------------------------------------------------------------------------------
# detect_simplified_annual_year() — IBGE 2025 visita 1 simplified module
# ------------------------------------------------------------------------------

test_that("detect_simplified_annual_year flags 2025 simplified schema", {
  source_pipeline_R()
  d <- fixture_annual_microdata(2025L, schema = "simplified_2025")
  expect_true(detect_simplified_annual_year(d))
})

test_that("detect_simplified_annual_year accepts full schema (2024)", {
  source_pipeline_R()
  d <- fixture_annual_microdata(2024L, schema = "full")
  expect_false(detect_simplified_annual_year(d))
})

test_that("detect_simplified_annual_year flags 100% NA vd5008 even when V5xxx present", {
  # Catches a degenerate case where IBGE keeps the column header but ships
  # all-NA values (would yield hhinc_pc = 0 via the legacy fifelse).
  source_pipeline_R()
  d <- fixture_annual_microdata(2024L, schema = "full")
  d[, vd5008 := NA_real_]
  expect_true(detect_simplified_annual_year(d))
})

# ------------------------------------------------------------------------------
# Phase 2 — measures expansion (Phase 2-4) and 3 new quarterly assets (Phase 2-6)
# ------------------------------------------------------------------------------

# Phase 2-4 added 8 distribution-stat measures alongside the 9 inequality
# measures. Total grade emitted by build_inequality_outputs and
# build_quarterly_income_outputs:
phase2_measures <- c(
  # 9 inequality measures
  "gini", "palma", "p90p10", "p90p50", "p50p10",
  "top1_share", "top5_share", "top10_share", "bottom50_share",
  # 8 distribution stats (Phase 2-4)
  "mean", "min", "p10", "p25", "median", "p75", "p90", "max"
)

test_that("validate_dashboard_asset accepts inequality_data with Phase 2-4 measures", {
  source_pipeline_R()
  n <- length(phase2_measures) * 65L  # 1105 rows, > 1000 min
  d <- data.table::data.table(
    ref_month_yyyymm = rep(202301L, n),
    breakdown_type = "overall",
    breakdown_value = "Nacional",
    measure = rep(phase2_measures, length.out = n),
    value = runif(n),
    n_obs = 100L,
    period = as.Date("2023-01-15")
  )
  tmp <- tempfile(fileext = ".rds")
  saveRDS(d, tmp)
  res <- validate_dashboard_asset(tmp, "inequality_data")
  expect_true(res$ok, info = res$reason)
  unlink(tmp)
})

test_that("validate_dashboard_asset recognizes quarterly_income_data with Phase 2-4 measures", {
  source_pipeline_R()
  n <- length(phase2_measures) * 65L
  d <- data.table::data.table(
    ref_month_yyyymm = rep(202301L, n),
    breakdown_type = "overall",
    breakdown_value = "Nacional",
    measure = rep(phase2_measures, length.out = n),
    value = runif(n),
    n_obs = 100L,
    period = as.Date("2023-01-15"),
    income_var = "indiv_hab_princ"
  )
  tmp <- tempfile(fileext = ".rds")
  saveRDS(d, tmp)
  res <- validate_dashboard_asset(tmp, "quarterly_income_data")
  expect_true(res$ok, info = res$reason)
  unlink(tmp)
})

test_that("validate_dashboard_asset recognizes quarterly_income_shares_data", {
  source_pipeline_R()
  d <- data.table::data.table(
    ref_month_yyyymm = rep(202301L, 600L),
    breakdown_type = "overall",
    breakdown_value = "Nacional",
    group_type = "quintile",
    group_label = "Q1",
    share = runif(600L),
    period = as.Date("2023-01-15"),
    income_var = "indiv_hab_princ"
  )
  tmp <- tempfile(fileext = ".rds")
  saveRDS(d, tmp)
  res <- validate_dashboard_asset(tmp, "quarterly_income_shares_data")
  expect_true(res$ok, info = res$reason)
  unlink(tmp)
})

test_that("validate_dashboard_asset recognizes quarterly_lorenz_data", {
  source_pipeline_R()
  d <- data.table::data.table(
    ref_month_yyyymm = rep(202301L, 1100L),
    breakdown_type = "overall",
    breakdown_value = "Nacional",
    p = seq(0, 1, length.out = 1100L),
    lorenz = seq(0, 1, length.out = 1100L)^2,
    income_var = "indiv_hab_princ"
  )
  tmp <- tempfile(fileext = ".rds")
  saveRDS(d, tmp)
  res <- validate_dashboard_asset(tmp, "quarterly_lorenz_data")
  expect_true(res$ok, info = res$reason)
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
