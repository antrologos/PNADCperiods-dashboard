test_that("tar-wrappers expose the expected function signatures", {
  source_pipeline_R()
  expect_true(is.function(build_crosswalk))
  expect_true(is.function(apply_periods_annual))
  expect_true(is.function(apply_periods_quarterly))
  expect_true(is.function(validate_pnadc_minimal))

  # Just signature shape — body may call into PNADCperiods which requires
  # microdata to actually run; we don't exercise the package here.
  args_apply_a <- names(formals(apply_periods_annual))
  expect_true(all(c("annual_dt", "crosswalk") %in% args_apply_a))
  args_apply_q <- names(formals(apply_periods_quarterly))
  expect_true(all(c("quarterly_dt", "crosswalk") %in% args_apply_q))
})
