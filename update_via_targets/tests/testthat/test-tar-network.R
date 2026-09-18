# Tests for tar-network.R (PR1: Network consolidation)
#
# All external network resources should be fetched in ONE Layer 1 target.
# This file tests the helpers; integration is covered by tar_make().

test_that("validate_deflation_target_date accepts MM/YYYY and rejects others", {
  source_pipeline_R()
  expect_silent(validate_deflation_target_date("12/2025"))
  expect_silent(validate_deflation_target_date("01/2030"))
  expect_error(validate_deflation_target_date("2025-12"), "MM/YYYY")
  expect_error(validate_deflation_target_date("13/2025"), "MM/YYYY")
  expect_error(validate_deflation_target_date("00/2025"), "MM/YYYY")
  expect_error(validate_deflation_target_date(""), "MM/YYYY")
  expect_error(validate_deflation_target_date(c("12/2025", "01/2026")),
               "single character")
})

test_that("inpc_factor_at returns the factor for a present date", {
  source_pipeline_R()
  tab <- data.table::data.table(
    nominal_date = as.Date(c("2021-07-01", "2024-07-01")),
    factor = c(1.20, 1.05)
  )
  expect_equal(inpc_factor_at(tab, as.Date("2024-07-01")), 1.05)
  expect_equal(inpc_factor_at(tab, "2024-07-01"), 1.05)
  expect_equal(inpc_factor_at(tab, as.Date("2021-07-01")), 1.20)
})

test_that("inpc_factor_at errors clearly when date missing", {
  source_pipeline_R()
  tab <- data.table::data.table(
    nominal_date = as.Date("2021-07-01"),
    factor = 1.20
  )
  expect_error(inpc_factor_at(tab, as.Date("2030-07-01")),
               "INPC factor.*not found")
})

test_that("compute_inpc_factors returns data.table with required nominal dates", {
  source_pipeline_R()
  skip_if_not_installed("deflateBR")
  skip_if_offline()
  # Use a tiny mw_years range to keep the test fast
  out <- compute_inpc_factors("12/2025", mw_years = 2022L:2024L)
  expect_s3_class(out, "data.table")
  expect_named(out, c("nominal_date", "factor"))
  expect_true(all(is.finite(out$factor)))
  # Required fixed dates
  expect_true(as.Date("2021-07-01") %in% out$nominal_date)
  expect_true(as.Date("2024-07-01") %in% out$nominal_date)
  # MW years are present
  expect_true(as.Date("2022-07-01") %in% out$nominal_date)
  expect_true(as.Date("2024-07-01") %in% out$nominal_date)
  # Factors for older years should be > 1 (deflate forward)
  f_2022 <- out[nominal_date == as.Date("2022-07-01"), factor]
  f_2024 <- out[nominal_date == as.Date("2024-07-01"), factor]
  expect_gt(f_2022, f_2024)  # 2022 needs more inflation to reach 12/2025
})

test_that("fetch_ipca_series returns the Dec/1993-based index by month", {
  source_pipeline_R()
  skip_if_not_installed("PNADCperiods")
  skip_if_offline()

  out <- tryCatch(fetch_ipca_series(max_retries = 2L),
                  error = function(e) skip(paste("SIDRA unavailable:",
                                                 conditionMessage(e))))

  expect_s3_class(out, "data.table")
  expect_named(out, c("yyyymm", "ipca_index"))
  expect_type(out$yyyymm, "integer")
  expect_true(all(is.finite(out$ipca_index)))

  # One row per month, no duplicates: a mistranslated request would bring
  # extra rows and silently double every period.
  expect_false(anyDuplicated(out$yyyymm) > 0)

  # The series covers Jan 1994 and must reach the recent past.
  expect_true(199401L %in% out$yyyymm)
  expect_gte(max(out$yyyymm), 202601L)

  # Base is Dec/1993 = 100. The index is not monotonic -- Brazil had
  # months of deflation -- but it grows by orders of magnitude over the
  # period, driven by the mid-1990s hyperinflation.
  expect_gt(out[yyyymm == 199401L, ipca_index], 100)
  expect_gt(out[yyyymm == max(yyyymm), ipca_index],
            10 * out[yyyymm == 199401L, ipca_index])
})

test_that("fetch_brazil_states_sf returns sf with 27 features", {
  source_pipeline_R()
  skip_if_not_installed("geobr")
  skip_if_offline()
  states <- fetch_brazil_states_sf(year = 2020L)
  expect_s3_class(states, "sf")
  expect_equal(nrow(states), 27L)
})
