# Unit tests for the Phase 5 equivalence engine (R/phase5_compare.R).
#
# Strategy: synthetic fixtures (small data.tables, sf objects, fst files)
# are written to tempfiles, the engine is invoked, and the verdict +
# axis breakdown is asserted. No external files; runs in seconds.

library(data.table)

# Source the engine so functions are visible in this test file's namespace.
# Path relative to tests/testthat/ -> ../../R/phase5_compare.R
source(testthat::test_path("..", "..", "R", "phase5_compare.R"),
       chdir = FALSE)

# -----------------------------------------------------------------------------
# Helper: build a minimal inequality_data spec
# -----------------------------------------------------------------------------

inequality_spec <- function(allowlist = list()) {
  list(
    asset = "inequality_data",
    file = "inequality_data.rds",
    mode = "rds",
    key_cols = c("ref_month_yyyymm", "breakdown_type",
                 "breakdown_value", "measure"),
    overlap_col = "ref_month_yyyymm",
    value_cols = list(value = "rate", n_obs = "count"),
    allowlist = allowlist,
    defaults = list(
      tolerances = list(
        rate     = list(abs = 1e-2, rel = 0.05),
        currency = list(abs = 1.0, rel = 0.06),
        count    = list(abs = 1000, rel = 0.005),
        population = list(abs = 1e5, rel = 0.01),
        lorenz   = list(abs = 5e-2),
        decomposition = list(abs = 0.5)
      ),
      rowcount = list(pass_threshold = 0.01, warn_threshold = 0.05),
      numeric  = list(pass_share_excess = 0.01, warn_share_excess = 0.10)
    )
  )
}

minimal_ineq <- function(value_offset = 0, period_max = 202412) {
  # Deterministic values: derived from the key so legacy and new produce
  # identical values on the overlap regardless of period_max.
  periods <- seq.int(202401L, period_max, by = 1L)
  periods <- periods[(periods %% 100L) %in% 1:12]
  measures <- c("gini", "palma", "mean_income")
  bds <- data.table::CJ(
    ref_month_yyyymm = periods,
    breakdown_type = c("overall", "uf"),
    breakdown_value = c("BR", "33"),
    measure = measures
  )
  bds[, value := 0.4 + value_offset +
                 (ref_month_yyyymm %% 100L) / 1000 +
                 nchar(measure) / 1000]
  bds[, n_obs := 1000L + as.integer(ref_month_yyyymm %% 100L) * 10L]
  bds[]
}

write_rds_pair <- function(legacy, new) {
  lp <- tempfile("legacy_", fileext = ".rds")
  np <- tempfile("new_", fileext = ".rds")
  saveRDS(legacy, lp)
  saveRDS(new, np)
  list(legacy = lp, new = np)
}

# -----------------------------------------------------------------------------
# Tests: compare_rds — happy path
# -----------------------------------------------------------------------------

test_that("compare_rds: identical inputs => PASS on all axes", {
  dt <- minimal_ineq()
  paths <- write_rds_pair(dt, copy(dt))
  res <- compare_rds(paths$legacy, paths$new, inequality_spec())

  expect_equal(res$status, "PASS")
  expect_equal(res$axes$existence, "PASS")
  expect_equal(res$axes$schema, "PASS")
  expect_equal(res$axes$levels, "PASS")
  expect_equal(res$axes$rowcount, "PASS")
  expect_equal(res$axes$numeric, "PASS")
  expect_equal(res$matched_rows, nrow(dt))
  expect_equal(res$legacy_only, 0L)
  expect_equal(res$new_only, 0L)
})

# -----------------------------------------------------------------------------
# Tests: compare_rds — schema axis
# -----------------------------------------------------------------------------

test_that("compare_rds: missing legacy column on new side => FAIL schema", {
  legacy <- minimal_ineq()
  new <- copy(legacy)[, n_obs := NULL]
  paths <- write_rds_pair(legacy, new)
  res <- compare_rds(paths$legacy, paths$new, inequality_spec())

  expect_equal(res$status, "FAIL")
  expect_equal(res$axes$schema, "FAIL")
})

test_that("compare_rds: extra column on new side => WARN schema", {
  legacy <- minimal_ineq()
  new <- copy(legacy)[, extra := 1.0]
  paths <- write_rds_pair(legacy, new)
  res <- compare_rds(paths$legacy, paths$new, inequality_spec())

  expect_equal(res$axes$schema, "WARN")
})

# -----------------------------------------------------------------------------
# Tests: compare_rds — overlap restriction
# -----------------------------------------------------------------------------

test_that("compare_rds: legacy ends earlier => only overlap compared", {
  legacy <- minimal_ineq(period_max = 202412L)
  new <- minimal_ineq(period_max = 202604L)
  paths <- write_rds_pair(legacy, new)
  res <- compare_rds(paths$legacy, paths$new, inequality_spec())

  expect_equal(res$overlap_max, 202412L)
  expect_equal(res$status, "PASS")  # overlap rows are identical
  expect_gt(res$new_only, 0L)       # post-overlap rows in new
})

# -----------------------------------------------------------------------------
# Tests: compare_rds — numeric axis
# -----------------------------------------------------------------------------

test_that("compare_rds: small drift within tol => PASS numeric", {
  legacy <- minimal_ineq()
  new <- copy(legacy)
  new[, value := value + 0.005]  # well under rate$abs = 1e-2
  paths <- write_rds_pair(legacy, new)
  res <- compare_rds(paths$legacy, paths$new, inequality_spec())

  expect_equal(res$axes$numeric, "PASS")
})

test_that("compare_rds: drift in 5% of rows => WARN numeric", {
  legacy <- minimal_ineq()
  new <- copy(legacy)
  drift_idx <- sample(seq_len(nrow(new)), size = ceiling(0.05 * nrow(new)))
  new[drift_idx, value := value + 0.5]  # blow past tol
  paths <- write_rds_pair(legacy, new)
  res <- compare_rds(paths$legacy, paths$new, inequality_spec())

  expect_equal(res$axes$numeric, "WARN")
})

test_that("compare_rds: drift in 20% of rows => FAIL numeric", {
  legacy <- minimal_ineq()
  new <- copy(legacy)
  drift_idx <- sample(seq_len(nrow(new)), size = ceiling(0.20 * nrow(new)))
  new[drift_idx, value := value + 0.5]
  paths <- write_rds_pair(legacy, new)
  res <- compare_rds(paths$legacy, paths$new, inequality_spec())

  expect_equal(res$axes$numeric, "FAIL")
  expect_equal(res$status, "FAIL")
})

# -----------------------------------------------------------------------------
# Tests: compare_rds — allowlist
# -----------------------------------------------------------------------------

test_that("compare_rds: allowlist abs_tol upgrades FAIL drift to PASS", {
  legacy <- minimal_ineq()
  new <- copy(legacy)
  # 100% of rows drift by 0.4 — would FAIL under default rate tol (1e-2)
  new[, value := value + 0.4]
  paths <- write_rds_pair(legacy, new)

  # Without allowlist: FAIL
  res_strict <- compare_rds(paths$legacy, paths$new, inequality_spec())
  expect_equal(res_strict$axes$numeric, "FAIL")

  # With allowlist abs_tol=0.5: PASS
  allow <- list(list(asset = "inequality_data",
                     rule = list(type = "abs_tol", threshold = 0.5),
                     reason = "test", approved_by = "test",
                     approved_on = "2026-04-26"))
  res_loose <- compare_rds(paths$legacy, paths$new,
                           inequality_spec(allowlist = allow))
  expect_equal(res_loose$axes$numeric, "PASS")
  expect_gt(length(res_loose$allowlist_hits), 0L)
})

test_that("compare_rds: allowlist scoped by where-clause only matches subset", {
  legacy <- minimal_ineq()
  new <- copy(legacy)
  new[measure == "mean_income", value := value + 0.4]  # scoped drift
  paths <- write_rds_pair(legacy, new)

  allow <- list(list(asset = "inequality_data",
                     scope = list(where = "measure == 'mean_income'"),
                     rule = list(type = "abs_tol", threshold = 0.5),
                     reason = "test", approved_by = "test",
                     approved_on = "2026-04-26"))
  res <- compare_rds(paths$legacy, paths$new,
                     inequality_spec(allowlist = allow))
  expect_equal(res$axes$numeric, "PASS")
})

# -----------------------------------------------------------------------------
# Tests: compare_sf — structural mode
# -----------------------------------------------------------------------------

test_that("compare_sf: identical sf => PASS structural", {
  skip_if_not_installed("sf")
  pts <- sf::st_sfc(
    sf::st_point(c(-50, -10)),
    sf::st_point(c(-45, -20)),
    crs = 4326
  )
  legacy <- sf::st_sf(uf_code = c("11", "12"),
                      uf_abbrev = c("RO", "AC"),
                      uf_name = c("Rondonia", "Acre"),
                      geometry = pts)
  paths <- write_rds_pair(legacy, legacy)

  spec <- list(
    asset = "brazil_states_sf",
    mode = "sf",
    structural_only = TRUE,
    key_cols = "uf_code",
    attribute_cols = c("uf_abbrev", "uf_name")
  )
  res <- compare_sf(paths$legacy, paths$new, spec)
  expect_equal(res$status, "PASS")
})

test_that("compare_sf: attribute mismatch => FAIL", {
  skip_if_not_installed("sf")
  pts <- sf::st_sfc(
    sf::st_point(c(-50, -10)),
    sf::st_point(c(-45, -20)),
    crs = 4326
  )
  legacy <- sf::st_sf(uf_code = c("11", "12"),
                      uf_abbrev = c("RO", "AC"),
                      uf_name = c("Rondonia", "Acre"),
                      geometry = pts)
  new <- legacy
  new$uf_name[1] <- "RondoniaCHANGED"
  paths <- write_rds_pair(legacy, new)

  spec <- list(asset = "brazil_states_sf", mode = "sf",
               structural_only = TRUE, key_cols = "uf_code",
               attribute_cols = c("uf_abbrev", "uf_name"))
  res <- compare_sf(paths$legacy, paths$new, spec)
  expect_equal(res$status, "FAIL")
})

# -----------------------------------------------------------------------------
# Tests: compare_fst_aggregates — aggregate mode
# -----------------------------------------------------------------------------

test_that("compare_fst_aggregates: identical fst => PASS", {
  skip_if_not_installed("fst")
  dt <- data.table::data.table(
    Ano = rep(2023L, 100),
    Trimestre = rep(2L, 100),
    UF = rep(35L, 100),
    weight_monthly = rep(100, 100),
    hhinc_pc = runif(100, 100, 1000),
    rendaTrab_ha_pc = runif(100, 50, 800),
    sexo = sample(c("M", "F"), 100, TRUE),
    raca = sample(c("Branca", "Parda", "Preta"), 100, TRUE)
  )
  lp <- tempfile("legacy_", fileext = ".fst")
  np <- tempfile("new_", fileext = ".fst")
  fst::write_fst(dt, lp)
  fst::write_fst(dt, np)

  spec <- list(
    asset = "prepared_microdata",
    mode = "aggregate_only",
    aggregates = c("n_rows", "weighted_pop_total_by_quarter",
                   "weighted_income_total_by_quarter",
                   "categorical_marginals"),
    defaults = list(
      tolerances = list(
        rate = list(abs = 1e-2, rel = 0.05),
        currency = list(abs = 1.0, rel = 0.06),
        count = list(abs = 1000, rel = 0.005),
        population = list(abs = 1e5, rel = 0.01)
      ),
      rowcount = list(pass_threshold = 0.01, warn_threshold = 0.05),
      numeric = list(pass_share_excess = 0.01, warn_share_excess = 0.10)
    ),
    allowlist = list()
  )
  res <- compare_fst_aggregates(lp, np, spec)
  expect_equal(res$status, "PASS")
})

# -----------------------------------------------------------------------------
# Tests: existence axis
# -----------------------------------------------------------------------------

test_that("compare_rds: missing new file => FAIL existence", {
  dt <- minimal_ineq()
  paths <- write_rds_pair(dt, dt)
  unlink(paths$new)  # remove new side
  res <- compare_rds(paths$legacy, paths$new, inequality_spec())
  expect_equal(res$axes$existence, "FAIL")
  expect_equal(res$status, "FAIL")
})

# -----------------------------------------------------------------------------
# Tests: report formatting
# -----------------------------------------------------------------------------

test_that("format_report_md: produces a string with key sections", {
  results <- list(
    list(asset = "inequality_data", status = "PASS",
         axes = list(existence = "PASS", schema = "PASS",
                     levels = "PASS", rowcount = "PASS", numeric = "PASS"),
         matched_rows = 100, legacy_only = 0, new_only = 0,
         overlap_max = 202412L, allowlist_hits = list(),
         column_summary = data.table::data.table(
           column = "value", max_abs = 1e-3, max_rel = 1e-3,
           p50_abs = 1e-4, p95_abs = 5e-4, p99_abs = 1e-3,
           tol_abs = 1e-2, tol_rel = 0.05, n_diff = 5L,
           share_excess = 0.0, status = "PASS"
         ))
  )
  md <- format_report_md(results, run_ts = "2026-04-26 00:00 UTC")
  expect_type(md, "character")
  expect_match(md, "Phase 5 Equivalence Report", fixed = TRUE)
  expect_match(md, "inequality_data", fixed = TRUE)
  expect_match(md, "PASS", fixed = TRUE)
})

test_that("aggregate_overall_status: worst-of semantics", {
  expect_equal(aggregate_overall_status(c("PASS", "PASS", "PASS")), "PASS")
  expect_equal(aggregate_overall_status(c("PASS", "WARN", "PASS")), "WARN")
  expect_equal(aggregate_overall_status(c("PASS", "WARN", "FAIL")), "FAIL")
  expect_equal(aggregate_overall_status(c("PASS", "SKIP", "PASS")), "PASS")
})
