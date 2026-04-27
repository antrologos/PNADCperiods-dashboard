# Targeted tests for tar-microdata.R helpers that don't require real
# microdata. Integration paths (build_prepared_microdata, the Layer 3
# builders) are exercised end-to-end by `tar_make()` and the migration
# equivalence test (Phase 5), not here.

write_synthetic_deflator <- function(periods_dt, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  dt <- data.table::copy(periods_dt)
  data.table::setnames(dt, c("Ano", "Trimestre", "UF"), c("ano", "trim", "uf"))
  dt[, CO2  := 1.0]
  dt[, CO2e := 1.0]
  dt[, CO3  := 1.0]
  if (!requireNamespace("writexl", quietly = TRUE)) {
    skip("writexl not available; cannot synthesise deflator XLS")
  }
  writexl::write_xlsx(as.data.frame(dt), path)
}

test_that("deflate_incomes aborts when deflator misses an (Ano, Trimestre)", {
  source_pipeline_R()
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  skip_if_not_installed("deflateBR")

  # Deflator covers only 2024-Q1; microdata has 2024-Q1 AND 2024-Q2
  defl <- data.table::CJ(Ano = 2024L, Trimestre = 1L, UF = 11L:53L)
  # writexl produces xlsx; readxl::read_excel auto-detects by extension.
  # Production uses .xls (libxls), but the coverage check logic is
  # format-independent so .xlsx is acceptable for this test.
  defl_path <- tempfile(fileext = ".xlsx")
  write_synthetic_deflator(defl, defl_path)
  on.exit(unlink(defl_path), add = TRUE)

  d <- data.table::data.table(
    Ano = c(2024L, 2024L),
    Trimestre = c(1L, 2L),
    UF = c(35L, 35L),
    vd5008 = c(1000, 2000),
    vd4019 = c(1000, 2000),
    vd4020 = c(1000, 2000)
  )

  expect_error(
    deflate_incomes(d, read_deflator_xls(defl_path), inpc_factor = 1.05),
    "does not cover .* microdata key",
    ignore.case = TRUE
  )
})

test_that("deflate_incomes aborts when deflator misses a UF for one quarter", {
  source_pipeline_R()
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  skip_if_not_installed("deflateBR")

  # Deflator covers (2024-Q1, all UFs) and (2024-Q2, UFs except 35)
  defl <- data.table::rbindlist(list(
    data.table::CJ(Ano = 2024L, Trimestre = 1L, UF = 11L:53L),
    data.table::CJ(Ano = 2024L, Trimestre = 2L, UF = setdiff(11L:53L, 35L))
  ))
  # writexl produces xlsx; readxl::read_excel auto-detects by extension.
  # Production uses .xls (libxls), but the coverage check logic is
  # format-independent so .xlsx is acceptable for this test.
  defl_path <- tempfile(fileext = ".xlsx")
  write_synthetic_deflator(defl, defl_path)
  on.exit(unlink(defl_path), add = TRUE)

  # Microdata has 2024-Q2-UF35 — uncovered by the deflator
  d <- data.table::data.table(
    Ano = c(2024L, 2024L),
    Trimestre = c(1L, 2L),
    UF = c(11L, 35L),
    vd5008 = c(1000, 2000),
    vd4019 = c(1000, 2000),
    vd4020 = c(1000, 2000)
  )

  expect_error(
    deflate_incomes(d, read_deflator_xls(defl_path), inpc_factor = 1.05),
    "does not cover .* microdata key",
    ignore.case = TRUE
  )
})

test_that("recode_annual sources labels_path into globalenv (regression: sex_label scope)", {
  # `local = TRUE` would scope sourced symbols to recode_annual's frame.
  # build_demographic_groupings (in globalenv via _targets.R line 47) then
  # can't see sex_label/race_label/etc. via lexical scoping.
  # PR4: source(labels_path) moved from build_prepared_microdata to
  # recode_annual (in tar-recode.R). The no-local-TRUE constraint still applies.
  source_pipeline_R()
  skip_if(!exists("recode_annual"), "recode_annual not loaded")
  body_src <- paste(deparse(body(recode_annual)), collapse = "\n")
  expect_match(body_src, "source\\(labels_path",
               info = "recode_annual must source labels_path")
  expect_false(grepl("source\\(labels_path[^)]*local\\s*=\\s*TRUE",
                     body_src),
               info = "source(labels_path) must NOT use local = TRUE")
})

test_that("PR6: inequality sub-builders are vectorized (compute_breakdowns/shares/lorenz/decomp)", {
  source_pipeline_R()
  skip_if(!exists("compute_breakdowns"), "compute_breakdowns not loaded")
  skip_if(!exists("compute_shares"),     "compute_shares not loaded")
  skip_if(!exists("compute_lorenz"),     "compute_lorenz not loaded")
  skip_if(!exists("compute_gini_decomp"),"compute_gini_decomp not loaded")

  # New signatures (PR6 dropped `months` arg — vectorization handles it)
  expect_named(formals(compute_breakdowns), c("d", "specs", "measure_fn"))
  expect_named(formals(compute_shares),     c("d", "specs"))
  expect_named(formals(compute_lorenz),     c("d", "specs"))
  expect_named(formals(compute_gini_decomp),c("d", "income_vars", "labels_dt"))

  # Bodies must use data.table `by =` (vectorized), not nested for-loops
  for (fn_name in c("compute_breakdowns", "compute_shares",
                    "compute_lorenz", "compute_gini_decomp")) {
    body_src <- paste(deparse(body(get(fn_name))), collapse = "\n")
    expect_match(body_src, "by\\s*=",
                 info = sprintf("PR6: %s must use data.table by = (vectorized)",
                                fn_name))
    # No nested for-loops over (g) and (m)
    expect_false(grepl("for\\s*\\([^)]+\\s+in\\s+grps[^)]*\\)\\s*for\\s*\\([^)]+\\s+in\\s+months",
                       body_src),
                 info = sprintf("PR6: %s must NOT have nested for(g) for(m)",
                                fn_name))
  }
})

test_that("PR5: build_poverty_outputs is vectorized (data.table by, not nested loops)", {
  source_pipeline_R()
  skip_if(!exists("build_poverty_outputs"), "build_poverty_outputs not loaded")
  body_src <- paste(deparse(body(build_poverty_outputs)), collapse = "\n")

  # Vectorized via data.table `by =`
  expect_match(body_src, "by\\s*=\\s*\\.\\(ref_month_yyyymm\\)",
               info = "PR5: must use by = .(ref_month_yyyymm) for overall sweep")
  expect_match(body_src, "by\\s*=\\s*c\\(\"ref_month_yyyymm\"",
               info = "PR5: must use by = c(...) for breakdown sweeps")

  # NO nested groups × months loop (the legacy 4-level nest)
  expect_false(grepl("for\\s*\\(\\s*g\\s+in\\s+grps\\s*\\)\\s*for\\s*\\(\\s*m\\s+in\\s+months",
                     body_src),
               info = "PR5: must NOT have nested for(g) for(m) loop")

  # No more `if (nrow(sub) < 10L) next` — replaced by post-filter `n_obs >= 10L`
  expect_false(grepl("nrow\\(sub\\)\\s*<\\s*10L", body_src),
               info = "PR5: must NOT subset+next; use n_obs >= 10L post-filter")
})

test_that("PR5: vectorized fgt_all reproduces scalar output (toy regression)", {
  measures_path <- testthat::test_path("..", "..", "..", "R", "measures_poverty.R")
  skip_if_not(file.exists(measures_path), "measures_poverty.R not found")
  sys.source(measures_path, envir = environment())

  # Toy data.table: 6 households, 2 ref_months, 1 fixed poverty line z = 100.
  d <- data.table::data.table(
    ref_month_yyyymm = c(rep(202401L, 3L), rep(202402L, 3L)),
    hhinc_pc = c(50, 80, 200, 60, 120, 300),
    weight_monthly = c(1, 1, 1, 1, 1, 1)
  )
  z <- 100

  # Vectorized via by-group
  vec_res <- d[, {
    fgt <- fgt_all(hhinc_pc, z, weight_monthly)
    list(fgt0 = fgt$fgt0, fgt1 = fgt$fgt1, fgt2 = fgt$fgt2)
  }, by = .(ref_month_yyyymm)]

  # Scalar (legacy) — manual call per group
  scalar_jan <- fgt_all(d[ref_month_yyyymm == 202401L, hhinc_pc],
                        z, d[ref_month_yyyymm == 202401L, weight_monthly])
  scalar_feb <- fgt_all(d[ref_month_yyyymm == 202402L, hhinc_pc],
                        z, d[ref_month_yyyymm == 202402L, weight_monthly])

  expect_equal(vec_res[ref_month_yyyymm == 202401L, fgt0], scalar_jan$fgt0,
               tolerance = 1e-12)
  expect_equal(vec_res[ref_month_yyyymm == 202401L, fgt1], scalar_jan$fgt1,
               tolerance = 1e-12)
  expect_equal(vec_res[ref_month_yyyymm == 202402L, fgt0], scalar_feb$fgt0,
               tolerance = 1e-12)
})

# ------------------------------------------------------------------------------
# 2025 visita 1 simplified income module (regression: hhinc_pc=0 cascade)
# ------------------------------------------------------------------------------

test_that("deflate_incomes preserves NA in years flagged as simplified", {
  source_pipeline_R()
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  # Synthetic deflator covering both 2024 (full) and 2025 (simplified)
  defl <- data.table::CJ(Ano = 2024L:2025L, Trimestre = 1L:4L, UF = 11L:53L)
  defl_path <- tempfile(fileext = ".xlsx")
  write_synthetic_deflator(defl, defl_path)
  on.exit(unlink(defl_path), add = TRUE)

  # Mixed microdata: 2024 with vd5008 valid + flag TRUE; 2025 with vd5008
  # NA + flag FALSE (mirrors the IBGE 2025 visita 1 simplified module).
  d <- data.table::data.table(
    Ano = c(rep(2024L, 4L), rep(2025L, 4L)),
    Trimestre = rep(1L:4L, 2L),
    UF = rep(35L, 8L),
    vd5008 = c(c(1000, 1500, 2000, 2500), rep(NA_real_, 4L)),
    vd4019 = c(c(800, 1200, 1600, 2000), c(900, 1300, 1700, 2100)),
    vd4020 = c(c(800, 1200, 1600, 2000), c(900, 1300, 1700, 2100)),
    income_module_complete = c(rep(TRUE, 4L), rep(FALSE, 4L))
  )

  out <- deflate_incomes(d, read_deflator_xls(defl_path), inpc_factor = 1.05)
  out_2024 <- out[Ano == 2024L]
  out_2025 <- out[Ano == 2025L]

  # 2024: legacy behaviour preserved (no NA in hhinc_pc_nominal)
  expect_true(all(!is.na(out_2024$hhinc_pc_nominal)))
  expect_true(all(out_2024$hhinc_pc_nominal > 0))
  expect_true(all(!is.na(out_2024$hhinc_pc)))

  # 2025: NA propagates (no silent zero-cascade)
  expect_true(all(is.na(out_2025$hhinc_pc_nominal)))
  expect_true(all(is.na(out_2025$hhinc_pc)))
  expect_false(any(out_2025$hhinc_pc == 0, na.rm = TRUE))
})

test_that("build_inequality_outputs filters income_module_complete == TRUE", {
  source_pipeline_R()
  skip_if(!exists("build_inequality_outputs"),
          "build_inequality_outputs not loaded")
  body_src <- paste(deparse(body(build_inequality_outputs)), collapse = "\n")
  expect_match(body_src,
               "income_module_complete\\s*==\\s*TRUE",
               info = "build_inequality_outputs must drop simplified-module years")
})

test_that("build_poverty_outputs filters income_module_complete == TRUE", {
  source_pipeline_R()
  skip_if(!exists("build_poverty_outputs"),
          "build_poverty_outputs not loaded")
  body_src <- paste(deparse(body(build_poverty_outputs)), collapse = "\n")
  expect_match(body_src,
               "income_module_complete\\s*==\\s*TRUE",
               info = "build_poverty_outputs must drop simplified-module years")
})

test_that("legacy-behaviour regression: 2024 hhinc_pc_nominal unchanged", {
  # Same numeric output as before the patch when income_module_complete==TRUE.
  # Mirrors the legacy single-line `fifelse(is.na(vd5008), 0, ...)` exactly.
  source_pipeline_R()
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")

  defl <- data.table::CJ(Ano = 2024L, Trimestre = 1L:4L, UF = 11L:53L)
  defl_path <- tempfile(fileext = ".xlsx")
  write_synthetic_deflator(defl, defl_path)
  on.exit(unlink(defl_path), add = TRUE)

  # Mix of valid vd5008 and explicit NA — legacy behaviour for the NA case
  # is hhinc_pc_nominal = 0; we must reproduce it exactly.
  d <- data.table::data.table(
    Ano = rep(2024L, 4L),
    Trimestre = 1L:4L,
    UF = rep(35L, 4L),
    vd5008 = c(1234.5, NA_real_, 9876.0, 0),
    vd4019 = c(1000, NA_real_, 1500, 0),
    vd4020 = c(1000, NA_real_, 1500, 0),
    income_module_complete = rep(TRUE, 4L)
  )
  expected <- c(1234.5, 0, 9876.0, 0)  # NA -> 0, others as numeric
  out <- deflate_incomes(d, read_deflator_xls(defl_path), inpc_factor = 1.0)
  expect_equal(out$hhinc_pc_nominal, expected, tolerance = 1e-12)
})

test_that("PR4: build_prepared_microdata is a thin writer (no recode logic)", {
  source_pipeline_R()
  skip_if(!exists("build_prepared_microdata"), "build_prepared_microdata not loaded")
  expect_named(formals(build_prepared_microdata),
               c("annual_recoded", "dest_path"), ignore.order = TRUE)
  body_src <- paste(deparse(body(build_prepared_microdata)), collapse = "\n")
  # No more annual loading, recoding, or deflation in this builder
  expect_false(grepl("apply_periods_annual", body_src),
               info = "PR4: apply_periods_annual moved to recode_annual")
  expect_false(grepl("deflate_incomes", body_src),
               info = "PR4: deflate_incomes moved to recode_annual")
  expect_false(grepl("load_annual_with_income_harmonization", body_src),
               info = "PR4: stacking moved to stack_annual")
  expect_false(grepl("source\\(labels_path", body_src),
               info = "PR4: source(labels_path) moved to recode_annual")
})

test_that("PR2 split: each builder sources only its relevant module", {
  source_pipeline_R()
  skip_if(!exists("build_inequality_outputs"), "builders not loaded")
  ineq_body <- paste(deparse(body(build_inequality_outputs)), collapse = "\n")
  pov_body  <- paste(deparse(body(build_poverty_outputs)),    collapse = "\n")
  # build_inequality_outputs sources measures_inequality, NOT poverty
  expect_match(ineq_body, "source\\(measures_inequality_path")
  expect_false(grepl("measures_poverty_path", ineq_body))
  # build_poverty_outputs sources measures_poverty, NOT inequality
  expect_match(pov_body, "source\\(measures_poverty_path")
  expect_false(grepl("measures_inequality_path", pov_body))

  # PR6 regression: NEITHER builder uses local = TRUE. compute_shares /
  # compute_lorenz / compute_gini_decomp call income_shares / lorenz_points /
  # gini_decomposition from globalenv via lexical scoping (same lesson as
  # Plan 5 fix for build_prepared_microdata).
  expect_false(grepl("source\\(measures_inequality_path[^)]*local\\s*=\\s*TRUE",
                     ineq_body),
               info = "build_inequality_outputs must NOT use local = TRUE")
  expect_false(grepl("source\\(measures_poverty_path[^)]*local\\s*=\\s*TRUE",
                     pov_body),
               info = "build_poverty_outputs must NOT use local = TRUE")
})

test_that("PR2 stub: utils_inequality.R sources all 3 split files into globalenv", {
  utils_path <- testthat::test_path("..", "..", "..", "R", "utils_inequality.R")
  skip_if_not(file.exists(utils_path), "utils_inequality.R stub not found")
  test_env <- new.env(parent = baseenv())
  # Use sys.source to mimic source(); the stub uses sys.source to globalenv,
  # so for testing we verify the stub's BODY contains the expected pattern.
  stub_lines <- readLines(utils_path)
  stub_text <- paste(stub_lines, collapse = "\n")
  expect_match(stub_text, "labels\\.R")
  expect_match(stub_text, "measures_inequality\\.R")
  expect_match(stub_text, "measures_poverty\\.R")
  # And the stub should be small (sanity: monolith was ~691 lines)
  expect_lt(length(stub_lines), 50L)
})

test_that("lorenz_points returns trivial curve when sum(w*x) <= 0 (regression: degenerate group)", {
  # `compute_lorenz` iterates over (ref_month, breakdown_value) groups.
  # If any single group has all zero/negative hhinc_pc, sum(w*x) == 0
  # → NaN in cumulative shares → stats::approx errors out:
  # "são precisos ao menos dois valores não-NA para interpolar".
  # lorenz_points must short-circuit to the equality-line fallback.
  # PR2: lorenz_points moved to measures_inequality.R; source that directly.
  measures_path <- testthat::test_path("..", "..", "..", "R", "measures_inequality.R")
  skip_if_not(file.exists(measures_path), "measures_inequality.R not found")
  sys.source(measures_path, envir = environment())

  # All zeros → must NOT error; must return p == lorenz curve
  lp_zeros <- lorenz_points(rep(0, 100), w = rep(1, 100), n = 100)
  expect_s3_class(lp_zeros, "data.frame")
  expect_equal(nrow(lp_zeros), 101L)
  expect_equal(lp_zeros$p, lp_zeros$lorenz)

  # Real data still produces a real Lorenz curve (regression guard)
  set.seed(42)
  x <- rexp(1000, rate = 1)  # skewed positive incomes
  lp_real <- lorenz_points(x, w = rep(1, 1000), n = 100)
  expect_equal(nrow(lp_real), 101L)
  # Lorenz curve must be below equality line for skewed income
  expect_true(mean(lp_real$lorenz < lp_real$p) > 0.7)
})

test_that("PR3: stack and crosswalk extracted; builders consume in-memory targets", {
  # PR3 of DAG re-architecture:
  # - stack_quarterly() reads 56 .fst once into quarterly_stacked
  # - build_crosswalk_from_stack consumes the in-memory stack
  # - recode_quarterly does ALL labor recoding once
  # - build_state_monthly is a thin aggregator over quarterly_recoded
  source_pipeline_R()
  skip_if(!exists("stack_quarterly"), "stack_quarterly not loaded")
  skip_if(!exists("build_crosswalk_from_stack"),
          "build_crosswalk_from_stack not loaded")
  skip_if(!exists("recode_quarterly"), "recode_quarterly not loaded")
  skip_if(!exists("build_state_monthly"), "build_state_monthly not loaded")
  skip_if(!exists("build_prepared_microdata"), "build_prepared_microdata not loaded")

  # Structural: new functions have the expected signatures.
  expect_named(formals(stack_quarterly),
               c("quarterly_manifest", "cols"), ignore.order = TRUE)
  expect_named(formals(build_crosswalk_from_stack), "quarterly_stacked")
  expect_named(formals(recode_quarterly),
               c("quarterly_stacked", "crosswalk"), ignore.order = TRUE)

  # build_state_monthly now takes ONLY the recoded data + dest_path
  # (no more acervo_manifest, no more crosswalk arg)
  expect_named(formals(build_state_monthly),
               c("quarterly_recoded", "dest_path"), ignore.order = TRUE)

  # PR4 update: build_prepared_microdata is now a thin writer over
  # annual_recoded — `crosswalk` is consumed by recode_annual (in tar-recode.R)
  # not by the writer. This assertion is in the dedicated PR4 test below.

  # Regression: build_state_monthly body should NOT re-stack .fst or call
  # apply_periods_quarterly (those moved to stack_quarterly + recode_quarterly).
  state_body <- paste(deparse(body(build_state_monthly)), collapse = "\n")
  expect_false(grepl("fst::read_fst", state_body),
               info = "build_state_monthly must NOT re-read .fst (PR3)")
  expect_false(grepl("apply_periods_quarterly", state_body),
               info = "build_state_monthly must NOT apply periods (PR3)")
  expect_false(grepl("rbindlist", state_body),
               info = "build_state_monthly must NOT stack .fst (PR3)")

  # Old name is gone
  expect_false(exists("build_crosswalk_from_quarterly"),
               info = "build_crosswalk_from_quarterly was renamed to build_crosswalk_from_stack")
})

test_that("recode_quarterly: numeric_cols include Ano and Trimestre (regression)", {
  # The .fst files store Ano and Trimestre as character. Arithmetic
  # `Ano * 100L + Trimestre * 3L` (now inside recode_quarterly) fails with
  # "non-numeric argument to binary operator" unless these are coerced first.
  # Guard against accidental removal of "Ano"/"Trimestre" from numeric_cols.
  source_pipeline_R()
  skip_if(!exists("recode_quarterly"), "recode_quarterly not loaded")

  # (a) Structural: the numeric_cols vector inside recode_quarterly's body
  # must contain "Ano" and "Trimestre".
  body_src <- paste(deparse(body(recode_quarterly)), collapse = "\n")
  expect_match(body_src, 'numeric_cols\\s*<-\\s*c\\([^)]*"Ano"')
  expect_match(body_src, 'numeric_cols\\s*<-\\s*c\\([^)]*"Trimestre"')

  # (b) Behavioral: after as.numeric coercion, the yyyymm_q arithmetic
  # produces the correct quarter-end month for both pre- and post-split
  # boundaries (vd4004_split_yyyymm = 201509L).
  ano_chr <- c("2014", "2015", "2015", "2024")
  tri_chr <- c("1",    "3",    "4",    "4")
  ano_num <- as.numeric(ano_chr)
  tri_num <- as.numeric(tri_chr)
  yyyymm_q <- ano_num * 100L + tri_num * 3L
  expect_equal(yyyymm_q, c(201403, 201509, 201512, 202412))
})

test_that("deflate_incomes succeeds when deflator covers all data keys", {
  source_pipeline_R()
  skip_if_not_installed("writexl")
  skip_if_not_installed("readxl")
  skip_if_not_installed("deflateBR")

  defl <- data.table::CJ(Ano = 2024L, Trimestre = 1L:2L, UF = 11L:53L)
  # writexl produces xlsx; readxl::read_excel auto-detects by extension.
  # Production uses .xls (libxls), but the coverage check logic is
  # format-independent so .xlsx is acceptable for this test.
  defl_path <- tempfile(fileext = ".xlsx")
  write_synthetic_deflator(defl, defl_path)
  on.exit(unlink(defl_path), add = TRUE)

  d <- data.table::data.table(
    Ano = c(2024L, 2024L),
    Trimestre = c(1L, 2L),
    UF = c(35L, 35L),
    vd5008 = c(1000, 2000),
    vd4019 = c(1000, 2000),
    vd4020 = c(1000, 2000)
  )

  out <- deflate_incomes(data.table::copy(d), read_deflator_xls(defl_path),
                         inpc_factor = 1.05)
  expect_true("hhinc_pc" %in% names(out))
  expect_true(all(!is.na(out$hhinc_pc)))
})
