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
    deflate_incomes(d, defl_path),
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
    deflate_incomes(d, defl_path),
    "does not cover .* microdata key",
    ignore.case = TRUE
  )
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

  out <- deflate_incomes(data.table::copy(d), defl_path)
  expect_true("hhinc_pc" %in% names(out))
  expect_true(all(!is.na(out$hhinc_pc)))
})
