# Tests for the dashboard's utils_sidra_mask.R workaround.
# Sourced from the dashboard root (testthat helper resolves the path).

source_utils_sidra_mask <- function() {
  path <- testthat::test_path("..", "..", "..", "R", "utils_sidra_mask.R")
  sys.source(path, envir = globalenv())
}

# ------------------------------------------------------------------------------
# T1: phantom value at trailing NA position is masked
# ------------------------------------------------------------------------------

test_that("mask_phantom_mensalized masks single trailing-NA phantom", {
  source_utils_sidra_mask()
  ms <- data.table::data.table(
    anomesexato   = c(202601L, 202602L, 202603L),
    m_taxadesocup = c(6.0,     6.2,     5.1)   # 5.1 is the phantom
  )
  rq <- data.table::data.table(
    anomesfinaltrimmovel = c(202601L, 202602L, 202603L),
    taxadesocup          = c(5.4,     5.8,     NA_real_)
  )
  out <- mask_phantom_mensalized(ms, rq)
  expect_equal(out[anomesexato == 202601L, m_taxadesocup], 6.0)
  expect_equal(out[anomesexato == 202602L, m_taxadesocup], 6.2)
  expect_true(is.na(out[anomesexato == 202603L, m_taxadesocup]))
})

# ------------------------------------------------------------------------------
# T2: series without trailing NA in rq is preserved exactly
# ------------------------------------------------------------------------------

test_that("mask_phantom_mensalized preserves data when rq has no trailing NA", {
  source_utils_sidra_mask()
  ms <- data.table::data.table(
    anomesexato   = c(202601L, 202602L, 202603L),
    m_taxadesocup = c(6.0,     6.2,     6.4)
  )
  rq <- data.table::data.table(
    anomesfinaltrimmovel = c(202601L, 202602L, 202603L),
    taxadesocup          = c(5.4,     5.8,     5.9)
  )
  out <- mask_phantom_mensalized(ms, rq)
  expect_equal(out$m_taxadesocup, c(6.0, 6.2, 6.4))
})

# ------------------------------------------------------------------------------
# T3: late-starting series (e.g., CNPJ from 201510) — leading NAs untouched,
# trailing phantom still masked
# ------------------------------------------------------------------------------

test_that("mask_phantom_mensalized only masks trailing positions, not leading", {
  source_utils_sidra_mask()
  ms <- data.table::data.table(
    anomesexato                = c(201509L, 201510L, 201511L, 201512L, 202601L),
    m_contapropriacomcnpj      = c(NA,      0.31,    0.32,    0.33,    0.40)
  )
  rq <- data.table::data.table(
    anomesfinaltrimmovel       = c(201509L, 201510L, 201511L, 201512L, 202601L),
    contapropriacomcnpj        = c(NA,      0.30,    0.31,    0.32,    NA)  # NA trailing
  )
  out <- mask_phantom_mensalized(ms, rq)
  # Leading NA preserved as-is (was already NA)
  expect_true(is.na(out[anomesexato == 201509L, m_contapropriacomcnpj]))
  # Middle positions preserved
  expect_equal(out[anomesexato == 201510L, m_contapropriacomcnpj], 0.31)
  expect_equal(out[anomesexato == 201512L, m_contapropriacomcnpj], 0.33)
  # Trailing position with rq=NA -> masked
  expect_true(is.na(out[anomesexato == 202601L, m_contapropriacomcnpj]))
})

# ------------------------------------------------------------------------------
# T4: count_phantom_mensalized — counts phantoms before, zero after masking
# ------------------------------------------------------------------------------

test_that("count_phantom_mensalized counts before/after masking", {
  source_utils_sidra_mask()
  ms <- data.table::data.table(
    anomesexato     = c(202601L, 202602L, 202603L),
    m_taxadesocup   = c(6.0,     6.2,     5.1),    # phantom at 202603
    m_pop           = c(100.0,   100.5,   101.0)   # phantom at 202603
  )
  rq <- data.table::data.table(
    anomesfinaltrimmovel = c(202601L, 202602L, 202603L),
    taxadesocup          = c(5.4,     5.8,     NA_real_),
    pop                  = c(220.0,   220.5,   NA_real_)
  )
  expect_equal(count_phantom_mensalized(ms, rq), 2L)
  fixed <- mask_phantom_mensalized(ms, rq)
  expect_equal(count_phantom_mensalized(fixed, rq), 0L)
})

# ------------------------------------------------------------------------------
# T5: NULL inputs are handled gracefully
# ------------------------------------------------------------------------------

test_that("mask_phantom_mensalized handles NULL inputs", {
  source_utils_sidra_mask()
  ms <- data.table::data.table(anomesexato = 202601L, m_x = 1.0)
  rq <- data.table::data.table(anomesfinaltrimmovel = 202601L, x = 1.0)
  expect_null(mask_phantom_mensalized(NULL, rq))
  expect_identical(mask_phantom_mensalized(ms, NULL), ms)
  expect_equal(count_phantom_mensalized(NULL, rq), 0L)
  expect_equal(count_phantom_mensalized(ms, NULL), 0L)
})
