test_that("get_default_visit returns 5 for COVID years and 1 otherwise", {
  source_pipeline_R()
  expect_equal(get_default_visit(2019), 1L)
  expect_equal(get_default_visit(2020), 5L)
  expect_equal(get_default_visit(2021), 5L)
  expect_equal(get_default_visit(2022), 1L)
  expect_equal(get_default_visit(2024), 1L)
  # vectorised
  expect_equal(get_default_visit(c(2019, 2020, 2021, 2022)),
               c(1L, 5L, 5L, 1L))
})

test_that("acervo_is_dry_run honours env var first, option fallback", {
  source_pipeline_R()
  withr::with_envvar(c(ACERVO_DRY_RUN = "1"), {
    expect_true(acervo_is_dry_run())
  })
  withr::with_envvar(c(ACERVO_DRY_RUN = ""), {
    withr::with_options(list(acervo.dry_run = TRUE), {
      expect_true(acervo_is_dry_run())
    })
    withr::with_options(list(acervo.dry_run = FALSE), {
      expect_false(acervo_is_dry_run())
    })
  })
})

test_that("acervo_subpaths returns the expected named entries", {
  source_pipeline_R()
  sp <- acervo_subpaths("/tmp/PNADC")
  expect_named(sp, c("quarterly", "annual", "deflator",
                     "quarterly_deflator", "suplements"))
  expect_match(sp$quarterly, "Trimestral/Dados$")
  expect_match(sp$annual, "Anual/visitas$")
  expect_match(sp$quarterly_deflator, "Trimestral/Documentacao$")
})
