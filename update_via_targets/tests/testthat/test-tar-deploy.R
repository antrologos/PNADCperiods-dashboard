# Unit tests for tar-deploy.R (deploy_dashboard_if_eligible).
#
# Mocks `deploy_fn` to avoid hitting rsconnect / shinyapps.io.

source(testthat::test_path("..", "..", "R", "tar-deploy.R"), chdir = FALSE)

# Helper: write a fake deploy.R into a temp dashboard_root so the
# "missing deploy.R" gate doesn't fire spuriously
make_dashboard_root <- function() {
  d <- tempfile("dashboard_root_"); dir.create(d)
  dir.create(file.path(d, "scripts"))
  writeLines("# fake", file.path(d, "scripts", "deploy.R"))
  d
}

ok_pipeline <- function(mode = "live", n_failed = 0L, n_invalid = 0L) {
  list(mode = mode, n_failed = n_failed, n_invalid = n_invalid)
}

# -----------------------------------------------------------------------------
# Early-return gates
# -----------------------------------------------------------------------------

test_that("staging mode => skip with reason='staging mode'", {
  res <- deploy_dashboard_if_eligible(
    pipeline_done   = ok_pipeline(mode = "staging"),
    dashboard_root  = make_dashboard_root(),
    auto_deploy_env = "1",
    deploy_fn       = function(p) stop("must not call!")
  )
  expect_false(res$deployed)
  expect_equal(res$reason, "staging mode")
})

test_that("PNADC_AUTO_DEPLOY != 1 => skip with that reason", {
  res <- deploy_dashboard_if_eligible(
    pipeline_done   = ok_pipeline(),
    dashboard_root  = make_dashboard_root(),
    auto_deploy_env = "0",
    deploy_fn       = function(p) stop("must not call!")
  )
  expect_false(res$deployed)
  expect_equal(res$reason, "PNADC_AUTO_DEPLOY != 1")
})

test_that("pipeline failures => skip", {
  res <- deploy_dashboard_if_eligible(
    pipeline_done   = ok_pipeline(n_failed = 2L),
    dashboard_root  = make_dashboard_root(),
    auto_deploy_env = "1",
    deploy_fn       = function(p) stop("must not call!")
  )
  expect_false(res$deployed)
  expect_equal(res$reason, "pipeline had failures/invalids")
})

test_that("pipeline invalids => skip", {
  res <- deploy_dashboard_if_eligible(
    pipeline_done   = ok_pipeline(n_invalid = 1L),
    dashboard_root  = make_dashboard_root(),
    auto_deploy_env = "1",
    deploy_fn       = function(p) stop("must not call!")
  )
  expect_false(res$deployed)
  expect_equal(res$reason, "pipeline had failures/invalids")
})

test_that("missing credentials => skip", {
  withr::with_envvar(c(SHINYAPPS_TOKEN = ""), {
    res <- deploy_dashboard_if_eligible(
      pipeline_done   = ok_pipeline(),
      dashboard_root  = make_dashboard_root(),  # has no .Renviron
      auto_deploy_env = "1",
      deploy_fn       = function(p) stop("must not call!")
    )
    expect_false(res$deployed)
    expect_equal(res$reason, "no SHINYAPPS credentials")
  })
})

test_that("missing deploy.R => skip", {
  d <- tempfile("dashboard_no_deploy_"); dir.create(d)
  withr::with_envvar(c(SHINYAPPS_TOKEN = "fake_token",
                       SHINYAPPS_NAME = "fake"), {
    res <- deploy_dashboard_if_eligible(
      pipeline_done   = ok_pipeline(),
      dashboard_root  = d,
      auto_deploy_env = "1",
      deploy_fn       = function(p) stop("must not call!")
    )
    expect_false(res$deployed)
    expect_equal(res$reason, "deploy.R not found")
  })
})

# -----------------------------------------------------------------------------
# Happy path (mocked)
# -----------------------------------------------------------------------------

test_that("all gates pass => deploy_fn invoked, returns deployed=TRUE", {
  d <- make_dashboard_root()
  called_with <- NULL
  withr::with_envvar(c(SHINYAPPS_TOKEN = "fake_token",
                       SHINYAPPS_NAME = "fake"), {
    res <- deploy_dashboard_if_eligible(
      pipeline_done   = ok_pipeline(),
      dashboard_root  = d,
      auto_deploy_env = "1",
      deploy_fn       = function(p) {
        called_with <<- p
        invisible(NULL)
      }
    )
  })
  expect_true(res$deployed)
  expect_equal(called_with, file.path(d, "scripts", "deploy.R"))
  expect_match(res$url, "fake.shinyapps.io", fixed = TRUE)
})
