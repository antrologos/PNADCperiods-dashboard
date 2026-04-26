# Unit tests for tar-watch.R (external_state_check helper).
#
# Network calls (fetch_ibge_ftp_catalog) are mocked via the `fetch_fn`
# parameter so tests don't hit the real IBGE FTP.

source(testthat::test_path("..", "..", "R", "tar-watch.R"), chdir = FALSE)

test_that("compute_external_state returns calendar + ftp_catalog (sans fetched_at)", {
  fake_catalog <- list(fetched_at = "fake", trimestral = list(),
                       anual = list(), fetched_ok = list())
  s <- compute_external_state(
    acervo_root = "irrelevant",
    state_path = NULL,
    fetch_fn = function(state_path) fake_catalog
  )

  expect_named(s, c("calendar", "ftp_catalog"))
  expect_named(s$calendar, c("year", "quarter"))
  expect_type(s$calendar$year, "integer")
  expect_type(s$calendar$quarter, "integer")
  expect_true(s$calendar$quarter >= 1L && s$calendar$quarter <= 4L)
  # fetched_at is stripped to keep the value content-only (idempotency)
  expect_null(s$ftp_catalog$fetched_at)
  expect_named(s$ftp_catalog, c("trimestral", "anual", "fetched_ok"))
})

test_that("compute_external_state passes state_path to fetch_fn", {
  captured <- NULL
  s <- compute_external_state(
    acervo_root = "irrelevant",
    state_path = "/tmp/test_catalog.json",
    fetch_fn = function(state_path) {
      captured <<- state_path
      list(fetched_at = "fake")
    }
  )
  expect_equal(captured, "/tmp/test_catalog.json")
})

test_that("compute_external_state hash differs when ftp_catalog differs", {
  s1 <- compute_external_state(
    acervo_root = "x",
    fetch_fn = function(state_path)
      list(fetched_at = "t1", trimestral = list(dados = list()))
  )
  s2 <- compute_external_state(
    acervo_root = "x",
    fetch_fn = function(state_path)
      list(fetched_at = "t1",
           trimestral = list(dados = list("2026" = data.frame(x = 1))))
  )
  expect_false(identical(s1$ftp_catalog, s2$ftp_catalog))
})
