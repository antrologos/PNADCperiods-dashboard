test_that("resolve_dest_dir respects PNADC_PIPELINE_MODE", {
  source_pipeline_R()
  base <- tempfile("dest_dir_")
  withr::with_envvar(c(PNADC_PIPELINE_MODE = "live"), {
    out <- resolve_dest_dir(base)
    expect_equal(normalizePath(out, winslash = "/", mustWork = FALSE),
                 normalizePath(base, winslash = "/", mustWork = FALSE))
  })
  withr::with_envvar(c(PNADC_PIPELINE_MODE = "staging"), {
    out <- resolve_dest_dir(base)
    expect_match(out, "_new$")
  })
  unlink(base, recursive = TRUE)
})

test_that("t0_migration_check is idempotent on identical files", {
  source_pipeline_R()
  src <- tempfile(fileext = ".rds")
  saveRDS(list(a = 1), src)
  archive <- tempfile("archive_")

  bk1 <- t0_migration_check(src, archive)
  bk2 <- t0_migration_check(src, archive)

  expect_equal(bk1, bk2)  # second call returns same path
  # Only one .bak written
  files <- list.files(archive, pattern = "\\.bak$")
  expect_length(files, 1L)

  unlink(src)
  unlink(archive, recursive = TRUE)
})

test_that("t0_migration_check skips missing files (returns NA)", {
  source_pipeline_R()
  archive <- tempfile("archive_")
  out <- t0_migration_check("/path/that/does/not/exist", archive)
  expect_true(is.na(out))
})
