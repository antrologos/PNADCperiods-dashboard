library(testthat)

# Tests run from update_via_targets/ as working directory.
test_check_paths <- function() {
  testthat::test_dir(testthat::test_path())
}

test_check_paths()
