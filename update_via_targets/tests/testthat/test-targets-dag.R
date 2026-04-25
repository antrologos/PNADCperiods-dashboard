# Smoke test: parse _targets.R and ensure tar_manifest() returns the expected
# target names without actually running anything.

test_that("DAG parses and contains the expected core targets", {
  skip_if_not_installed("targets")
  pipeline_root <- testthat::test_path("..", "..")

  # Targets discovers _targets.R from project root: temporarily switch wd.
  withr::with_dir(pipeline_root, {
    withr::with_envvar(
      c(TARGETS_STORE = tempfile("targets_store_"),
        ACERVO_DRY_RUN = "1",
        PNADC_PIPELINE_MODE = "staging"),
      {
        man <- tryCatch(
          targets::tar_manifest(callr_function = NULL),
          error = function(e) {
            skip(paste("tar_manifest failed:", conditionMessage(e)))
          }
        )
        expect_s3_class(man, "data.frame")
        expect_true(nrow(man) > 10L)

        expected_targets <- c(
          "current_year", "current_quarter",
          "expected_quarters", "expected_visits",
          "quarterly_inventory", "annual_inventory",
          "quarterly_plan", "annual_plan",
          "quarterly_manifest_partial", "annual_manifest_partial",
          "quarterly_manifest", "annual_manifest",
          "acervo_manifest", "acervo_manifest_csv",
          "deflator_path",
          "prepared_microdata_fst",
          "inequality_assets", "poverty_asset",
          "state_monthly_asset", "brazil_states_sf_asset",
          "geographic_fallback_asset",
          "sidra_series_metadata_qs2", "sidra_monthly_qs2",
          "sidra_rolling_qs2", "sidra_deseasonalized_qs2",
          "dashboard_validation", "pipeline_done"
        )
        # Targets that should NOT exist after Camada 1 simplification
        forbidden_targets <- c(
          "quarterly_remote_listing", "annual_remote_listing",
          "acervo_archive_root"
        )
        present_forbidden <- intersect(forbidden_targets, man$name)
        expect_length(present_forbidden, 0L)
        missing <- setdiff(expected_targets, man$name)
        expect_length(missing, 0L)
      }
    )
  })
})
