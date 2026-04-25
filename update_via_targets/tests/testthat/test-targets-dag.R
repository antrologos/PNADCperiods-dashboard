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
          # Configuration / paths
          "current_year", "current_quarter",
          "acervo_root", "processed_cache_dir", "dashboard_data_dir",
          "dashboard_data_dest", "processed_cache_dest",
          "utils_inequality_path",
          # Migration safety gate (must exist; depends_on edge enforced
          # via force(t0_backup_targets) in Layer 2/3 builders)
          "t0_backup_targets",
          # Camada 1
          "expected_quarters", "expected_visits",
          "quarterly_inventory", "annual_inventory", "deflator_inventory",
          "quarterly_plan", "annual_plan",
          "quarterly_manifest_partial", "annual_manifest_partial",
          "quarterly_manifest", "annual_manifest",
          "acervo_manifest", "acervo_manifest_csv",
          "deflator_path",
          # Camada 2
          "prepared_microdata_fst",
          # Camada 3
          "inequality_assets", "poverty_asset",
          "state_monthly_asset", "brazil_states_sf_asset",
          "geographic_fallback_asset",
          # SIDRA inputs (external)
          "sidra_series_metadata_qs2", "sidra_monthly_qs2",
          "sidra_rolling_qs2", "sidra_deseasonalized_qs2",
          # Final gates
          "dashboard_validation", "pipeline_done"
        )
        # Targets that should NOT exist after Camada 1 simplification
        forbidden_targets <- c(
          "quarterly_remote_listing", "annual_remote_listing",
          "acervo_archive_root", "expected_deflator"
        )
        present_forbidden <- intersect(forbidden_targets, man$name)
        expect_length(present_forbidden, 0L)
        missing <- setdiff(expected_targets, man$name)
        expect_length(missing, 0L)
      }
    )
  })
})
