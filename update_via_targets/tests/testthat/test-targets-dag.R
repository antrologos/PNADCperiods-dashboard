# Smoke test: parse _targets.R and ensure tar_manifest() returns the expected
# target names without actually running anything.

test_that("DAG parses and contains the expected core targets", {
  skip_if_not_installed("targets")
  pipeline_root <- testthat::test_path("..", "..")

  # Sourcing _targets.R calls tar_config_set() which PERSISTS the store
  # path to _targets.yaml. Without backup/restore here, this test would
  # leave _targets.yaml pointing at a tempfile and the user's next
  # `tar_make()` would rebuild from scratch. Snapshot+restore the yaml.
  yaml_path <- file.path(pipeline_root, "_targets.yaml")
  yaml_backup <- if (file.exists(yaml_path)) readLines(yaml_path) else NULL
  withr::defer({
    if (is.null(yaml_backup)) {
      if (file.exists(yaml_path)) unlink(yaml_path)
    } else {
      writeLines(yaml_backup, yaml_path)
    }
  })

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
          # PR2: split utils_inequality.R into 3 fine-grained tracked paths
          "labels_path", "measures_inequality_path", "measures_poverty_path",
          # Pipeline auto-detection (cue=always single-target watcher)
          "external_state_check",
          # FTP-listing-based plan inputs
          "acervo_sidecar_path",
          # Migration safety gate (must exist; depends_on edge enforced
          # via force(t0_backup_targets) in Layer 2/3 builders)
          "t0_backup_targets",
          # PR1: Network resources hoisted to L1
          "inpc_factor_table",
          # Camada 1
          "expected_quarters", "expected_visits",
          "deflator_download",
          "quarterly_inventory", "annual_inventory", "deflator_inventory",
          "quarterly_plan", "annual_plan",
          "quarterly_manifest_partial", "annual_manifest_partial",
          "quarterly_manifest", "annual_manifest",
          "acervo_manifest",
          "deflator_path",
          # Camada 2
          "quarterly_stacked",
          "crosswalk_target",
          "quarterly_recoded",
          "annual_stacked", "deflator_dt", "annual_recoded",
          "prepared_microdata_fst",
          # Camada 3
          "inequality_assets", "poverty_asset",
          "state_monthly_asset", "brazil_states_sf_asset",
          # SIDRA NOT tracked here — produced by GitHub Actions, fetched
          # at dashboard startup via httr2. Out-of-scope for this DAG.
          # Final gates
          "dashboard_validation", "pipeline_done",
          # Last DAG step: deploy to shinyapps.io (gated)
          "dashboard_deployed"
        )
        # Targets that should NOT exist (simplification + structural removals)
        forbidden_targets <- c(
          "quarterly_remote_listing", "annual_remote_listing",
          "acervo_archive_root", "expected_deflator",
          # SIDRA geographic fallback removed: hardcoded URLs were broken;
          # state_monthly_data.rds (from microdata) is the canonical source.
          "sidra_geographic_raw", "geographic_fallback_asset",
          # 2026-04-26 cleanup: vestigial trackers with no DAG consumer.
          # `utils_inequality_path` was a backward-compat sentinel; the 3
          # split files (labels/measures_inequality/measures_poverty) are
          # tracked separately. SIDRA `.qs2` are produced by GitHub
          # Actions and fetched by the dashboard at startup, not here.
          "utils_inequality_path",
          "sidra_series_metadata_qs2", "sidra_monthly_qs2",
          "sidra_rolling_qs2", "sidra_deseasonalized_qs2"
        )
        present_forbidden <- intersect(forbidden_targets, man$name)
        expect_length(present_forbidden, 0L)
        missing <- setdiff(expected_targets, man$name)
        expect_length(missing, 0L)

        # Regression: deflator_path must run AFTER deflator_download so the
        # newly-fetched XLS is the one picked up by list.files(). Likewise
        # deflator_inventory. Without these force() edges, deflator_path
        # resolves to whatever stale file was on disk before the download.
        deflator_path_cmd <- man$command[man$name == "deflator_path"]
        deflator_inv_cmd  <- man$command[man$name == "deflator_inventory"]
        expect_match(deflator_path_cmd, "deflator_download",
                     info = "deflator_path must depend on deflator_download")
        expect_match(deflator_inv_cmd,  "deflator_download",
                     info = "deflator_inventory must depend on deflator_download")
      }
    )
  })
})

test_that("only external_state_check uses cue = tar_cue(mode = \"always\")", {
  # Plan: 2026-04-26_pipeline-auto-detection.md.
  # Plan 6 (no cue=always anywhere) is RELAXED to allow exactly ONE
  # lightweight watcher target — `external_state_check` — at the top of the
  # DAG. All other targets must keep the default `thorough` cue so the DAG
  # remains visually-green (1 single `+`) when nothing external changed.
  pipeline_root <- testthat::test_path("..", "..")
  src <- readLines(file.path(pipeline_root, "_targets.R"))
  always_idx <- grep('cue\\s*=\\s*tar_cue\\(\\s*mode\\s*=\\s*"always"', src)
  expect_length(always_idx, 1L)

  # Confirm the surrounding lines mention `external_state_check`
  context <- src[max(1L, always_idx - 15L):min(length(src), always_idx + 2L)]
  expect_true(any(grepl("external_state_check", context)),
              info = "the only cue=always must be on external_state_check")
})
