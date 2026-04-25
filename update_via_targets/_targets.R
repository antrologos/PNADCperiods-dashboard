# ==============================================================================
# _targets.R — orchestrator for the PNADCperiods microdata pipeline
#
# Three layers:
#  1) Acervo custody  (D:/Dropbox/Bancos_Dados/PNADC/...)
#  2) Project cache   (data/processed/prepared_microdata.fst)
#  3) Dashboard assets (PNADCperiods-dashboard/data/*.rds)
#
# SIDRA assets (.qs2) are NOT produced here — they come from the GitHub Actions
# workflow (.github/workflows/sidra-daily.yml) and are tracked as external
# inputs via tar_target(..., format = "file"). When the workflow refreshes the
# release `data-latest`, the deploy script populates data/*.qs2; targets sees
# the changed hash and re-validates the dashboard bundle.
#
# Usage:
#   Sys.setenv(TARGETS_STORE = "D:/targets_store/mensalizacao_pnad")
#   targets::tar_visnetwork()
#   targets::tar_make()
#
# Dry-run (no downloads, no I/O on acervo):
#   Sys.setenv(ACERVO_DRY_RUN = "1")
#   targets::tar_make()
#
# Staging vs. live writes (migration window):
#   Sys.setenv(PNADC_PIPELINE_MODE = "staging")  # default
#   Sys.setenv(PNADC_PIPELINE_MODE = "live")     # cutover
# ==============================================================================

# ------------------------------------------------------------------------------
# Bootstrap: source helpers and pin packages
# ------------------------------------------------------------------------------

source("_targets_packages.R")

library(targets)
library(tarchetypes)

# Resolve the targets store from TARGETS_STORE env var (preferred) or default.
# Done programmatically so _targets.yaml stays a plain literal file.
local({
  store_path <- Sys.getenv("TARGETS_STORE", "_targets")
  targets::tar_config_set(store = store_path)
})

# Source pipeline R/ files in declaration order
for (f in list.files("R", pattern = "^tar-.*\\.R$", full.names = TRUE)) {
  source(f)
}

# ------------------------------------------------------------------------------
# Global tar_option_set
# ------------------------------------------------------------------------------

tar_option_set(
  packages = tar_pipeline_packages,
  imports  = "PNADCperiods",          # invalidates downstream when CRAN pkg changes
  format   = "rds",
  memory   = "transient",
  garbage_collection = TRUE,
  storage  = "main",
  retrieval = "main",
  error    = "abridge",
  workspace_on_error = TRUE
)

# ------------------------------------------------------------------------------
# Targets
#
# `current_year` and `current_quarter` are computed inside the target body via
# `Sys.Date()` AND have cue = always, so each tar_make() picks up the calendar
# year/quarter at run time (not at script-parse time). Downstream targets that
# depend on them are re-evaluated, so a 2027 January `tar_make()` will see
# year 2027 and add the new quarter to `expected_quarters`.
# ------------------------------------------------------------------------------

list(

  # --------------------------------------------------------------------------
  # Configuration targets
  # --------------------------------------------------------------------------

  tar_target(
    current_year,
    as.integer(format(Sys.Date(), "%Y")),
    cue = tar_cue(mode = "always")
  ),

  tar_target(
    current_quarter,
    ceiling(as.integer(format(Sys.Date(), "%m")) / 3),
    cue = tar_cue(mode = "always")
  ),

  tar_target(acervo_root,           tar_acervo_root()),
  tar_target(processed_cache_dir,   tar_processed_cache_dir()),
  tar_target(dashboard_data_dir,    tar_dashboard_data_dir()),

  # `dashboard_data_dest` resolves to data/ (live) or data/_new/ (staging)
  # based on Sys.getenv("PNADC_PIPELINE_MODE"). Env vars are NOT tracked
  # inputs; cue = always re-evaluates each tar_make() so a session-level
  # toggle (staging -> live) propagates without manual tar_invalidate().
  tar_target(
    dashboard_data_dest,
    resolve_dest_dir(dashboard_data_dir),
    cue = tar_cue(mode = "always")
  ),

  tar_target(
    utils_inequality_path,
    file.path(tar_dashboard_root(), "R", "utils_inequality.R"),
    format = "file"
  ),

  # --------------------------------------------------------------------------
  # T0 migration backup — runs once before any output is written.
  # Idempotent: skips files that already have an identical .bak.
  # --------------------------------------------------------------------------

  # T0 backup of any pre-existing live assets, idempotent via md5. Captures
  # only the LIVE folder paths regardless of PNADC_PIPELINE_MODE — the goal
  # is to preserve the user's existing dashboard inputs before the pipeline
  # ever overwrites them. (Staging-mode runs write to data/_new/ and never
  # touch the live folder, so no extra coverage is needed.)
  tar_target(
    t0_backup_targets,
    {
      paths_to_backup <- c(
        file.path(processed_cache_dir, "prepared_microdata.fst"),
        file.path(dashboard_data_dir, "inequality_data.rds"),
        file.path(dashboard_data_dir, "income_shares_data.rds"),
        file.path(dashboard_data_dir, "lorenz_data.rds"),
        file.path(dashboard_data_dir, "income_decomposition_data.rds"),
        file.path(dashboard_data_dir, "poverty_data.rds"),
        file.path(dashboard_data_dir, "state_monthly_data.rds"),
        file.path(dashboard_data_dir, "geographic_data.rds"),
        file.path(dashboard_data_dir, "brazil_states_sf.rds")
      )
      t0_migration_check(
        paths_to_backup,
        archive_dir = file.path(processed_cache_dir, "_pre_pipeline_backup")
      )
    },
    cue = tar_cue(mode = "always")
  ),

  # --------------------------------------------------------------------------
  # Camada 1 — acervo custody
  # --------------------------------------------------------------------------

  tar_target(
    expected_quarters,
    list_expected_quarters(current_year, current_quarter)
  ),

  tar_target(
    expected_visits,
    list_expected_visits(current_year)
  ),

  tar_target(
    expected_deflator,
    list_expected_deflator(current_year)
  ),

  tar_target(
    quarterly_inventory,
    inventory_local(
      acervo_subpaths(acervo_root)$quarterly,
      pattern = "^pnadc_\\d{4}-[1-4]q\\.fst$"
    ),
    cue = tar_cue(mode = "always")
  ),

  tar_target(
    annual_inventory,
    inventory_local(
      acervo_subpaths(acervo_root)$annual,
      pattern = "^pnadc_\\d{4}_visita[1-5]\\.fst$"
    ),
    cue = tar_cue(mode = "always")
  ),

  tar_target(
    deflator_inventory,
    inventory_local(
      acervo_subpaths(acervo_root)$deflator,
      pattern = "^deflator_pnadc_\\d{4}\\.xls$"
    ),
    cue = tar_cue(mode = "always")
  ),

  # Plan: each expected file is OK (already local) or MISSING (will be
  # downloaded). Republication / reweighting detection is OUT of band — when
  # IBGE reweights, the user removes the local file and reruns tar_make().
  tar_target(
    quarterly_plan,
    {
      p <- plan_acervo_actions(
        file_type = "quarterly",
        expected = expected_quarters,
        local_inventory = quarterly_inventory
      )
      p[, file_type := "quarterly"]
      p[]
    }
  ),

  tar_target(
    annual_plan,
    {
      p <- plan_acervo_actions(
        file_type = "annual",
        expected = expected_visits,
        local_inventory = annual_inventory
      )
      p[, file_type := "annual"]
      p[]
    }
  ),

  tar_target(
    quarterly_manifest_partial,
    apply_acervo_plan(
      plan = quarterly_plan,
      file_type = "quarterly",
      dest_dir = acervo_subpaths(acervo_root)$quarterly
    ),
    error = "continue"
  ),

  tar_target(
    annual_manifest_partial,
    apply_acervo_plan(
      plan = annual_plan,
      file_type = "annual",
      dest_dir = acervo_subpaths(acervo_root)$annual
    ),
    error = "continue"
  ),

  # Validate downloaded files (only those flagged DOWNLOADED_NEW)
  tar_target(
    quarterly_manifest,
    {
      m <- data.table::copy(quarterly_manifest_partial)
      m[, validation_ok := TRUE]
      m[, validation_reason := NA_character_]
      idx <- which(m$status == "DOWNLOADED_NEW" & !is.na(m$local_path))
      for (i in idx) {
        v <- validate_downloaded_file(m$local_path[i], "quarterly", m$year[i])
        if (!isTRUE(v$ok)) {
          m[i, `:=`(status = "INVALID",
                    validation_ok = FALSE,
                    validation_reason = v$reason)]
        } else {
          m[i, n_rows := v$n_rows]
        }
      }
      m[]
    }
  ),

  tar_target(
    annual_manifest,
    {
      m <- data.table::copy(annual_manifest_partial)
      m[, validation_ok := TRUE]
      m[, validation_reason := NA_character_]
      idx <- which(m$status == "DOWNLOADED_NEW" & !is.na(m$local_path))
      for (i in idx) {
        v <- validate_downloaded_file(m$local_path[i], "annual", m$year[i])
        if (!isTRUE(v$ok)) {
          m[i, `:=`(status = "INVALID",
                    validation_ok = FALSE,
                    validation_reason = v$reason)]
        } else {
          m[i, n_rows := v$n_rows]
        }
      }
      m[]
    }
  ),

  # Combined manifest used by Layer 2
  tar_target(
    acervo_manifest,
    data.table::rbindlist(
      list(quarterly_manifest, annual_manifest),
      use.names = TRUE, fill = TRUE
    )
  ),

  # CSV sidecar (for git-diff inspection)
  tar_target(
    acervo_manifest_csv,
    {
      path <- file.path(processed_cache_dir, "acervo_manifest.csv")
      write_manifest_csv(acervo_manifest, path)
      path
    },
    format = "file"
  ),

  # Deflator file path target
  tar_target(
    deflator_path,
    {
      candidates <- list.files(
        acervo_subpaths(acervo_root)$deflator,
        pattern = "^deflator_pnadc_\\d{4}\\.xls$",
        full.names = TRUE
      )
      if (!length(candidates)) {
        stop("Deflator file not found in ",
             acervo_subpaths(acervo_root)$deflator, call. = FALSE)
      }
      # Pick the latest year
      yrs <- as.integer(sub(".*deflator_pnadc_(\\d{4})\\.xls", "\\1",
                            basename(candidates)))
      candidates[which.max(yrs)]
    },
    format = "file"
  ),

  # --------------------------------------------------------------------------
  # Camada 2 — prepared_microdata.fst
  # --------------------------------------------------------------------------

  # Path destination depends on PNADC_PIPELINE_MODE (env var, not a tracked
  # input). Resolve via a cue=always sub-target so cutover is automatic.
  tar_target(
    processed_cache_dest,
    if (Sys.getenv("PNADC_PIPELINE_MODE", "staging") == "live")
      processed_cache_dir
    else
      file.path(processed_cache_dir, "_new"),
    cue = tar_cue(mode = "always")
  ),

  tar_target(
    prepared_microdata_fst,
    {
      dir.create(processed_cache_dest, recursive = TRUE, showWarnings = FALSE)
      dest <- file.path(processed_cache_dest, "prepared_microdata.fst")
      build_prepared_microdata(
        acervo_manifest = acervo_manifest,
        deflator_path = deflator_path,
        dest_path = dest,
        utils_inequality_path = utils_inequality_path
      )
    },
    format = "file"
  ),

  # --------------------------------------------------------------------------
  # Camada 3 — dashboard assets
  # --------------------------------------------------------------------------

  tar_target(
    inequality_assets,
    build_inequality_outputs(
      prepared_microdata_path = prepared_microdata_fst,
      dest_dir = dashboard_data_dest,
      utils_inequality_path = utils_inequality_path
    ),
    format = "file"
  ),

  tar_target(
    poverty_asset,
    build_poverty_outputs(
      prepared_microdata_path = prepared_microdata_fst,
      dest_dir = dashboard_data_dest,
      utils_inequality_path = utils_inequality_path
    ),
    format = "file"
  ),

  tar_target(
    state_monthly_asset,
    build_state_monthly(
      acervo_manifest = acervo_manifest,
      dest_path = file.path(dashboard_data_dest, "state_monthly_data.rds")
    ),
    format = "file"
  ),

  tar_target(
    brazil_states_sf_asset,
    build_brazil_states_sf(
      dest_path = file.path(dashboard_data_dest, "brazil_states_sf.rds")
    ),
    format = "file",
    cue = tar_cue(mode = "thorough")  # one-off; rebuild only on full clean
  ),

  tar_target(
    geographic_fallback_asset,
    build_geographic_fallback(
      dest_path = file.path(dashboard_data_dest, "geographic_data.rds")
    ),
    format = "file",
    cue = tar_cue(mode = "thorough")
  ),

  # --------------------------------------------------------------------------
  # SIDRA inputs — external files produced by GitHub Actions, tracked here
  # so dashboard_validation invalidates when they change.
  # --------------------------------------------------------------------------

  tar_target(
    sidra_series_metadata_qs2,
    file.path(dashboard_data_dir, "series_metadata.qs2"),
    format = "file",
    error = "continue"   # may not exist on first run
  ),

  tar_target(
    sidra_monthly_qs2,
    file.path(dashboard_data_dir, "monthly_sidra.qs2"),
    format = "file",
    error = "continue"
  ),

  tar_target(
    sidra_rolling_qs2,
    file.path(dashboard_data_dir, "rolling_quarters.qs2"),
    format = "file",
    error = "continue"
  ),

  tar_target(
    sidra_deseasonalized_qs2,
    file.path(dashboard_data_dir, "deseasonalized_cache.qs2"),
    format = "file",
    error = "continue"
  ),

  # --------------------------------------------------------------------------
  # Validation gate: read every Layer-3 .rds and check schema + row counts.
  # Throws on failure; success returns the asset_paths list.
  # --------------------------------------------------------------------------

  tar_target(
    dashboard_validation,
    {
      asset_paths <- list(
        inequality_data           = grep("inequality_data\\.rds$",
                                          inequality_assets, value = TRUE),
        income_shares_data        = grep("income_shares_data\\.rds$",
                                          inequality_assets, value = TRUE),
        lorenz_data               = grep("lorenz_data\\.rds$",
                                          inequality_assets, value = TRUE),
        income_decomposition_data = grep("income_decomposition_data\\.rds$",
                                          inequality_assets, value = TRUE),
        poverty_data              = poverty_asset,
        state_monthly_data        = state_monthly_asset,
        brazil_states_sf          = brazil_states_sf_asset,
        geographic_data           = geographic_fallback_asset
      )
      validate_all_assets(asset_paths)
      asset_paths
    }
  ),

  # --------------------------------------------------------------------------
  # End marker — depending on this target builds everything
  # --------------------------------------------------------------------------

  tar_target(
    pipeline_done,
    {
      list(
        ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        manifest = nrow(acervo_manifest),
        validation = vapply(dashboard_validation,
                            function(x) isTRUE(x$ok), logical(1L)),
        mode = Sys.getenv("PNADC_PIPELINE_MODE", "staging")
      )
    }
  )

)
