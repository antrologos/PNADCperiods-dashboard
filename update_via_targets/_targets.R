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

  tar_target(acervo_root,           tar_acervo_root()),
  tar_target(acervo_paths,          acervo_subpaths(acervo_root)),
  tar_target(processed_cache_dir,   tar_processed_cache_dir()),
  tar_target(dashboard_data_dir,    tar_dashboard_data_dir()),

  # External-state checker: ONLY target with cue = always. Lists IBGE FTP
  # (~5-15s for 4 directories) + reads system date. Returns a structured
  # catalog (see fetch_ibge_ftp_catalog). Downstream consumers
  # (current_year, current_quarter, *_inventory, *_plan) depend on this
  # so IBGE-side changes (new year folder, new file, reweight, deflator
  # update, year/quarter rollover) auto-invalidate without manual
  # `tar_invalidate(...)`. Sticky cache in processed_cache_dir guards
  # against IBGE outages.
  # See plan: 2026-04-26_ftp-listing-watcher.md.
  tar_target(
    external_state_check,
    compute_external_state(
      acervo_root,
      state_path = file.path(processed_cache_dir, "_ibge_ftp_catalog.json")
    ),
    cue = tar_cue(mode = "always")
  ),

  # Year/quarter sourced from the always-running checker, so calendar
  # rollover (Jan 1, quarter end) propagates without manual invalidation.
  tar_target(
    current_year,
    external_state_check$calendar$year
  ),

  tar_target(
    current_quarter,
    external_state_check$calendar$quarter
  ),

  # `dashboard_data_dest` resolves to data/ (live) or data/_new/ (staging)
  # based on Sys.getenv("PNADC_PIPELINE_MODE"). Env vars are NOT tracked
  # inputs. After Plan 6 (visual-green policy), cutover staging→live
  # requires manual `tar_invalidate(c("processed_cache_dest","dashboard_data_dest"))`.
  tar_target(
    dashboard_data_dest,
    resolve_dest_dir(dashboard_data_dir)
  ),

  # PR2: split utils_inequality.R into 3 files, each tracked separately.
  # A change to (e.g.) measures_poverty.R now invalidates ONLY poverty_asset,
  # not prepared_microdata_fst or inequality_assets.
  tar_target(
    labels_path,
    file.path(tar_dashboard_root(), "R", "labels.R"),
    format = "file"
  ),
  tar_target(
    measures_inequality_path,
    file.path(tar_dashboard_root(), "R", "measures_inequality.R"),
    format = "file"
  ),
  tar_target(
    measures_poverty_path,
    file.path(tar_dashboard_root(), "R", "measures_poverty.R"),
    format = "file"
  ),
  tar_target(
    utils_deseasonalize_path,
    file.path(tar_dashboard_root(), "R", "utils_deseasonalize.R"),
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
        file.path(dashboard_data_dir, "brazil_states_sf.rds")
      )
      t0_migration_check(
        paths_to_backup,
        archive_dir = file.path(processed_cache_dir, "_pre_pipeline_backup")
      )
    }
  ),

  # --------------------------------------------------------------------------
  # Camada 1 — Network resources (PR1: hoisted to L1, fetched ONCE)
  # --------------------------------------------------------------------------

  # INPC factor lookup table: single deflateBR::inpc call covering ALL nominal
  # dates needed downstream (2021-07 for WB lines, 2024-07 for hhinc_pc, MW
  # year-mids 1990..current-1). Replaces 4 scattered API calls.
  tar_target(
    inpc_factor_table,
    compute_inpc_factors(deflation_target_date)
  ),

  # Phase 2: IPCA monthly index from SIDRA (table 1737, var 2266 base
  # dez/1993). Replaces the per-UF IBGE deflator XLS (CO1/CO2/CO3) for
  # all income deflation; one national series for every (Ano,
  # Trimestre, UF) row. Each tar_make() refetches via SIDRA — the
  # series gets new tail months as IBGE publishes them.
  tar_target(
    ipca_index_table,
    fetch_ipca_series(),
    cue = tar_cue(mode = "always")
  ),

  # NOTE: sidra_geographic_raw + geographic_fallback_asset removed (this
  # session). The 3 SIDRA URLs hardcoded in fetch_sidra_geographic never
  # worked completely (2 of 3 always returned HTTP 400 — wrong tab/var
  # combos). Hecksher's reference Stata code derives taxadesocup/taxapartic/
  # nivelocup from microdata directly, and `state_monthly_data.rds`
  # (state_monthly_asset) already provides them at UF-month granularity.
  # global.R prioritises state_monthly_data over the fallback file anyway.

  # --------------------------------------------------------------------------
  # Camada 1 — acervo custody
  # --------------------------------------------------------------------------

  tar_target(
    expected_quarters,
    list_expected_quarters(current_year, current_quarter)
  ),

  # Enumerate ALL 5 visits per year. Pipeline still picks the
  # default visit (1 / 5 for COVID) when building prepared_microdata_fst,
  # but the acervo holds all visits for downstream research use.
  tar_target(
    expected_visits,
    list_expected_visits(current_year, visits = 1L:5L)
  ),

  # The deflator XLS is updated yearly by IBGE at
  # ftp.ibge.gov.br/.../Anual/Microdados/Visita/Documentacao_Geral/
  # `deflator_download` checks if the latest year is present locally and
  # fetches it from the FTP if missing. After Plan 6 (visual-green),
  # detection of new IBGE deflators requires manual
  # `tar_invalidate("deflator_download")`.
  tar_target(
    deflator_download,
    ensure_deflator_downloaded(current_year - 1L, acervo_root),
    format = "file"
  ),

  tar_target(
    quarterly_inventory,
    {
      # Bind invalidation to external_state_check$acervo$quarterly: when
      # a .fst is added/removed/touched in the acervo dir, the hash
      # changes and this inventory re-runs.
      force(external_state_check)
      inventory_local(
        acervo_paths$quarterly,
        pattern = "^pnadc_\\d{4}-[1-4]q\\.fst$"
      )
    }
  ),

  tar_target(
    annual_inventory,
    {
      force(external_state_check)
      inventory_local(
        acervo_paths$annual,
        pattern = "^pnadc_\\d{4}_visita[1-5]\\.fst$"
      )
    }
  ),

  tar_target(
    deflator_inventory,
    # IBGE ships deflator_PNADC_YYYY.xls (uppercase PNADC); rest of the
    # acervo uses lowercase pnadc_*. Only this inventory needs case-
    # insensitive matching — the canonical basenames in expected_quarters
    # / expected_visits are lowercase.
    {
      force(external_state_check)  # auto-invalidate on deflator dir change
      force(deflator_download)     # ensure download attempted before listing
      inventory_local(
        acervo_paths$deflator,
        pattern = "^deflator_pnadc_\\d{4}\\.xls$",
        ignore.case = TRUE
      )
    }
  ),

  # Plan: each expected file is OK (already local) or MISSING (will be
  # downloaded). Republication / reweighting detection is OUT of band — when
  # IBGE reweights, the user removes the local file and reruns tar_make().
  # Sidecar path: tracks the IBGE-side filename + Last-Modified captured
  # at last successful download. plan_acervo_actions compares the FTP
  # catalog NOW against this sidecar to detect IBGE-side updates.
  # NOT format="file" — file may not exist on first run; load_acervo_sidecar
  # handles missing path gracefully (returns empty list).
  tar_target(
    acervo_sidecar_path,
    file.path(acervo_root, ".acervo_catalog.json")
  ),

  tar_target(
    quarterly_plan,
    plan_acervo_actions(
      file_type       = "quarterly",
      expected        = expected_quarters,
      local_inventory = quarterly_inventory,
      remote_catalog  = external_state_check$ftp_catalog$trimestral$dados,
      catalog_sidecar = load_acervo_sidecar(acervo_sidecar_path)
    )[, file_type := "quarterly"][]
  ),

  tar_target(
    annual_plan,
    plan_acervo_actions(
      file_type       = "annual",
      expected        = expected_visits,
      local_inventory = annual_inventory,
      remote_catalog  = external_state_check$ftp_catalog$anual$dados,
      catalog_sidecar = load_acervo_sidecar(acervo_sidecar_path)
    )[, file_type := "annual"][]
  ),

  tar_target(
    quarterly_manifest_partial,
    apply_acervo_plan(
      plan         = quarterly_plan,
      file_type    = "quarterly",
      dest_dir     = acervo_paths$quarterly,
      sidecar      = load_acervo_sidecar(acervo_sidecar_path),
      sidecar_path = acervo_sidecar_path
    ),
    error = "continue"
  ),

  tar_target(
    annual_manifest_partial,
    apply_acervo_plan(
      plan         = annual_plan,
      file_type    = "annual",
      dest_dir     = acervo_paths$annual,
      sidecar      = load_acervo_sidecar(acervo_sidecar_path),
      sidecar_path = acervo_sidecar_path
    ),
    error = "continue"
  ),

  # Validate downloaded files (only those flagged DOWNLOADED_NEW / _UPDATE)
  tar_target(
    quarterly_manifest,
    validate_acervo_manifest(quarterly_manifest_partial, "quarterly")
  ),

  tar_target(
    annual_manifest,
    validate_acervo_manifest(annual_manifest_partial, "annual")
  ),

  # Combined manifest used by Layer 2
  tar_target(
    acervo_manifest,
    data.table::rbindlist(
      list(quarterly_manifest, annual_manifest),
      use.names = TRUE, fill = TRUE
    )
  ),

  # Deflator file path target. Filename matching is case-insensitive
  # because IBGE ships the file as `deflator_PNADC_YYYY.xls` (capital
  # PNADC), but the legacy code and config use lowercase in the regex.
  tar_target(
    deflator_path,
    {
      force(deflator_download)  # ensure latest XLS is on disk first
      candidates <- list.files(
        acervo_paths$deflator,
        pattern = "^deflator_pnadc_\\d{4}\\.xls$",
        full.names = TRUE,
        ignore.case = TRUE
      )
      if (!length(candidates)) {
        stop("Deflator file not found in ",
             acervo_paths$deflator, call. = FALSE)
      }
      # Pick the latest year (regex with ignore.case to match either case)
      yrs <- as.integer(sub(".*deflator_pnadc_(\\d{4})\\.xls", "\\1",
                            basename(candidates), ignore.case = TRUE))
      candidates[which.max(yrs)]
    },
    format = "file"
  ),

  # --------------------------------------------------------------------------
  # Camada 2 — prepared_microdata.fst
  # --------------------------------------------------------------------------

  # Path destination depends on PNADC_PIPELINE_MODE (env var, not a tracked
  # input). After Plan 6 (visual-green), cutover requires manual
  # `tar_invalidate(c("processed_cache_dest","dashboard_data_dest"))`.
  tar_target(
    processed_cache_dest,
    if (Sys.getenv("PNADC_PIPELINE_MODE", "staging") == "live")
      processed_cache_dir
    else
      file.path(processed_cache_dir, "_new")
  ),

  # PR3: single-pass quarterly stack. The 56 .fst files are read ONCE here;
  # crosswalk_target and quarterly_recoded both consume this in-memory target
  # — eliminating the duplicate ~40 GB I/O of the previous design.
  tar_target(
    quarterly_stacked,
    stack_quarterly(quarterly_manifest)
  ),

  # Crosswalk derives from the in-memory stack (PR3); previously
  # re-read 56 .fst inside build_crosswalk_from_quarterly.
  tar_target(
    crosswalk_target,
    PNADCperiods::pnadc_identify_periods(
      quarterly_stacked,
      verbose = TRUE,
      store_date_bounds = TRUE
    )
  ),

  # PR3: ALL quarterly recoding (employed/informal/sector flags + apply_periods
  # + V2009 filter) extracted from build_state_monthly into a dedicated target.
  # state_monthly_asset (and any future quarterly aggregator) consumes this
  # without re-doing the recodes.
  tar_target(
    quarterly_recoded,
    recode_quarterly(
      quarterly_stacked, crosswalk_target,
      ipca_table  = ipca_index_table,
      labels_path = labels_path
    )
  ),

  # PR4: annual stack — read 14 visit-1 .fst files once with income variable
  # harmonization (pre/post-2015 schema reconciliation).
  tar_target(
    annual_stacked,
    stack_annual(annual_manifest)
  ),

  # PR4: deflator XLS parsed to data.table once per tar_make. Replaces the
  # in-builder `readxl::read_excel(deflator_path)` call inside deflate_incomes.
  tar_target(
    deflator_dt,
    read_deflator_xls(deflator_path)
  ),

  # PR4: ALL annual recoding (apply_periods + V2005 filter + deflate +
  # pc_income components + demographic groupings) in one target. Consumed by
  # the thin writer prepared_microdata_fst (and could feed inequality/poverty
  # directly in a future PR).
  tar_target(
    annual_recoded,
    recode_annual(
      annual_stacked = annual_stacked,
      crosswalk      = crosswalk_target,
      ipca_table     = ipca_index_table,
      labels_path    = labels_path
    )
  ),

  # `t0_backup_targets` is referenced via `force()` so that targets'
  # dependency analyser sees an edge. This guarantees the backup
  # actually runs BEFORE prepared_microdata_fst (and the Layer 3 cascade
  # downstream) overwrites any pre-existing live files.
  # PR4: thin writer — selects dashboard cols from `annual_recoded` and writes
  # the .fst (kept for external consumers: legacy scripts, dashboard offline
  # mode, Phase 5 equivalence test).
  tar_target(
    prepared_microdata_fst,
    {
      force(t0_backup_targets)
      dir.create(processed_cache_dest, recursive = TRUE, showWarnings = FALSE)
      dest <- file.path(processed_cache_dest, "prepared_microdata.fst")
      build_prepared_microdata(
        annual_recoded = annual_recoded,
        dest_path = dest
      )
    },
    format = "file"
  ),

  # --------------------------------------------------------------------------
  # Camada 3 — dashboard assets
  # --------------------------------------------------------------------------

  tar_target(
    inequality_assets,
    {
      force(t0_backup_targets)
      build_inequality_outputs(
        prepared_microdata_path = prepared_microdata_fst,
        dest_dir = dashboard_data_dest,
        measures_inequality_path = measures_inequality_path,
        utils_deseasonalize_path = utils_deseasonalize_path
      )
    },
    format = "file"
  ),

  tar_target(
    poverty_asset,
    {
      force(t0_backup_targets)
      build_poverty_outputs(
        prepared_microdata_path = prepared_microdata_fst,
        ipca_table              = ipca_index_table,
        dest_dir                = dashboard_data_dest,
        measures_poverty_path   = measures_poverty_path,
        utils_deseasonalize_path = utils_deseasonalize_path
      )
    },
    format = "file"
  ),

  tar_target(
    state_monthly_asset,
    {
      force(t0_backup_targets)
      build_state_monthly(
        quarterly_recoded = quarterly_recoded,
        dest_path = file.path(dashboard_data_dest, "state_monthly_data.rds")
      )
    },
    format = "file"
  ),

  # PR-quarterly-income: monthly individual labor income aggregates from the
  # mensalized quarterly stack — 4 income vars × {mean, median} = 8 measures,
  # each by overall + 7 demographic breakdowns. Long-format schema mirrors
  # inequality_data.rds so the dashboard's Inequality module can route the
  # extra measures to this asset transparently.
  tar_target(
    quarterly_income_asset,
    {
      force(t0_backup_targets)
      build_quarterly_income_outputs(
        quarterly_recoded = quarterly_recoded,
        dest_dir = dashboard_data_dest,
        measures_inequality_path = measures_inequality_path,
        utils_deseasonalize_path = utils_deseasonalize_path
      )
    },
    format = "file"
  ),

  # geobr fetch + simplify + write — geobr has internal cache so combining
  # network call with the L3 transform doesn't refetch on every run.
  tar_target(
    brazil_states_sf_asset,
    build_brazil_states_sf(
      brazil_states_sf_raw = fetch_brazil_states_sf(year = 2020L),
      dest_path = file.path(dashboard_data_dest, "brazil_states_sf.rds")
    ),
    format = "file"
  ),

  # geographic_fallback_asset removed: see NOTE in L1 section above.
  #
  # NOTE: SIDRA `.qs2` assets are NOT tracked here. They're produced by
  # the GitHub Actions workflow `.github/workflows/sidra-daily.yml` and
  # fetched at dashboard startup via `httr2` from the GitHub release
  # `data-latest`. They never enter this DAG — neither as inputs nor as
  # outputs. (Earlier versions tracked them as `format = "file"` targets
  # but no downstream target consumed those values, so they were
  # vestigial.)

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
        quarterly_income_data     = quarterly_income_asset
      )
      validate_all_assets(asset_paths)
    }
  ),

  # --------------------------------------------------------------------------
  # microdata_log.json — provenance sidecar bundled with .rds for the dashboard
  # to display "Última atualização" badges. Mirror of sidra_log.json
  # (produced by GitHub Actions).
  # --------------------------------------------------------------------------

  tar_target(
    microdata_log,
    {
      force(dashboard_validation)
      ineq_ref <- attr(inequality_assets, "latest_ref_month")
      nrows    <- attr(inequality_assets, "n_rows")
      log <- list(
        fetched_at    = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        pipeline_mode = Sys.getenv("PNADC_PIPELINE_MODE", "staging"),
        assets = list(
          inequality_data           = list(latest_ref_month = ineq_ref,
                                           n_rows = nrows[["inequality_data"]]),
          income_shares_data        = list(latest_ref_month = ineq_ref,
                                           n_rows = nrows[["income_shares_data"]]),
          lorenz_data               = list(latest_ref_month = ineq_ref,
                                           n_rows = nrows[["lorenz_data"]]),
          income_decomposition_data = list(latest_ref_month = ineq_ref,
                                           n_rows = nrows[["income_decomposition_data"]]),
          poverty_data       = list(latest_ref_month = attr(poverty_asset, "latest_ref_month"),
                                    n_rows           = attr(poverty_asset, "n_rows")),
          state_monthly_data = list(latest_ref_month = attr(state_monthly_asset, "latest_ref_month"),
                                    n_rows           = attr(state_monthly_asset, "n_rows")),
          brazil_states_sf   = list(latest_ref_month = NA,
                                    n_rows           = attr(brazil_states_sf_asset, "n_rows")),
          quarterly_income_data = list(latest_ref_month = attr(quarterly_income_asset, "latest_ref_month"),
                                       n_rows           = attr(quarterly_income_asset, "n_rows"))
        )
      )
      dest <- file.path(dashboard_data_dest, "microdata_log.json")
      jsonlite::write_json(log, dest, pretty = TRUE,
                           auto_unbox = TRUE, na = "null")
      dest
    },
    format = "file"
  ),

  # --------------------------------------------------------------------------
  # End marker — depending on this target builds everything
  # --------------------------------------------------------------------------

  tar_target(
    pipeline_done,
    {
      force(microdata_log)
      n_failed  <- sum(acervo_manifest$status == "FAILED",  na.rm = TRUE)
      n_invalid <- sum(acervo_manifest$status == "INVALID", na.rm = TRUE)
      if (n_failed > 0L) {
        warning(sprintf(
          "%d acervo file(s) FAILED to download; affected periods: %s",
          n_failed,
          paste(acervo_manifest[status == "FAILED", basename],
                collapse = ", ")
        ), call. = FALSE)
      }
      if (n_invalid > 0L) {
        warning(sprintf(
          "%d acervo file(s) flagged INVALID by validate_downloaded_file: %s",
          n_invalid,
          paste(acervo_manifest[status == "INVALID", basename],
                collapse = ", ")
        ), call. = FALSE)
      }
      list(
        ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        manifest_total = nrow(acervo_manifest),
        n_ok         = sum(acervo_manifest$status == "OK", na.rm = TRUE),
        n_downloaded = sum(acervo_manifest$status == "DOWNLOADED_NEW",
                           na.rm = TRUE),
        n_updated    = sum(acervo_manifest$status == "DOWNLOADED_UPDATE",
                           na.rm = TRUE),
        n_missing    = sum(acervo_manifest$status == "MISSING", na.rm = TRUE),
        n_missing_upstream = sum(acervo_manifest$status == "MISSING_UPSTREAM",
                                  na.rm = TRUE),
        n_failed     = n_failed,
        n_invalid    = n_invalid,
        validation   = vapply(dashboard_validation,
                              function(x) isTRUE(x$ok), logical(1L)),
        mode         = Sys.getenv("PNADC_PIPELINE_MODE", "staging")
      )
    }
  ),

  # --------------------------------------------------------------------------
  # Final step: deploy to shinyapps.io (gated by mode + env + creds + integrity)
  # --------------------------------------------------------------------------
  #
  # Runs ONLY when pipeline_done invalidates (i.e. some upstream rebuilt).
  # When the DAG is fully cached, dashboard_deployed stays cached too — no
  # spurious redeploys. See R/tar-deploy.R for the gate logic.
  tar_target(
    dashboard_deployed,
    deploy_dashboard_if_eligible(
      pipeline_done   = pipeline_done,
      dashboard_root  = tar_dashboard_root(),
      auto_deploy_env = Sys.getenv("PNADC_AUTO_DEPLOY", "1")
    )
  )

)
