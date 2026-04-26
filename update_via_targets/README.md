# `update_via_targets/` — microdata pipeline

Targets pipeline that produces all microdata-derived dashboard assets
(`inequality_data.rds`, `poverty_data.rds`, `state_monthly_data.rds`, etc.)
from the local PNADC acervo.

## Layers

1. **Custody** (`R/tar-acervo.R`): inventories
   `D:/Dropbox/Bancos_Dados/PNADC/`, downloads missing files via
   `PNADcIBGE`, validates each download (sample read + cardinality check).
   IBGE republication (reweighting) is handled out-of-band: the user
   removes the affected local file and runs `tar_make()` again, which
   detects MISSING and re-downloads.
2. **Cache** (`R/tar-microdata.R::build_prepared_microdata`): produces
   `data/processed/prepared_microdata.fst` (~161 MB, reusable across
   dashboard and papers).
3. **Dashboard assets** (`R/tar-microdata.R::build_*`): produces the 8
   `.rds` consumed by the Shiny app.

SIDRA assets (`*.qs2`) are NOT produced here — they come from the GitHub
Actions workflow `.github/workflows/sidra-daily.yml`. The DAG tracks them
as external `format = "file"` inputs so the validation gate notices when
they change.

## Setup (one-time)

```bash
# Pick a location for the targets store OUTSIDE Dropbox
mkdir -p D:/targets_store/mensalizacao_pnad
```

In `~/.Renviron` (or before each session):

```
TARGETS_STORE=D:/targets_store/mensalizacao_pnad
PNADC_ACERVO_ROOT=D:/Dropbox/Bancos_Dados/PNADC
PNADC_PIPELINE_MODE=staging
```

`PNADC_PIPELINE_MODE`:
  - `staging` (default): writes to `data/processed/_new/` and
    `PNADCperiods-dashboard/data/_new/` — non-destructive during the
    4-week migration window.
  - `live`: writes directly to the live folders.

The destination is resolved each `tar_make()` (cue = always on
`dashboard_data_dest` and `processed_cache_dest`), so toggling the env
var between sessions takes effect on the next run without
`tar_invalidate()`.

## Common commands

```r
# Inspect the DAG before running
targets::tar_visnetwork()
targets::tar_manifest()

# Dry-run the acervo plan without downloading or touching disk
Sys.setenv(ACERVO_DRY_RUN = "1")
targets::tar_make()

# Real run
Sys.setenv(ACERVO_DRY_RUN = "")
targets::tar_make()                      # serial
targets::tar_make_future(workers = 4)    # parallel (after future setup)

# Force re-check of local acervo (e.g., after manually removing a file
# IBGE has just reweighted)
targets::tar_invalidate(c("quarterly_inventory", "annual_inventory"))

# Cutover after equivalence test passes
Sys.setenv(PNADC_PIPELINE_MODE = "live")
targets::tar_invalidate(c("processed_cache_dest", "dashboard_data_dest"))
targets::tar_make()
```

## Visualizing the DAG (honest view)

`targets::tar_visnetwork()` by default colors nodes using `tar_outdated()`
— which is a *conservative static analysis*. Because this pipeline has
ONE `cue = always` target (`external_state_check`), `tar_outdated`
cascades the "outdated" label through every descendant, even though
`tar_make()` actually skips them when the watcher's output hash is
unchanged.

The result: the default visnetwork shows ~45 of 45 nodes as blue
"outdated" while `tar_make()` reports 1 completed + 44 skipped. The
visualization is pessimistic; the execution is the truth.

Use the helper instead — colors nodes by last-run progress only:

```r
source("R/tar-viz.R")
tar_visnetwork_honest()
# equivalent to: targets::tar_visnetwork(outdated = FALSE)
```

After a normal `tar_make()` you'll see `external_state_check` as
"completed" and all 44 downstream nodes as "skipped" (success, cached).
No misleading blue cascade.

## Auto-detection of external changes (FTP-based)

The pipeline has ONE target with `cue = always`: `external_state_check`.
It does HTTP listings of the IBGE FTP and returns a structured catalog
of:

- Trimestral data: every `<year>/PNADC_QQYYYY[_YYYYMMDD].zip` known to IBGE
- Trimestral deflator: `Documentacao/Deflatores.zip` (Last-Modified)
- Anual data: every `Visita_N/Dados/PNADC_YYYY_visitaN[_YYYYMMDD].zip`
  for visits 1..5
- Anual deflator: every `Documentacao_Geral/deflator_PNADC_YYYY.xls`

Plus `Sys.Date()` for calendar rollover (year/quarter).

**Cost:** ~5-10 seconds per `tar_make()` for the listings (network).

**Sticky cache:** the catalog is persisted to
`<processed_cache_dir>/_ibge_ftp_catalog.json`. If IBGE FTP is
unreachable, the watcher returns the last-known catalog so `tar_make()`
doesn't break.

**Sidecar:** `<acervo_root>/.acervo_catalog.json` records the upstream
filename + Last-Modified captured at the last successful download for
each local file. `plan_acervo_actions` compares the FTP listing NOW
against this sidecar to detect IBGE-side updates (whether the filename
changed via reweight suffix, OR Last-Modified advanced with the same
filename — both are signals).

**Status values produced by the plans:**

| Status | Meaning |
|---|---|
| `OK` | local present, sidecar matches FTP |
| `MISSING` | local absent, FTP has it (download via PNADcIBGE) |
| `MISSING_UPSTREAM` | expected (calendar) but FTP doesn't have it (IBGE not yet published) |
| `OUTDATED` | local present, but filename or Last-Modified changed on FTP since last download (rename local to `.OUTDATED.<ts>` + redownload) |
| `DOWNLOADED_NEW` | apply just downloaded a new file |
| `DOWNLOADED_UPDATE` | apply just redownloaded an OUTDATED file |
| `INVALID` | downloaded file failed schema/size validation |
| `FAILED` | download attempt failed |

**Scenarios handled automatically:**

| Scenario | Detection signal | Cascade |
|---|---|---|
| IBGE publishes new year folder (e.g. 2026/) | catalog gains a year key | new (year, quarter) rows MISSING_UPSTREAM until ZIPs appear |
| IBGE publishes new quarter ZIP in existing year | catalog row count grows | row flagged MISSING → download |
| IBGE re-publishes a ZIP with new `_YYYYMMDD` suffix | filename differs from sidecar | row flagged OUTDATED → rename local + redownload |
| IBGE re-publishes a ZIP with same filename but new Last-Modified | Last-Modified > sidecar | row flagged OUTDATED → redownload |
| New deflator XLS (`deflator_PNADC_<year+1>.xls`) | catalog gains a row | (currently downloaded by `deflator_download` target as before; FTP catalog tracks for visibility) |
| Year/quarter rollover | `external_state_check$calendar` shifts | `current_year`/`current_quarter` invalidate |
| IBGE FTP unreachable | listings return NULL → sticky cache used | nothing invalidates |

**Annual visits — pipeline vs. acervo:**

`expected_visits` enumerates ALL 5 visits per year (so the acervo holds
visit_1 + visit_2 + ... + visit_5). The dashboard pipeline still picks
the default per `get_default_visit()` (visit 1 for non-COVID years,
visit 5 for 2020-2021) when building `prepared_microdata_fst`. Other
visits sit in the acervo for downstream research use.

**Result:** `tar_make()` with no external changes shows exactly **1 `+`**
(the FTP-listing watcher) and 49 skips in ~8 seconds. The DAG stays
visually green.

**What still needs manual intervention:**

| Case | Manual command |
|---|---|
| Cutover `PNADC_PIPELINE_MODE` staging→live | `tar_invalidate(c("processed_cache_dest", "dashboard_data_dest"))` |
| Live data files modified out-of-band by an external tool | `tar_invalidate("t0_backup_targets")` |
| You want to force-redeploy even with no upstream changes | `tar_invalidate("dashboard_deployed")` |

When in doubt, `tar_invalidate(everything)` and re-run.

## Auto-deploy as the last DAG step

The final target `dashboard_deployed` deploys to shinyapps.io. It only
runs when the upstream `pipeline_done` rebuilt (i.e., something actually
changed). Gated by:

1. `PNADC_PIPELINE_MODE == "live"` (staging never deploys)
2. `PNADC_AUTO_DEPLOY != "0"` (default `"1"` = enabled; set `"0"` to skip)
3. `SHINYAPPS_TOKEN` present (loaded from `.Renviron` if missing in env)
4. `pipeline_done$n_failed + pipeline_done$n_invalid == 0` (no broken
   downloads)
5. `scripts/deploy.R` exists in dashboard root

Each gate produces an early-return with a `reason` string, so a
`tar_read("dashboard_deployed")` always tells you what happened.

**Skip a deploy without invalidating it manually:** set `PNADC_AUTO_DEPLOY=0`
before `tar_make()` for local iteration. The target re-evaluates each
time `pipeline_done` invalidates, so flipping it back to `"1"` is enough
to enable on the next run.

## Tests

```r
testthat::test_dir("tests/testthat")
```

Six suites:
- `test-tar-config.R`: visit rule, dry-run flag, paths
- `test-tar-acervo.R`: inventory_local, plan_acervo_actions OK/MISSING,
  atomic_rename overwrite, dry-run
- `test-tar-validation.R`: schema checks, gate behaviour
- `test-tar-export.R`: staging-vs-live, idempotent backup
- `test-tar-wrappers.R`: PNADCperiods wrapper signatures
- `test-targets-dag.R`: DAG parses with all expected target names

## First-run expectations

| Scenario | Time |
|----------|------|
| Acervo already populated (~52 GB), dry-run | <1 min |
| Acervo populated, full `tar_make()` | 30–60 min |
| Acervo empty (worst case, ~68 ZIPs via PNADcIBGE) | 6–12 h |
| Incremental (1 new quarter) | 5–10 min |
