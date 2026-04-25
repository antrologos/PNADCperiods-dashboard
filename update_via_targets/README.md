# `update_via_targets/` — microdata pipeline

Targets pipeline that produces all microdata-derived dashboard assets
(`inequality_data.rds`, `poverty_data.rds`, `state_monthly_data.rds`, etc.)
from the local PNADC acervo.

## Layers

1. **Custody** (`R/tar-acervo.R`): inventories `D:/Dropbox/Bancos_Dados/PNADC/`,
   detects upstream republication via IBGE FTP listing, archives + re-downloads
   reweighted files via `PNADcIBGE`, validates downloads.
2. **Cache** (`R/tar-microdata.R::build_prepared_microdata`): produces
   `data/processed/prepared_microdata.fst` (~161 MB, reusable across
   dashboard + papers).
3. **Dashboard assets** (`R/tar-microdata.R::build_*`): produces the
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

# Force re-check of acervo (FTP listing) without touching files
targets::tar_invalidate(c(
  "quarterly_remote_listing", "annual_remote_listing",
  "quarterly_inventory", "annual_inventory"
))

# Cutover after equivalence test passes
Sys.setenv(PNADC_PIPELINE_MODE = "live")
targets::tar_make()
```

## Tests

```r
testthat::test_dir("tests/testthat")
```

Six suites:
- `test-tar-config.R`: visit rule, dry-run flag, paths
- `test-tar-acervo.R`: file listings, IBGE filename parsing, action plan
- `test-tar-validation.R`: schema checks, gate behaviour
- `test-tar-export.R`: staging-vs-live, idempotent backup
- `test-tar-wrappers.R`: PNADCperiods wrapper signatures
- `test-targets-dag.R`: DAG parses with all expected target names

## First-run expectations

| Scenario | Time |
|----------|------|
| Acervo already populated (~52 GB), dry-run | <1 min |
| Acervo populated, full `tar_make()` | 30–60 min |
| Acervo empty (worst case, 68 ZIPs to download) | 6–12 h |
| Incremental (1 new quarter) | 5–10 min |
