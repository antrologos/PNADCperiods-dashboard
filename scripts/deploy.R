# ==============================================================================
# Deploy PNADCperiods Dashboard to ShinyApps.io
# ==============================================================================
#
# Reads credentials from `.Renviron` (gitignored) and pre-populates `data/`
# with the SIDRA `.qs2` assets from the GitHub release `data-latest` BEFORE
# uploading the bundle. Those assets are the runtime fallback when
# `global.R` cannot reach the release at startup.
#
# IMPORTANT: do not call `rsconnect::deployApp()` directly without running
# this script — the bundle would ship without `data/*.qs2` fallback files.
#
# Setup (one-time):
#   .Renviron in this directory must define:
#     SHINYAPPS_NAME=...
#     SHINYAPPS_TOKEN=...
#     SHINYAPPS_SECRET=...
#
# Usage:
#   source("scripts/deploy.R")
# ==============================================================================

# Skip renv snapshot validation (PNADCperiods is installed from local source).
options(renv.config.snapshot.validate = FALSE)

# Force packrat mode: rsconnect's renv path has been flaky in this project.
options(rsconnect.packrat = TRUE)

library(rsconnect)

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

APP_DIR  <- normalizePath(
  "D:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods-dashboard"
)
APP_NAME <- "PNADCperiods-dashboard"  # canonical slug; URL: /PNADCperiods-dashboard/
DATA_DIR <- file.path(APP_DIR, "data")

# GitHub release used for SIDRA .qs2 assets. Override DASHBOARD_RELEASE_TAG
# in `.Renviron` for emergency rollback to `data-prev` or a weekly snapshot.
RELEASE_REPO <- "antrologos/PNADCperiods-dashboard"
RELEASE_TAG  <- Sys.getenv("DASHBOARD_RELEASE_TAG", "data-latest")

ASSETS <- c(
  "series_metadata.qs2",
  "rolling_quarters.qs2",
  "monthly_sidra.qs2",
  "deseasonalized_cache.qs2",
  "sidra_log.json"
)

# ------------------------------------------------------------------------------
# Load shinyapps.io credentials from .Renviron
# ------------------------------------------------------------------------------

renviron_path <- file.path(APP_DIR, ".Renviron")
if (file.exists(renviron_path)) {
  readRenviron(renviron_path)
  cat("Loaded credentials from", renviron_path, "\n")
}

account_name <- Sys.getenv("SHINYAPPS_NAME")
token        <- Sys.getenv("SHINYAPPS_TOKEN")
secret       <- Sys.getenv("SHINYAPPS_SECRET")

if (account_name == "" || token == "" || secret == "") {
  stop("Missing credentials. Set SHINYAPPS_NAME, SHINYAPPS_TOKEN, ",
       "SHINYAPPS_SECRET in .Renviron")
}

cat("Configuring account:", account_name, "\n")
rsconnect::setAccountInfo(name = account_name, token = token, secret = secret)

# ------------------------------------------------------------------------------
# Pre-deploy: download release assets to data/ as runtime fallback
# ------------------------------------------------------------------------------

dir.create(DATA_DIR, showWarnings = FALSE, recursive = TRUE)

cat("\nFetching release", RELEASE_TAG, "from", RELEASE_REPO, "...\n")
for (a in ASSETS) {
  url  <- sprintf("https://github.com/%s/releases/download/%s/%s",
                  RELEASE_REPO, RELEASE_TAG, a)
  dest <- file.path(DATA_DIR, a)
  resp <- httr2::request(url) |>
    httr2::req_timeout(60) |>
    httr2::req_user_agent("PNADCperiods-dashboard-deploy") |>
    httr2::req_error(is_error = function(r) httr2::resp_status(r) >= 400) |>
    httr2::req_perform(path = dest)
  size <- if (file.exists(dest)) file.size(dest) else 0L
  if (size < 100L) {
    stop(sprintf("Pre-deploy download of %s failed or empty (size=%d)", a, size))
  }
  cat(sprintf("  %-30s %d bytes\n", a, size))
}

# ------------------------------------------------------------------------------
# Deploy
# ------------------------------------------------------------------------------

cat("\nDeploying", APP_DIR, "as", APP_NAME, "...\n")
rsconnect::deployApp(
  appDir       = APP_DIR,
  appName      = APP_NAME,
  appTitle     = "PNADCperiods Dashboard",
  account      = account_name,
  forceUpdate  = TRUE
)

cat("\n=== Deployment complete ===\n")
cat(sprintf("URL: https://%s.shinyapps.io/%s/\n", account_name, APP_NAME))
