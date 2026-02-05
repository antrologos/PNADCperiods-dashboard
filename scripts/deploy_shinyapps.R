# ==============================================================================
# Deploy PNADCperiods Dashboard to ShinyApps.io
# ==============================================================================
#
# This script reads credentials from environment variables for security.
#
# Setup (one-time):
# 1. Create a .Renviron file in the app directory with:
#    SHINYAPPS_NAME=your_account_name
#    SHINYAPPS_TOKEN=your_token
#    SHINYAPPS_SECRET=your_secret
# 2. The .Renviron file is already in .gitignore
#
# Usage:
#   source("scripts/deploy_shinyapps.R")
#
# ==============================================================================

library(rsconnect)

# Disable renv's strict validation to work around locked packages
options(renv.snapshot.validate = FALSE)
Sys.setenv("RENV_SNAPSHOT_VALIDATE" = "FALSE")

# Force using packrat mode instead of renv (which is stricter)
options(rsconnect.packrat = TRUE)

# Load environment variables from .Renviron if in app directory
app_dir <- normalizePath("d:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods-dashboard")
renviron_path <- file.path(app_dir, ".Renviron")

if (file.exists(renviron_path)) {
  readRenviron(renviron_path)
  cat("Loaded credentials from .Renviron\n")
}

# Get credentials from environment
account_name <- Sys.getenv("SHINYAPPS_NAME")
token <- Sys.getenv("SHINYAPPS_TOKEN")
secret <- Sys.getenv("SHINYAPPS_SECRET")

# Validate credentials
if (account_name == "" || token == "" || secret == "") {
  stop("Missing credentials. Please set SHINYAPPS_NAME, SHINYAPPS_TOKEN, and SHINYAPPS_SECRET in .Renviron")
}

cat("Configuring account:", account_name, "\n")

# Configure account
rsconnect::setAccountInfo(
  name   = account_name,
  token  = token,
  secret = secret
)

cat("Account configured successfully!\n\n")

# Deploy
cat("Deploying app from:", app_dir, "\n")
cat("This may take a few minutes...\n\n")

# Specify only the files needed for the app (exclude scripts and test files)
app_files <- c(
  "app.R",
  "global.R",
  "DESCRIPTION",
  "R/i18n.R",
  "R/mod_about.R",
  "R/mod_geographic.R",
  "R/mod_series_explorer.R",
  "R/utils_deseasonalize.R",
  "data/monthly_sidra.rds",
  "data/rolling_quarters.rds",
  "data/series_metadata.rds",
  "data/geographic_data.rds",
  "data/brazil_states_sf.rds",
  "www/custom.css"
)

# Only include files that exist
app_files <- app_files[file.exists(file.path(app_dir, app_files))]

rsconnect::deployApp(
  appDir = app_dir,
  appFiles = app_files,
  appName = "PNADCperiods-dashboard",
  appTitle = "PNADCperiods Dashboard",
  account = account_name,
  forceUpdate = TRUE
)

cat("\n=== Deployment Complete ===\n")
cat("Your app is available at:\n")
cat(paste0("https://", account_name, ".shinyapps.io/PNADCperiods-dashboard/\n"))
