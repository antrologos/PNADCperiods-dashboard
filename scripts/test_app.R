# Test that the app loads correctly
setwd("d:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods-dashboard")

cat("=== Testing PNADCperiods Dashboard ===\n\n")

# Test 1: Load packages
cat("Test 1: Loading required packages...\n")
tryCatch({
  library(shiny)
  library(bslib)
  library(data.table)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(bsicons)
  library(shinyjs)
  cat("  PASSED: All packages loaded\n\n")
}, error = function(e) {
  cat("  FAILED:", e$message, "\n\n")
})

# Test 2: Load global.R
cat("Test 2: Loading global.R...\n")
tryCatch({
  source("global.R")
  cat("  PASSED: global.R loaded\n\n")
}, error = function(e) {
  cat("  FAILED:", e$message, "\n\n")
})

# Test 3: Check data files
cat("Test 3: Checking data files...\n")
data_files <- c(
  "data/series_metadata.rds",
  "data/rolling_quarters.rds",
  "data/monthly_sidra.rds"
)
all_exist <- all(file.exists(data_files))
if (all_exist) {
  cat("  PASSED: All data files exist\n")
  for (f in data_files) {
    cat("    -", f, ":", format(file.size(f), big.mark = ","), "bytes\n")
  }
  cat("\n")
} else {
  cat("  FAILED: Some data files missing\n\n")
}

# Test 4: Load data
cat("Test 4: Loading precomputed data...\n")
tryCatch({
  metadata <- readRDS("data/series_metadata.rds")
  rolling <- readRDS("data/rolling_quarters.rds")
  monthly <- readRDS("data/monthly_sidra.rds")
  cat("  PASSED: Data loaded successfully\n")
  cat("    - Metadata:", nrow(metadata), "series\n")
  cat("    - Rolling quarters:", nrow(rolling), "rows x", ncol(rolling), "columns\n")
  cat("    - Monthly:", nrow(monthly), "rows x", ncol(monthly), "columns\n\n")
}, error = function(e) {
  cat("  FAILED:", e$message, "\n\n")
})

# Test 5: Load modules
cat("Test 5: Loading modules...\n")
tryCatch({
  source("R/mod_series_explorer.R")
  source("R/mod_about.R")
  cat("  PASSED: All modules loaded\n\n")
}, error = function(e) {
  cat("  FAILED:", e$message, "\n\n")
})

# Test 6: UI can be created
cat("Test 6: Creating UI...\n")
tryCatch({
  ui <- page_navbar(
    title = "Test",
    theme = app_theme,
    nav_panel("Test", seriesExplorerUI("test"))
  )
  cat("  PASSED: UI created successfully\n\n")
}, error = function(e) {
  cat("  FAILED:", e$message, "\n\n")
})

cat("=== All Tests Completed ===\n")
cat("\nTo run the app, use:\n")
cat('  shiny::runApp("d:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods-dashboard")\n')
