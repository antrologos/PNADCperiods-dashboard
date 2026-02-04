# Test backwards compatibility with old metadata structure
# Run this script to verify the app works with existing precomputed data

# Load required packages
suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(data.table)
})

# Source global config and i18n
setwd("d:/Dropbox/Artigos/mensalizacao_pnad/PNADCperiods-dashboard")
source("global.R")

# Check what we have
cat("=== Testing Backwards Compatibility ===\n\n")

# Check metadata structure
cat("1. Metadata structure:\n")
meta <- app_data$series_metadata
if (!is.null(meta)) {
  cat("   - Columns:", paste(names(meta), collapse = ", "), "\n")
  cat("   - Has 'theme' column:", "theme" %in% names(meta), "\n")
  cat("   - Has 'category' column:", "category" %in% names(meta), "\n")
  cat("   - Number of series:", nrow(meta), "\n")
} else {
  cat("   - WARNING: metadata is NULL\n")
}

# Test get_theme_choices
cat("\n2. Testing get_theme_choices():\n")
themes_pt <- get_theme_choices(meta, "pt")
themes_en <- get_theme_choices(meta, "en")
cat("   - PT themes:", paste(names(themes_pt), collapse = ", "), "\n")
cat("   - EN themes:", paste(names(themes_en), collapse = ", "), "\n")
cat("   - Number of themes:", length(themes_pt), "\n")

# Test get_theme_category_choices (should return empty for old structure)
cat("\n3. Testing get_theme_category_choices():\n")
if (length(themes_pt) > 0) {
  first_theme <- themes_pt[1]
  cats <- get_theme_category_choices(meta, first_theme, "pt")
  cat("   - Categories for '", first_theme, "': ",
      if (length(cats) > 0) paste(names(cats), collapse = ", ") else "(empty - expected for old structure)",
      "\n", sep = "")
}

# Test get_series_choices
cat("\n4. Testing get_series_choices():\n")
if (length(themes_pt) > 0) {
  first_theme <- themes_pt[1]
  series <- get_series_choices(meta, first_theme, NULL, NULL, "pt")
  cat("   - Series for '", first_theme, "': ", length(series), " series\n", sep = "")
  if (length(series) > 0) {
    cat("   - First 3: ", paste(head(names(series), 3), collapse = ", "), "\n", sep = "")
  }
}

# Test with monthly data
cat("\n5. Checking monthly_sidra data:\n")
if (!is.null(app_data$monthly_sidra)) {
  cat("   - Rows:", nrow(app_data$monthly_sidra), "\n")
  cat("   - Columns:", ncol(app_data$monthly_sidra), "\n")
  cat("   - Date range:", range(app_data$monthly_sidra$anomesexato), "\n")
} else {
  cat("   - WARNING: monthly_sidra is NULL\n")
}

cat("\n=== Test Complete ===\n")
cat("If no errors appeared above, backwards compatibility is working.\n")
