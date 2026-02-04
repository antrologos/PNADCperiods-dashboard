# ==============================================================================
# PNADCperiods Dashboard - Global Configuration
# ==============================================================================

# Load required packages
library(shiny)
library(bslib)
library(data.table)
library(ggplot2)
library(plotly)
library(DT)
# Try to load shinyjs, create no-op fallbacks if not available
shinyjs_available <- requireNamespace("shinyjs", quietly = TRUE)
if (shinyjs_available) {
  library(shinyjs)
} else {
  # Create no-op fallback function
  useShinyjs <- function() tags$div()
}

# Try to load bsicons, fall back to shiny icons if not available
if (!requireNamespace("bsicons", quietly = TRUE)) {
  bs_icon <- function(name, ...) {
    # Map common bootstrap icon names to Font Awesome equivalents
    fa_map <- list(
      "graph-up" = "chart-line",
      "map" = "map",
      "bar-chart-line" = "chart-bar",
      "currency-dollar" = "dollar-sign",
      "info-circle" = "info-circle",
      "arrow-down" = "arrow-down",
      "arrow-up" = "arrow-up",
      "dash" = "minus",
      "calendar" = "calendar",
      "percent" = "percent",
      "github" = "github",
      "book" = "book",
      "building" = "building"
    )
    icon_name <- if (name %in% names(fa_map)) fa_map[[name]] else name
    icon(icon_name, ...)
  }
} else {
  library(bsicons)
}

# ==============================================================================
# Theme Configuration
# ==============================================================================

app_theme <- bs_theme(

version = 5,
bootswatch = "flatly",
primary = "#1976D2",
secondary = "#E53935",
base_font = font_google("Inter"),
heading_font = font_google("Poppins")
)

# ==============================================================================
# Source Utility Files
# ==============================================================================

# Source internationalization (i18n) system
source("R/i18n.R")

# Source de-seasonalization utilities
source("R/utils_deseasonalize.R")

# ==============================================================================
# Default Language Setting
# ==============================================================================

# Default language for the app (can be overridden by user preference)
default_lang <- "pt"

# ==============================================================================
# Data Loading
# ==============================================================================

# Load precomputed data (if available)
load_app_data <- function() {
  data_dir <- "data"

  # Initialize empty list for data
  app_data <- list(
    monthly_sidra = NULL,
    rolling_quarters = NULL,
    series_metadata = NULL,
    deseasonalized_cache = NULL,
    last_updated = NULL
  )

  # Load series metadata
  metadata_path <- file.path(data_dir, "series_metadata.rds")
  if (file.exists(metadata_path)) {
    app_data$series_metadata <- readRDS(metadata_path)
  } else {
    # Try to get from package if available
    if (requireNamespace("PNADCperiods", quietly = TRUE)) {
      app_data$series_metadata <- PNADCperiods::get_sidra_series_metadata()
      # Save for future use
      if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
      saveRDS(app_data$series_metadata, metadata_path)
    }
  }

  # Load mensalized SIDRA series
  monthly_path <- file.path(data_dir, "monthly_sidra.rds")
  if (file.exists(monthly_path)) {
    app_data$monthly_sidra <- readRDS(monthly_path)
    app_data$last_updated <- file.mtime(monthly_path)
  }

  # Load rolling quarters
  rolling_path <- file.path(data_dir, "rolling_quarters.rds")
  if (file.exists(rolling_path)) {
    app_data$rolling_quarters <- readRDS(rolling_path)
  }

  # Load precomputed de-seasonalized series (if available)
  deseason_path <- file.path(data_dir, "deseasonalized_cache.rds")
  if (file.exists(deseason_path)) {
    app_data$deseasonalized_cache <- readRDS(deseason_path)
  }

  app_data
}

# Load data at startup
app_data <- load_app_data()

# ==============================================================================
# Helper Functions
# ==============================================================================

# Format numbers with Brazilian locale (thousands separator)
format_br <- function(x, digits = 1) {
  formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
}

# Convert YYYYMM to Date
yyyymm_to_date <- function(yyyymm) {
  as.Date(paste0(substr(yyyymm, 1, 4), "-", substr(yyyymm, 5, 6), "-15"))
}

# Note: Category/series choice functions are now in R/i18n.R with i18n support:
# - get_theme_choices(metadata, lang)
# - get_theme_category_choices(metadata, selected_theme, lang)
# - get_subcategory_choices(metadata, selected_theme, selected_category, lang)
# - get_series_choices(metadata, selected_theme, selected_category, selected_subcategory, lang)

# ==============================================================================
# IBGE Color Palette
# ==============================================================================

ibge_colors <- list(
  primary = "#1976D2",
  secondary = "#E53935",
  gray = "#666666",
  light_gray = "#CCCCCC",
  background = "#F5F5F5"
)

# ==============================================================================
# Plot Theme
# ==============================================================================

theme_pnadc <- function(base_size = 14) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size * 1.2),
      plot.subtitle = element_text(color = "gray50"),
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_blank()
    )
}
