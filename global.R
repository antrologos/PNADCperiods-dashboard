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
library(leaflet)
library(sf)
library(htmltools)

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
      "building" = "building",
      "house" = "home",
      "lightbulb" = "lightbulb"
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

# Source shared constants (release URLs, TOP_SERIES_FOR_PRECOMPUTE)
source("R/constants.R")

# Source internationalization (i18n) system
source("R/i18n.R")

# Source de-seasonalization utilities
source("R/utils_deseasonalize.R")

# Source inequality/poverty utilities
source("R/utils_inequality.R")

# Source SIDRA phantom-month mask (workaround for PNADCperiods <= 0.1.1)
source("R/utils_sidra_mask.R")

# ==============================================================================
# Default Language Setting
# ==============================================================================

# Default language for the app (can be overridden by user preference)
default_lang <- "pt"

# ==============================================================================
# Data Loading
# ==============================================================================

# Lazy loader: reads an RDS file on first access, caches for subsequent calls
make_lazy_loader <- function(path) {
  cache <- NULL
  function() {
    if (is.null(cache) && file.exists(path)) {
      cache <<- readRDS(path)
    }
    cache
  }
}

# ----------------------------------------------------------------------------
# SIDRA bundle: fetch one .qs2 asset from the GitHub release `data-latest`,
# falling back to the bundled file if the network is unavailable or the
# release hasn't been populated yet. Used in load_app_data() at startup.
# ----------------------------------------------------------------------------
fetch_sidra_qs_from_release <- function(filename, fallback_path,
                                        repo = RELEASE_REPO,
                                        tag = RELEASE_TAG,
                                        timeout = RELEASE_TIMEOUT_SECONDS) {
  url <- sprintf("https://github.com/%s/releases/download/%s/%s",
                 repo, tag, filename)
  result <- list(data = NULL, source = "bundled",
                 fetched_at = NULL, url = url)

  ok <- tryCatch({
    tmp <- tempfile(fileext = ".qs2")
    on.exit(try(unlink(tmp), silent = TRUE), add = TRUE)
    resp <- httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_retry(max_tries = 2, backoff = ~ 2) |>
      httr2::req_user_agent("PNADCperiods-dashboard") |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform(path = tmp)
    if (httr2::resp_status(resp) == 200L &&
        file.exists(tmp) && file.size(tmp) > 0L) {
      result$data       <- qs2::qs_read(tmp)
      result$source     <- "release"
      result$fetched_at <- Sys.time()
      TRUE
    } else FALSE
  }, error = function(e) {
    message("Release fetch failed for ", filename, ": ", conditionMessage(e))
    FALSE
  })

  if (!ok && !is.null(fallback_path) && file.exists(fallback_path)) {
    fb <- tryCatch({
      if (grepl("\\.qs2$", fallback_path)) qs2::qs_read(fallback_path)
      else readRDS(fallback_path)
    }, error = function(e) {
      message("Bundled fallback for ", filename, " is corrupted: ",
              conditionMessage(e))
      NULL
    })
    if (!is.null(fb)) {
      result$data       <- fb
      result$source     <- "bundled"
      result$fetched_at <- file.mtime(fallback_path)
    }
  }
  result
}

# Read sidra_log.json from the release. Returns NULL if unavailable.
# All field accesses elsewhere should be defensive (`log[["x"]] %||% default`).
fetch_sidra_log_from_release <- function(repo = RELEASE_REPO,
                                         tag = RELEASE_TAG,
                                         timeout = RELEASE_TIMEOUT_SECONDS) {
  url <- sprintf("https://github.com/%s/releases/download/%s/sidra_log.json",
                 repo, tag)
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_user_agent("PNADCperiods-dashboard") |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) == 200L) {
      jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
    } else NULL
  }, error = function(e) {
    message("sidra_log fetch failed: ", conditionMessage(e)); NULL
  })
}

# Load precomputed data (if available)
# Small/always-needed files are loaded eagerly; large tab-specific files use lazy loaders
load_app_data <- function() {
  data_dir <- "data"

  app_data <- list(
    # Eager data (small, needed by Home / Series Explorer)
    monthly_sidra = NULL,
    rolling_quarters = NULL,
    series_metadata = NULL,
    deseasonalized_cache = NULL,
    last_updated = NULL,
    # SIDRA release provenance
    sidra_source = NULL,
    sidra_fetched_at = NULL,
    sidra_latest_ref_month = NULL,
    sidra_log = NULL,
    # Microdata pipeline provenance (bundled microdata_log.json from tar_make())
    microdata_log = NULL,
    microdata_fetched_at = NULL,
    microdata_pipeline_mode = NULL
  )

  # --- Eager loads (SIDRA: prefer GitHub release, fall back to bundled .qs2/.rds) ---

  sources       <- character(0)
  fetched_times <- as.POSIXct(character(0))
  for (slot in names(RELEASE_QS2_FILES)) {
    fname  <- RELEASE_QS2_FILES[[slot]]
    fb_qs2 <- file.path(data_dir, fname)
    fb_rds <- file.path(data_dir, sub("\\.qs2$", ".rds", fname))
    fallback <- if (file.exists(fb_qs2)) fb_qs2 else fb_rds
    res <- fetch_sidra_qs_from_release(fname, fallback_path = fallback)
    app_data[[slot]] <- res$data
    sources <- c(sources, res$source)
    if (!is.null(res$fetched_at)) {
      fetched_times <- c(fetched_times, res$fetched_at)
    }
  }

  # Defensive runtime mask: even if the release was produced before the
  # Action workaround was deployed (or the Action regressed), strip phantom
  # mensalized values before any module sees them. See R/utils_sidra_mask.R
  # for context. Idempotent when the release is already clean.
  if (!is.null(app_data$monthly_sidra) && !is.null(app_data$rolling_quarters)) {
    app_data$monthly_sidra <- mask_phantom_mensalized(
      app_data$monthly_sidra, app_data$rolling_quarters
    )
  }

  # Read provenance from the log JSON (defensive: any field can be missing).
  log <- fetch_sidra_log_from_release()
  app_data$sidra_log <- log
  if (!is.null(log)) {
    if (!is.null(log[["latest_ref_month"]])) {
      app_data$sidra_latest_ref_month <- log[["latest_ref_month"]]
    }
    if (!is.null(log[["fetched_at"]])) {
      app_data$sidra_fetched_at <- tryCatch(
        as.POSIXct(log[["fetched_at"]],
                   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
        error = function(e) NULL
      )
    }
  }
  if (is.null(app_data$sidra_fetched_at) && length(fetched_times) > 0L) {
    app_data$sidra_fetched_at <- max(fetched_times, na.rm = TRUE)
  }
  app_data$sidra_source <- if (any(sources == "release")) "release" else "bundled"
  # Legacy alias: many places still read `last_updated`
  app_data$last_updated <- app_data$sidra_fetched_at

  # Microdata provenance (bundled JSON written by the targets pipeline).
  mlog_path <- file.path(data_dir, "microdata_log.json")
  if (file.exists(mlog_path)) {
    mlog <- tryCatch(jsonlite::read_json(mlog_path, simplifyVector = FALSE),
                     error = function(e) NULL)
    app_data$microdata_log <- mlog
    if (!is.null(mlog)) {
      if (!is.null(mlog$fetched_at)) {
        app_data$microdata_fetched_at <- tryCatch(
          as.POSIXct(mlog$fetched_at,
                     format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
          error = function(e) NULL
        )
      }
      app_data$microdata_pipeline_mode <- mlog$pipeline_mode
    }
  }

  # --- Lazy loads (tab-specific, loaded on first visit) ---

  # Geographic tab (~540 KB)
  state_monthly_path <- file.path(data_dir, "state_monthly_data.rds")
  geo_path <- file.path(data_dir, "geographic_data.rds")
  if (file.exists(state_monthly_path)) {
    app_data$get_geographic_data <- make_lazy_loader(state_monthly_path)
    app_data$geo_last_updated <- file.mtime(state_monthly_path)
  } else if (file.exists(geo_path)) {
    # Fallback: wrap in a loader that renames field for compatibility
    app_data$get_geographic_data <- local({
      cache <- NULL
      function() {
        if (is.null(cache)) {
          geo_data <- readRDS(geo_path)
          if ("anomesfinaltrimmovel" %in% names(geo_data)) {
            data.table::setnames(geo_data, "anomesfinaltrimmovel", "ref_month")
          }
          cache <<- geo_data
        }
        cache
      }
    })
    app_data$geo_last_updated <- file.mtime(geo_path)
  } else {
    app_data$get_geographic_data <- function() NULL
    app_data$geo_last_updated <- NULL
  }

  sf_path <- file.path(data_dir, "brazil_states_sf.rds")
  app_data$get_brazil_states_sf <- make_lazy_loader(sf_path)

  # Inequality tab (~2.35 MB)
  ineq_path   <- file.path(data_dir, "inequality_data.rds")
  shares_path <- file.path(data_dir, "income_shares_data.rds")
  lorenz_path <- file.path(data_dir, "lorenz_data.rds")
  decomp_path <- file.path(data_dir, "income_decomposition_data.rds")
  app_data$get_inequality_data           <- make_lazy_loader(ineq_path)
  app_data$get_income_shares_data        <- make_lazy_loader(shares_path)
  app_data$get_lorenz_data               <- make_lazy_loader(lorenz_path)
  app_data$get_income_decomposition_data <- make_lazy_loader(decomp_path)
  ineq_existing <- c(ineq_path, shares_path, lorenz_path, decomp_path)
  ineq_existing <- ineq_existing[file.exists(ineq_existing)]
  app_data$ineq_last_updated <- if (length(ineq_existing))
    max(file.mtime(ineq_existing)) else NULL

  # Poverty tab (~2.6 MB)
  pov_path <- file.path(data_dir, "poverty_data.rds")
  app_data$get_poverty_data <- make_lazy_loader(pov_path)
  app_data$pov_last_updated <- if (file.exists(pov_path))
    file.mtime(pov_path) else NULL

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

# Colorblind-friendly palette (avoids red-green confusion)
ibge_colors <- list(
  primary = "#1976D2",      # Blue - good for all color vision types
  secondary = "#FF7043",    # Orange instead of red - colorblind-friendly
  tertiary = "#00ACC1",     # Teal/Cyan instead of green - colorblind-friendly
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
