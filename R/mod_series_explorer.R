# ==============================================================================
# Series Explorer Module
# ==============================================================================
#
# This module provides the Series Explorer tab functionality.
# It now supports:
#   - Hierarchical series selection (theme → category → subcategory → series)
#   - Internationalization (i18n) for English/Portuguese
#
# ==============================================================================

# Null-coalescing operator (if not available)
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

seriesExplorerUI <- function(id) {

  ns <- NS(id)

  layout_sidebar(
    fillable = TRUE,

    # Sidebar with controls - wider to fit series names (320px)
    sidebar = sidebar(
      width = "320px",
      open = "desktop",
      bg = "white",
      class = "series-explorer-sidebar",

      # Data freshness display
      div(
        class = "mb-3 p-2 rounded",
        style = "background: #f8f9fa; border: 1px solid #e9ecef;",
        div(
          class = "d-flex justify-content-between align-items-center",
          div(
            tags$small(class = "text-muted", style = "font-size: 0.7rem;",
                       textOutput(ns("label_last_updated"), inline = TRUE)),
            tags$div(
              class = "text-primary",
              style = "font-size: 0.8rem; font-weight: 600;",
              textOutput(ns("last_updated"), inline = TRUE)
            )
          ),
          actionButton(
            ns("refresh_sidra"),
            label = NULL,
            icon = icon("sync", class = "fa-sm"),
            class = "btn-sm btn-outline-primary",
            title = "Refresh from SIDRA API",
            style = "width: 28px; height: 28px; padding: 0; border-radius: 50%;"
          )
        )
      ),

      # Theme selection (top level)
      selectInput(
        ns("theme"),
        label = tags$span(style = "font-size: 0.75rem; font-weight: 600;",
                          textOutput(ns("label_theme"), inline = TRUE)),
        choices = NULL,
        selected = NULL
      ),

      # Theme Category selection (middle level)
      selectInput(
        ns("theme_category"),
        label = tags$span(style = "font-size: 0.75rem; font-weight: 600;",
                          textOutput(ns("label_category"), inline = TRUE)),
        choices = NULL,
        selected = NULL
      ),

      # Subcategory selection (optional - only shown when subcategories exist)
      uiOutput(ns("subcategory_ui")),

      # Series selection
      selectInput(
        ns("series"),
        label = tags$span(style = "font-size: 0.75rem; font-weight: 600;",
                          textOutput(ns("label_series"), inline = TRUE)),
        choices = NULL,
        selected = NULL
      ),

      tags$hr(style = "margin: 0.75rem 0;"),

      # Date range
      div(
        style = "padding: 0 5px;",  # Extra padding for slider labels
        sliderInput(
          ns("date_range"),
          label = tags$span(style = "font-size: 0.75rem; font-weight: 600;",
                            textOutput(ns("label_date_range"), inline = TRUE)),
          min = as.Date("2012-03-01"),
          max = Sys.Date(),
          value = c(as.Date("2012-03-01"), Sys.Date()),
          timeFormat = "%Y-%m"
        )
      ),

      tags$hr(style = "margin: 0.75rem 0;"),

      # Display options
      tags$p(style = "font-size: 0.7rem; font-weight: 600; color: #6c757d; margin-bottom: 0.5rem;",
             textOutput(ns("label_display"), inline = TRUE)),

      checkboxInput(
        ns("show_quarterly"),
        label = tags$span(style = "font-size: 0.8rem;",
                          textOutput(ns("label_show_quarterly"), inline = TRUE)),
        value = TRUE
      ),

      checkboxInput(
        ns("show_difference"),
        label = tags$span(style = "font-size: 0.8rem;",
                          textOutput(ns("label_show_difference"), inline = TRUE)),
        value = FALSE
      ),

      tags$hr(style = "margin: 0.75rem 0;"),

      # De-seasonalization options
      tags$p(style = "font-size: 0.7rem; font-weight: 600; color: #6c757d; margin-bottom: 0.5rem;",
             textOutput(ns("label_seasonal_adjustment"), inline = TRUE)),

      # Radio buttons will be rendered dynamically for i18n
      uiOutput(ns("deseasonalize_ui")),

      tags$hr(style = "margin: 0.75rem 0;"),

      # Download buttons
      tags$p(style = "font-size: 0.7rem; font-weight: 600; color: #6c757d; margin-bottom: 0.5rem;",
             textOutput(ns("label_export"), inline = TRUE)),
      div(
        class = "d-flex gap-1 flex-wrap",
        downloadButton(ns("download_csv"), "CSV", class = "btn-sm btn-outline-secondary", style = "font-size: 0.75rem;"),
        downloadButton(ns("download_png"), "PNG", class = "btn-sm btn-outline-secondary", style = "font-size: 0.75rem;"),
        downloadButton(ns("download_xlsx"), "Excel", class = "btn-sm btn-outline-secondary", style = "font-size: 0.75rem;")
      )
    ),

    # Main panel content
    div(
      class = "p-3",

      # Main plot card
      div(
        class = "card mb-3",
        style = "border: none; box-shadow: 0 1px 3px rgba(0,0,0,0.08);",
        div(
          class = "card-header bg-white d-flex justify-content-between align-items-center",
          style = "padding: 0.75rem 1rem; border-bottom: 1px solid #eee;",
          tags$h6(class = "mb-0", style = "font-weight: 600; font-size: 0.9rem;",
                  textOutput(ns("plot_title"), inline = TRUE)),
          popover(
            icon("info-circle", class = "text-muted", style = "font-size: 0.9rem; cursor: pointer;"),
            title = "Series Info",
            textOutput(ns("series_description"))
          )
        ),
        div(
          class = "card-body",
          style = "padding: 0.5rem;",
          plotlyOutput(ns("main_plot"), height = "380px")
        )
      ),

      # Conditional difference plot
      conditionalPanel(
        condition = sprintf("input['%s']", ns("show_difference")),
        div(
          class = "card mb-3",
          style = "border: none; box-shadow: 0 1px 3px rgba(0,0,0,0.08);",
          div(
            class = "card-header bg-white",
            style = "padding: 0.5rem 1rem; border-bottom: 1px solid #eee;",
            tags$span(style = "font-weight: 600; font-size: 0.85rem;",
                      textOutput(ns("difference_plot_title"), inline = TRUE))
          ),
          div(
            class = "card-body",
            style = "padding: 0.5rem;",
            plotlyOutput(ns("difference_plot"), height = "180px")
          )
        )
      ),

      # Summary statistics - simple horizontal layout
      div(
        class = "card mb-3",
        style = "border: none; box-shadow: 0 1px 3px rgba(0,0,0,0.08);",
        div(
          class = "card-header bg-white",
          style = "padding: 0.5rem 1rem; border-bottom: 1px solid #eee;",
          tags$span(style = "font-weight: 600; font-size: 0.85rem;",
                    textOutput(ns("label_summary_stats"), inline = TRUE))
        ),
        div(
          class = "card-body",
          style = "padding: 1rem;",
          div(
            class = "row g-3",
            # Minimum
            div(
              class = "col",
              div(
                style = "text-align: center; padding: 0.5rem; background: #f8f9fa; border-radius: 6px; cursor: help;",
                tooltip(
                  tags$div(style = "font-size: 0.65rem; color: #6c757d; text-transform: uppercase; letter-spacing: 0.05em;",
                           textOutput(ns("label_stat_min"), inline = TRUE)),
                  textOutput(ns("tooltip_stat_min"), inline = TRUE)
                ),
                tags$div(style = "font-size: 1.1rem; font-weight: 700; color: #212529;", textOutput(ns("stat_min"), inline = TRUE))
              )
            ),
            # Maximum
            div(
              class = "col",
              div(
                style = "text-align: center; padding: 0.5rem; background: #f8f9fa; border-radius: 6px; cursor: help;",
                tooltip(
                  tags$div(style = "font-size: 0.65rem; color: #6c757d; text-transform: uppercase; letter-spacing: 0.05em;",
                           textOutput(ns("label_stat_max"), inline = TRUE)),
                  textOutput(ns("tooltip_stat_max"), inline = TRUE)
                ),
                tags$div(style = "font-size: 1.1rem; font-weight: 700; color: #212529;", textOutput(ns("stat_max"), inline = TRUE))
              )
            ),
            # Mean
            div(
              class = "col",
              div(
                style = "text-align: center; padding: 0.5rem; background: #f8f9fa; border-radius: 6px; cursor: help;",
                tooltip(
                  tags$div(style = "font-size: 0.65rem; color: #6c757d; text-transform: uppercase; letter-spacing: 0.05em;",
                           textOutput(ns("label_stat_mean"), inline = TRUE)),
                  textOutput(ns("tooltip_stat_mean"), inline = TRUE)
                ),
                tags$div(style = "font-size: 1.1rem; font-weight: 700; color: #212529;", textOutput(ns("stat_mean"), inline = TRUE))
              )
            ),
            # Latest
            div(
              class = "col",
              div(
                style = "text-align: center; padding: 0.5rem; background: #e3f2fd; border-radius: 6px; cursor: help;",
                tooltip(
                  tags$div(style = "font-size: 0.65rem; color: #1565c0; text-transform: uppercase; letter-spacing: 0.05em;",
                           textOutput(ns("label_stat_latest"), inline = TRUE)),
                  textOutput(ns("tooltip_stat_latest"), inline = TRUE)
                ),
                tags$div(style = "font-size: 1.1rem; font-weight: 700; color: #1565c0;", textOutput(ns("stat_latest"), inline = TRUE))
              )
            ),
            # YoY Change
            div(
              class = "col",
              div(
                style = "text-align: center; padding: 0.5rem; background: #f8f9fa; border-radius: 6px; cursor: help;",
                tooltip(
                  tags$div(style = "font-size: 0.65rem; color: #6c757d; text-transform: uppercase; letter-spacing: 0.05em;",
                           textOutput(ns("label_stat_yoy"), inline = TRUE)),
                  textOutput(ns("tooltip_stat_yoy"), inline = TRUE)
                ),
                tags$div(style = "font-size: 1.1rem; font-weight: 700; color: #212529;", textOutput(ns("stat_yoy"), inline = TRUE))
              )
            )
          )
        )
      ),

      # Data table
      div(
        class = "card",
        style = "border: none; box-shadow: 0 1px 3px rgba(0,0,0,0.08);",
        div(
          class = "card-header bg-white d-flex justify-content-between align-items-center",
          style = "padding: 0.5rem 1rem; border-bottom: 1px solid #eee;",
          tags$span(style = "font-weight: 600; font-size: 0.85rem;",
                    textOutput(ns("label_data_table"), inline = TRUE)),
          uiOutput(ns("show_table_ui"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s']", ns("show_table")),
          div(
            class = "card-body",
            style = "padding: 0.75rem;",
            DTOutput(ns("data_table"))
          )
        )
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

seriesExplorerServer <- function(id, shared_data, lang = reactive("pt")) {
  moduleServer(id, function(input, output, session) {

    # --------------------------------------------------------------------------
    # Session-level cache for refresh timing and warnings
    # --------------------------------------------------------------------------

    session_cache <- reactiveValues(
      last_refresh_attempt = NULL,
      deseason_warning_shown = FALSE
    )

    # Minimum time between refreshes (5 minutes)
    REFRESH_COOLDOWN_SECONDS <- 300

    # Helper to get current language
    get_lang <- reactive({ lang() })

    # --------------------------------------------------------------------------
    # i18n Label Outputs (sidebar)
    # --------------------------------------------------------------------------

    output$label_last_updated <- renderText({ i18n("messages.last_updated", get_lang()) })
    output$label_theme <- renderText({ toupper(i18n("controls.theme", get_lang())) })
    output$label_category <- renderText({ toupper(i18n("controls.category", get_lang())) })
    output$label_subcategory <- renderText({ toupper(i18n("controls.subcategory", get_lang())) })
    output$label_series <- renderText({ toupper(i18n("controls.series", get_lang())) })
    output$label_date_range <- renderText({ toupper(i18n("controls.date_range", get_lang())) })
    output$label_display <- renderText({ toupper("DISPLAY") })  # Keep simple
    output$label_show_quarterly <- renderText({ i18n("controls.show_quarterly", get_lang()) })
    output$label_show_difference <- renderText({ i18n("controls.show_difference", get_lang()) })
    output$label_seasonal_adjustment <- renderText({ toupper(i18n("controls.deseasonalization", get_lang())) })
    output$label_export <- renderText({ toupper("EXPORT") })  # Keep simple

    # i18n Label Outputs (main panel)
    output$label_summary_stats <- renderText({
      if (get_lang() == "en") "Summary Statistics" else "Estatísticas Resumidas"
    })
    output$label_data_table <- renderText({ i18n("data_panel.title", get_lang()) })
    output$label_stat_min <- renderText({ i18n("stats.min", get_lang()) })
    output$label_stat_max <- renderText({ i18n("stats.max", get_lang()) })
    output$label_stat_mean <- renderText({ i18n("stats.mean", get_lang()) })
    output$label_stat_latest <- renderText({ i18n("stats.latest", get_lang()) })
    output$label_stat_yoy <- renderText({ i18n("stats.yoy_change", get_lang()) })

    # Tooltip outputs
    output$tooltip_stat_min <- renderText({ i18n("tooltips.min", get_lang()) })
    output$tooltip_stat_max <- renderText({ i18n("tooltips.max", get_lang()) })
    output$tooltip_stat_mean <- renderText({ i18n("tooltips.mean", get_lang()) })
    output$tooltip_stat_latest <- renderText({ i18n("tooltips.latest", get_lang()) })
    output$tooltip_stat_yoy <- renderText({ i18n("tooltips.yoy_change", get_lang()) })

    # --------------------------------------------------------------------------
    # Dynamic UI: De-seasonalization radio buttons (for i18n)
    # --------------------------------------------------------------------------

    output$deseasonalize_ui <- renderUI({
      lang_val <- get_lang()
      choices <- c(
        setNames("none", i18n("controls.none", lang_val)),
        setNames("x13", i18n("controls.x13_arima", lang_val)),
        setNames("stl", i18n("controls.stl", lang_val)),
        setNames("both", i18n("controls.compare_both", lang_val))
      )
      radioButtons(
        session$ns("deseasonalize"),
        label = NULL,
        choices = choices,
        selected = isolate(input$deseasonalize) %||% "none",
        inline = FALSE
      )
    })

    # --------------------------------------------------------------------------
    # Dynamic UI: Show table checkbox (for i18n)
    # --------------------------------------------------------------------------

    output$show_table_ui <- renderUI({
      lang_val <- get_lang()
      label_text <- if (lang_val == "en") "Show" else "Ver"
      checkboxInput(session$ns("show_table"), label_text, value = isolate(input$show_table) %||% FALSE, width = "auto")
    })

    # --------------------------------------------------------------------------
    # Dynamic UI: Subcategory dropdown (only shown when subcategories exist)
    # --------------------------------------------------------------------------

    output$subcategory_ui <- renderUI({
      req(input$theme, input$theme_category)
      metadata <- shared_data$series_metadata
      req(metadata)

      subcat_choices <- get_subcategory_choices(
        metadata, input$theme, input$theme_category, get_lang(), include_all = TRUE
      )

      # Only show if there are actual subcategories (not just "All")
      if (length(subcat_choices) > 1) {
        selectInput(
          session$ns("subcategory"),
          label = tags$span(style = "font-size: 0.75rem; font-weight: 600;",
                            toupper(i18n("controls.subcategory", get_lang()))),
          choices = subcat_choices,
          selected = ""  # Default to "All"
        )
      } else {
        NULL
      }
    })

    # --------------------------------------------------------------------------
    # Reactive: Update theme choices
    # --------------------------------------------------------------------------

    observe({
      metadata <- shared_data$series_metadata
      if (!is.null(metadata)) {
        themes <- get_theme_choices(metadata, get_lang())
        updateSelectInput(
          session, "theme",
          choices = themes,
          selected = if ("labor_market" %in% themes) "labor_market" else themes[1]
        )
      }
    })

    # --------------------------------------------------------------------------
    # Reactive: Check if metadata uses new hierarchical structure
    # --------------------------------------------------------------------------

    uses_new_structure <- reactive({
      metadata <- shared_data$series_metadata
      if (is.null(metadata)) return(FALSE)
      "theme" %in% names(metadata) && "theme_category" %in% names(metadata)
    })

    # --------------------------------------------------------------------------
    # Reactive: Update theme_category choices based on theme
    # --------------------------------------------------------------------------

    observeEvent(input$theme, {
      metadata <- shared_data$series_metadata
      req(metadata, input$theme)

      category_choices <- get_theme_category_choices(metadata, input$theme, get_lang())

      # Default selection based on theme
      default_cat <- if (length(category_choices) > 0) {
        if (input$theme == "labor_market" && "unemployment" %in% category_choices) {
          "unemployment"
        } else {
          category_choices[1]
        }
      } else {
        NULL
      }

      updateSelectInput(
        session, "theme_category",
        choices = category_choices,
        selected = default_cat
      )

      # For NEW structure: explicitly clear series dropdown when theme changes
      # The series will be repopulated by the theme_category observer
      if (uses_new_structure() && length(category_choices) > 0) {
        updateSelectInput(
          session, "series",
          choices = character(0),
          selected = NULL
        )
      }

      # If using old structure (no categories), trigger series update directly
      if (!uses_new_structure() && length(category_choices) == 0) {
        # Old structure: theme is actually category, update series directly
        series_choices <- get_series_choices(metadata, input$theme, NULL, NULL, get_lang())

        # Filter to only include series that exist in the monthly_sidra data
        if (!is.null(shared_data$monthly_sidra) && length(series_choices) > 0) {
          dt_cols <- names(shared_data$monthly_sidra)
          available_series <- vapply(series_choices, function(s) {
            paste0("m_", s) %in% dt_cols || s %in% dt_cols
          }, FUN.VALUE = logical(1))
          series_choices <- series_choices[available_series]
        }

        default_series <- if (length(series_choices) > 0) {
          if (input$theme == "rate" && "taxadesocup" %in% series_choices) {
            "taxadesocup"
          } else {
            series_choices[1]
          }
        } else {
          NULL
        }

        updateSelectInput(
          session, "series",
          choices = series_choices,
          selected = default_series
        )
      }
    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # Reactive: Update series choices based on hierarchy (new structure only)
    # --------------------------------------------------------------------------

    observeEvent(list(input$theme, input$theme_category, input$subcategory), {
      # Only run if using new hierarchical structure
      req(uses_new_structure())

      metadata <- shared_data$series_metadata
      req(metadata, input$theme, input$theme_category)

      # Get subcategory (may be NULL or "")
      subcat <- input$subcategory

      series_choices <- get_series_choices(
        metadata, input$theme, input$theme_category, subcat, get_lang()
      )

      # Filter to only include series that exist in the monthly_sidra data
      if (!is.null(shared_data$monthly_sidra) && length(series_choices) > 0) {
        dt_cols <- names(shared_data$monthly_sidra)
        available_series <- vapply(series_choices, function(s) {
          paste0("m_", s) %in% dt_cols || s %in% dt_cols
        }, FUN.VALUE = logical(1))
        series_choices <- series_choices[available_series]
      }

      # Always select the first available series
      default_series <- if (length(series_choices) > 0) {
        # For unemployment category, prefer unemployment rate
        if (input$theme_category == "unemployment" && "taxadesocup" %in% series_choices) {
          "taxadesocup"
        } else {
          series_choices[1]
        }
      } else {
        NULL
      }

      updateSelectInput(
        session, "series",
        choices = series_choices,
        selected = default_series
      )
    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # Reset date range and seasonal adjustment when series changes
    # --------------------------------------------------------------------------

    observeEvent(input$series, {
      req(shared_data$monthly_sidra)
      dt <- shared_data$monthly_sidra

      # Get date range from data
      dates <- yyyymm_to_date(dt$anomesexato)
      min_date <- min(dates, na.rm = TRUE)
      max_date <- max(dates, na.rm = TRUE)

      updateSliderInput(
        session, "date_range",
        min = min_date,
        max = max_date,
        value = c(min_date, max_date)
      )

      # Reset seasonal adjustment to None
      updateRadioButtons(session, "deseasonalize", selected = "none")

      # Reset the warning flag so notification can show again for new series
      session_cache$deseason_warning_shown <- FALSE
    }, ignoreInit = TRUE)

    # --------------------------------------------------------------------------
    # Reactive: Filtered data
    # --------------------------------------------------------------------------

    filtered_data <- reactive({
      req(input$series)
      req(shared_data$monthly_sidra)

      dt <- shared_data$monthly_sidra

      # Get the column for the selected series
      monthly_col <- paste0("m_", input$series)

      if (!monthly_col %in% names(dt)) {
        # Try without prefix
        monthly_col <- input$series
        if (!monthly_col %in% names(dt)) {
          return(NULL)
        }
      }

      # Create date from anomesexato
      result <- data.table(
        anomesexato = dt$anomesexato,
        date = yyyymm_to_date(dt$anomesexato),
        monthly = dt[[monthly_col]]
      )

      # Add quarterly data if available
      # Note: Rolling quarters align with the 2nd month (middle), not the 3rd (final)
      # So we shift anomesfinaltrimmovel back by 1 month
      if (!is.null(shared_data$rolling_quarters)) {
        rq <- shared_data$rolling_quarters
        quarterly_col <- input$series
        if (quarterly_col %in% names(rq)) {
          # Shift the rolling quarter final month back by 1 to align with middle month
          rq_yyyymm <- as.integer(rq$anomesfinaltrimmovel)
          rq_year <- rq_yyyymm %/% 100
          rq_month <- rq_yyyymm %% 100
          # Shift back 1 month
          rq_month_shifted <- rq_month - 1L
          rq_year_shifted <- rq_year
          # Handle January -> December of previous year
          rq_year_shifted[rq_month_shifted == 0L] <- rq_year[rq_month_shifted == 0L] - 1L
          rq_month_shifted[rq_month_shifted == 0L] <- 12L
          # Create shifted anomesexato
          rq_anomesexato_shifted <- rq_year_shifted * 100L + rq_month_shifted

          rq_subset <- data.table(
            anomesexato = rq_anomesexato_shifted,
            quarterly = rq[[quarterly_col]]
          )
          result <- merge(result, rq_subset, by = "anomesexato", all.x = TRUE)
        }
      }

      # Filter by date range
      result <- result[date >= input$date_range[1] & date <= input$date_range[2]]

      # Calculate difference (before de-seasonalization)
      if ("quarterly" %in% names(result)) {
        result[, difference := monthly - quarterly]
      }

      # Always include BOTH X-13 and STL for downloads (regardless of display selection)
      if (nrow(result) > 0) {

        # Check package availability
        deseason_available <- check_deseasonalization_available()

        # Check if we have precomputed values in cache
        cache <- shared_data$deseasonalized_cache
        series_name <- input$series
        cached_data <- if (!is.null(cache) && series_name %in% names(cache)) {
          cache[[series_name]]
        } else {
          NULL
        }

        # Always compute BOTH X-13 and STL for downloads
        # Try to use cached values first
        if (!is.null(cached_data) && "x13" %in% names(cached_data)) {
          result[, monthly_x13 := cached_data$x13[match(anomesexato,
                                  shared_data$monthly_sidra$anomesexato)]]
        } else if (deseason_available["x13"]) {
          result[, monthly_x13 := deseasonalize_x13(monthly, date)]
        }

        if (!is.null(cached_data) && "stl" %in% names(cached_data)) {
          result[, monthly_stl := cached_data$stl[match(anomesexato,
                                  shared_data$monthly_sidra$anomesexato)]]
        } else if (deseason_available["stl"]) {
          result[, monthly_stl := deseasonalize_stl(monthly, date)]
        }
      }

      result
    })

    # --------------------------------------------------------------------------
    # Output: Last updated timestamp
    # --------------------------------------------------------------------------

    output$last_updated <- renderText({
      if (!is.null(shared_data$last_updated)) {
        format(shared_data$last_updated, "%Y-%m-%d %H:%M")
      } else {
        "Not available"
      }
    })

    # --------------------------------------------------------------------------
    # Output: Plot title (uses i18n for series description)
    # --------------------------------------------------------------------------

    output$plot_title <- renderText({
      req(input$series)
      metadata <- shared_data$series_metadata
      get_series_description(metadata, input$series, get_lang())
    })

    # --------------------------------------------------------------------------
    # Output: Series description (popover info)
    # --------------------------------------------------------------------------

    output$series_description <- renderText({
      req(input$series)
      metadata <- shared_data$series_metadata
      lang_val <- get_lang()

      if (!is.null(metadata)) {
        row <- metadata[metadata[["series_name"]] == input$series, ]
        if (nrow(row) > 0) {
          # Check if using new hierarchical structure
          if ("theme" %in% names(row) && "theme_category" %in% names(row)) {
            theme_label <- get_theme_label(row[["theme"]][1], lang_val)
            cat_label <- get_theme_category_label(row[["theme_category"]][1], lang_val)
            unit_val <- row[["unit"]][1]

            if (lang_val == "en") {
              paste0(
                "Theme: ", theme_label, "\n",
                "Category: ", cat_label, "\n",
                "SIDRA Table: ", row[["table_id"]][1], "\n",
                "Unit: ", unit_val
              )
            } else {
              paste0(
                "Tema: ", theme_label, "\n",
                "Categoria: ", cat_label, "\n",
                "Tabela SIDRA: ", row[["table_id"]][1], "\n",
                "Unidade: ", unit_val
              )
            }
          } else {
            # Old structure
            cat_val <- if ("category" %in% names(row)) row[["category"]][1] else "N/A"
            unit_val <- if ("unit" %in% names(row)) row[["unit"]][1] else "N/A"

            if (lang_val == "en") {
              paste0(
                "Category: ", cat_val, "\n",
                "SIDRA Table: ", row[["table_id"]][1], "\n",
                "Unit: ", unit_val
              )
            } else {
              paste0(
                "Categoria: ", cat_val, "\n",
                "Tabela SIDRA: ", row[["table_id"]][1], "\n",
                "Unidade: ", unit_val
              )
            }
          }
        } else {
          if (lang_val == "en") "No description available" else "Descrição não disponível"
        }
      } else {
        if (lang_val == "en") "No description available" else "Descrição não disponível"
      }
    })

    # --------------------------------------------------------------------------
    # Output: Main plot
    # --------------------------------------------------------------------------

    output$main_plot <- renderPlotly({
      dt <- filtered_data()

      # Validate data
      validate(
        need(!is.null(dt), "Loading data..."),
        need(nrow(dt) > 0, "No data available for the selected series and date range")
      )

      # Build plot with plotly directly for better control
      p <- plot_ly(dt, x = ~date)

      deseason_method <- input$deseasonalize

      # Handle de-seasonalization display
      if (!is.null(deseason_method) && deseason_method == "both") {
        # Compare both methods: show original as faded background
        p <- p %>% add_lines(
          y = ~monthly,
          name = "Monthly Original",
          line = list(color = ibge_colors$gray, width = 1.5),
          opacity = 0.7
        )
        p <- p %>% add_lines(
          y = ~monthly_x13,
          name = "Monthly Adjusted (X-13 ARIMA)",
          line = list(color = ibge_colors$primary, width = 2)
        )
        p <- p %>% add_lines(
          y = ~monthly_stl,
          name = "Monthly Adjusted (STL)",
          line = list(color = "#4CAF50", width = 2)
        )
      } else if (!is.null(deseason_method) && deseason_method == "x13") {
        # X-13 ARIMA method: show both original and adjusted
        p <- p %>% add_lines(
          y = ~monthly,
          name = "Monthly Original",
          line = list(color = ibge_colors$gray, width = 1.5, dash = "dot"),
          opacity = 0.7
        )
        p <- p %>% add_lines(
          y = ~monthly_x13,
          name = "Monthly Adjusted (X-13 ARIMA)",
          line = list(color = ibge_colors$secondary, width = 2)
        )
      } else if (!is.null(deseason_method) && deseason_method == "stl") {
        # STL method: show both original and adjusted
        p <- p %>% add_lines(
          y = ~monthly,
          name = "Monthly Original",
          line = list(color = ibge_colors$gray, width = 1.5, dash = "dot"),
          opacity = 0.7
        )
        p <- p %>% add_lines(
          y = ~monthly_stl,
          name = "Monthly Adjusted (STL)",
          line = list(color = ibge_colors$secondary, width = 2)
        )
      } else {
        # No de-seasonalization: show monthly series normally
        p <- p %>% add_lines(
          y = ~monthly,
          name = "Monthly",
          line = list(color = ibge_colors$secondary, width = 2)
        )
      }

      # Add quarterly overlay if requested and available (only when not comparing de-seasonalization)
      if (input$show_quarterly && "quarterly" %in% names(dt) &&
          (is.null(deseason_method) || deseason_method == "none")) {
        p <- p %>% add_lines(
          y = ~quarterly,
          name = "Quarterly",
          line = list(color = ibge_colors$gray, width = 2, dash = "dash")
        )
      }

      # Configure layout
      p %>% layout(
        xaxis = list(title = "", tickfont = list(size = 10)),
        yaxis = list(title = "", tickfont = list(size = 10)),
        hovermode = "x unified",
        legend = list(orientation = "h", y = 1.08, x = 0.5, xanchor = "center", font = list(size = 10)),
        margin = list(t = 30, b = 40, l = 50, r = 20),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      ) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE,
               modeBarButtonsToRemove = c("lasso2d", "select2d"))
    })

    # --------------------------------------------------------------------------
    # Output: Difference plot title (dynamic based on seasonal adjustment)
    # --------------------------------------------------------------------------

    output$difference_plot_title <- renderText({
      deseason_method <- input$deseasonalize
      lang_val <- get_lang()

      if (is.null(deseason_method) || deseason_method == "none") {
        if (lang_val == "en") "Difference: Monthly - Quarterly" else "Diferença: Mensal - Trimestral"
      } else if (deseason_method == "x13") {
        if (lang_val == "en") "Seasonal Component (X-13 ARIMA)" else "Componente Sazonal (X-13 ARIMA)"
      } else if (deseason_method == "stl") {
        if (lang_val == "en") "Seasonal Component (STL)" else "Componente Sazonal (STL)"
      } else if (deseason_method == "both") {
        if (lang_val == "en") "Seasonal Components Comparison" else "Comparação de Componentes Sazonais"
      } else {
        if (lang_val == "en") "Difference" else "Diferença"
      }
    })

    # --------------------------------------------------------------------------
    # Output: Difference plot
    # --------------------------------------------------------------------------

    output$difference_plot <- renderPlotly({
      req(input$show_difference)
      dt <- filtered_data()

      validate(need(!is.null(dt) && nrow(dt) > 0, "No data available"))

      deseason_method <- input$deseasonalize

      # Determine what to plot based on seasonal adjustment selection
      if (is.null(deseason_method) || deseason_method == "none") {
        # Show Monthly - Quarterly difference
        validate(
          need("difference" %in% names(dt), "Quarterly data not available for this series"),
          need(!all(is.na(dt$difference)), "No difference data to display")
        )

        dt_plot <- dt[!is.na(difference)]
        validate(need(nrow(dt_plot) > 0, "No difference data in selected date range"))

        p <- plot_ly(dt_plot, x = ~date) %>%
          add_lines(
            y = ~difference,
            name = "Monthly - Quarterly",
            line = list(color = ibge_colors$primary, width = 1.5),
            fill = "tozeroy",
            fillcolor = "rgba(25, 118, 210, 0.15)"
          )

      } else if (deseason_method == "x13") {
        # Show X-13 seasonal component (Original - Adjusted)
        validate(
          need("monthly_x13" %in% names(dt), "X-13 adjustment not available"),
          need(!all(is.na(dt$monthly_x13)), "No X-13 adjusted data available")
        )

        dt_plot <- dt[!is.na(monthly_x13)]
        dt_plot[, seasonal_x13 := monthly - monthly_x13]

        p <- plot_ly(dt_plot, x = ~date) %>%
          add_lines(
            y = ~seasonal_x13,
            name = "Seasonal (X-13)",
            line = list(color = ibge_colors$primary, width = 1.5),
            fill = "tozeroy",
            fillcolor = "rgba(25, 118, 210, 0.15)"
          )

      } else if (deseason_method == "stl") {
        # Show STL seasonal component (Original - Adjusted)
        validate(
          need("monthly_stl" %in% names(dt), "STL adjustment not available"),
          need(!all(is.na(dt$monthly_stl)), "No STL adjusted data available")
        )

        dt_plot <- dt[!is.na(monthly_stl)]
        dt_plot[, seasonal_stl := monthly - monthly_stl]

        p <- plot_ly(dt_plot, x = ~date) %>%
          add_lines(
            y = ~seasonal_stl,
            name = "Seasonal (STL)",
            line = list(color = "#4CAF50", width = 1.5),
            fill = "tozeroy",
            fillcolor = "rgba(76, 175, 80, 0.15)"
          )

      } else if (deseason_method == "both") {
        # Show both seasonal components
        validate(
          need("monthly_x13" %in% names(dt) || "monthly_stl" %in% names(dt),
               "No seasonal adjustment data available")
        )

        dt_plot <- copy(dt)
        if ("monthly_x13" %in% names(dt_plot)) {
          dt_plot[, seasonal_x13 := monthly - monthly_x13]
        }
        if ("monthly_stl" %in% names(dt_plot)) {
          dt_plot[, seasonal_stl := monthly - monthly_stl]
        }

        p <- plot_ly(dt_plot, x = ~date)

        if ("seasonal_x13" %in% names(dt_plot)) {
          p <- p %>% add_lines(
            y = ~seasonal_x13,
            name = "X-13 ARIMA",
            line = list(color = ibge_colors$primary, width = 1.5)
          )
        }
        if ("seasonal_stl" %in% names(dt_plot)) {
          p <- p %>% add_lines(
            y = ~seasonal_stl,
            name = "STL",
            line = list(color = "#4CAF50", width = 1.5)
          )
        }
      }

      # Common layout
      p %>% layout(
        xaxis = list(title = "", tickfont = list(size = 10)),
        yaxis = list(title = "", tickfont = list(size = 10)),
        hovermode = "x unified",
        showlegend = (deseason_method == "both"),
        legend = list(orientation = "h", y = 1.1, x = 0.5, xanchor = "center", font = list(size = 9)),
        margin = list(t = 10, b = 30, l = 50, r = 20),
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        shapes = list(
          list(type = "line", x0 = min(dt_plot$date), x1 = max(dt_plot$date),
               y0 = 0, y1 = 0, line = list(color = ibge_colors$gray, dash = "dash", width = 1))
        )
      ) %>%
        config(displayModeBar = FALSE)
    })

    # --------------------------------------------------------------------------
    # Output: Summary statistics
    # --------------------------------------------------------------------------

    output$stat_min <- renderText({
      req(filtered_data())
      dt <- filtered_data()
      format_br(min(dt$monthly, na.rm = TRUE))
    })

    output$stat_max <- renderText({
      req(filtered_data())
      dt <- filtered_data()
      format_br(max(dt$monthly, na.rm = TRUE))
    })

    output$stat_mean <- renderText({
      req(filtered_data())
      dt <- filtered_data()
      format_br(mean(dt$monthly, na.rm = TRUE))
    })

    output$stat_latest <- renderText({
      req(filtered_data())
      dt <- filtered_data()
      latest <- dt[which.max(date), monthly]
      format_br(latest)
    })

    output$stat_yoy <- renderText({
      req(filtered_data())
      dt <- filtered_data()

      # Get latest and value from 12 months ago
      dt <- dt[order(date)]
      latest_date <- max(dt$date, na.rm = TRUE)
      yoy_date <- latest_date - 365

      latest_val <- dt[date == latest_date, monthly][1]

      # Find the closest date to 12 months ago
      dt_past <- dt[date <= yoy_date]
      if (nrow(dt_past) == 0) {
        return("N/A")
      }
      yoy_val <- dt_past[which.max(date), monthly]

      if (length(yoy_val) > 0 && !is.na(yoy_val) && !is.na(latest_val) && yoy_val != 0) {
        change <- (latest_val - yoy_val) / yoy_val * 100
        paste0(ifelse(change > 0, "+", ""), format_br(change), "%")
      } else {
        "N/A"
      }
    })

    # --------------------------------------------------------------------------
    # Output: Data table
    # --------------------------------------------------------------------------

    output$data_table <- renderDT({
      req(filtered_data())
      dt <- filtered_data()
      lang_val <- get_lang()

      # Select columns to display
      cols <- c("date", "monthly")
      if ("quarterly" %in% names(dt)) cols <- c(cols, "quarterly", "difference")

      # Add seasonal adjusted columns if available (always show both X-13 and STL)
      if ("monthly_x13" %in% names(dt)) cols <- c(cols, "monthly_x13")
      if ("monthly_stl" %in% names(dt)) cols <- c(cols, "monthly_stl")

      dt_display <- dt[, ..cols]

      # i18n column names
      setnames(dt_display, "date", i18n("data_panel.date", lang_val))
      setnames(dt_display, "monthly", i18n("data_panel.monthly", lang_val))
      if ("quarterly" %in% names(dt_display)) {
        setnames(dt_display, "quarterly", i18n("data_panel.quarterly", lang_val))
        setnames(dt_display, "difference", i18n("data_panel.difference", lang_val))
      }
      if ("monthly_x13" %in% names(dt_display)) {
        setnames(dt_display, "monthly_x13", i18n("data_panel.adjusted_x13", lang_val))
      }
      if ("monthly_stl" %in% names(dt_display)) {
        setnames(dt_display, "monthly_stl", i18n("data_panel.adjusted_stl", lang_val))
      }

      # i18n table language options
      search_label <- if (lang_val == "en") "Filter:" else "Filtrar:"

      datatable(
        dt_display,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip',
          language = list(search = search_label)
        ),
        rownames = FALSE,
        style = "bootstrap4",
        class = "table-sm table-striped"
      ) %>%
        formatRound(columns = 2:ncol(dt_display), digits = 2)
    })

    # --------------------------------------------------------------------------
    # Download: CSV
    # --------------------------------------------------------------------------

    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("pnadcperiods_", input$series, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        dt <- filtered_data()
        fwrite(dt, file)
      }
    )

    # --------------------------------------------------------------------------
    # Download: PNG
    # --------------------------------------------------------------------------

    # Helper to get plot title (uses i18n)
    get_plot_title <- function() {
      metadata <- shared_data$series_metadata
      get_series_description(metadata, input$series, get_lang())
    }

    output$download_png <- downloadHandler(
      filename = function() {
        paste0("pnadcperiods_", input$series, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(filtered_data())
        dt <- filtered_data()
        lang_val <- get_lang()

        # i18n caption
        caption_text <- if (lang_val == "en") {
          paste("Source: PNADCperiods | Generated:", Sys.Date())
        } else {
          paste("Fonte: PNADCperiods | Gerado em:", Sys.Date())
        }

        p <- ggplot(dt, aes(x = date)) +
          geom_line(aes(y = monthly), color = ibge_colors$secondary, linewidth = 1) +
          labs(
            title = get_plot_title(),
            x = NULL, y = NULL,
            caption = caption_text
          ) +
          theme_pnadc()

        if (input$show_quarterly && "quarterly" %in% names(dt)) {
          p <- p +
            geom_line(aes(y = quarterly), color = ibge_colors$gray,
                      linewidth = 1, linetype = "dashed")
        }

        ggsave(file, plot = p, width = 10, height = 6, dpi = 150, bg = "white")
      },
      contentType = "image/png"
    )

    # --------------------------------------------------------------------------
    # Download: Excel
    # --------------------------------------------------------------------------

    output$download_xlsx <- downloadHandler(
      filename = function() {
        paste0("pnadcperiods_", input$series, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        dt <- filtered_data()
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(as.data.frame(dt), file)
        } else {
          # Fallback to CSV if openxlsx not available
          fwrite(dt, file)
        }
      }
    )

    # --------------------------------------------------------------------------
    # Observer: Notify user when de-seasonalization packages unavailable
    # --------------------------------------------------------------------------

    observeEvent(input$deseasonalize, {
      if (input$deseasonalize != "none" && !session_cache$deseason_warning_shown) {
        deseason_available <- check_deseasonalization_available()

        # Check which packages are missing
        missing_packages <- c()
        if (input$deseasonalize %in% c("x13", "both") && !deseason_available["x13"]) {
          missing_packages <- c(missing_packages, "seasonal")
        }
        if (input$deseasonalize %in% c("stl", "both") && !deseason_available["stl"]) {
          missing_packages <- c(missing_packages, "forecast")
        }

        if (length(missing_packages) > 0) {
          showNotification(
            paste0(
              "Seasonal adjustment unavailable. Install package(s): ",
              paste(missing_packages, collapse = ", "),
              "\n\nRun: install.packages(c('", paste(missing_packages, collapse = "', '"), "'))"
            ),
            type = "warning",
            duration = 10
          )
          session_cache$deseason_warning_shown <- TRUE
        }
      }
    }, ignoreInit = TRUE)

    # --------------------------------------------------------------------------
    # Event: Refresh SIDRA data
    # --------------------------------------------------------------------------

    observeEvent(input$refresh_sidra, {
      # Session-level caching: check if we refreshed recently
      if (!is.null(session_cache$last_refresh_attempt)) {
        time_since_refresh <- difftime(Sys.time(), session_cache$last_refresh_attempt,
                                        units = "secs")
        if (time_since_refresh < REFRESH_COOLDOWN_SECONDS) {
          remaining <- round(REFRESH_COOLDOWN_SECONDS - as.numeric(time_since_refresh))
          showNotification(
            paste("Data was recently refreshed. Please wait", remaining, "seconds."),
            type = "warning"
          )
          return()
        }
      }

      # Disable button during fetch (if shinyjs available)
      if (exists("shinyjs_available") && shinyjs_available) {
        shinyjs::disable("refresh_sidra")
      }

      # Mark refresh attempt time
      session_cache$last_refresh_attempt <- Sys.time()

      withProgress(message = "Checking for new data...", value = 0, {
        tryCatch({
          incProgress(0.1, detail = "Querying SIDRA API...")

          if (!requireNamespace("PNADCperiods", quietly = TRUE)) {
            showNotification("PNADCperiods package not available", type = "error")
            return()
          }

          # First, check if there's actually new data available
          # Get current latest date from our data
          current_max_date <- NULL
          if (!is.null(shared_data$rolling_quarters)) {
            current_max_date <- max(shared_data$rolling_quarters$anomesfinaltrimmovel, na.rm = TRUE)
          }

          # Fetch new rolling quarters data
          incProgress(0.2, detail = "Fetching rolling quarters...")
          new_rq <- PNADCperiods::fetch_sidra_rolling_quarters(verbose = FALSE)

          # Check if there's new data
          new_max_date <- max(new_rq$anomesfinaltrimmovel, na.rm = TRUE)

          if (!is.null(current_max_date) && new_max_date <= current_max_date) {
            # No new data available
            showNotification(
              paste0("No new data available. Latest data point: ",
                     substr(current_max_date, 1, 4), "-",
                     substr(current_max_date, 5, 6)),
              type = "message",
              duration = 5
            )
            # Re-enable button
            if (exists("shinyjs_available") && shinyjs_available) {
              shinyjs::enable("refresh_sidra")
            }
            return()
          }

          # New data found - proceed with full refresh
          n_new_periods <- if (!is.null(current_max_date)) {
            sum(new_rq$anomesfinaltrimmovel > current_max_date)
          } else {
            nrow(new_rq)
          }

          incProgress(0.4, detail = paste("Found", n_new_periods, "new periods. Mensalizing..."))
          new_monthly <- PNADCperiods::mensalize_sidra_series(new_rq, verbose = FALSE)

          # Update shared data
          shared_data$rolling_quarters <- new_rq
          shared_data$monthly_sidra <- new_monthly
          shared_data$last_updated <- Sys.time()

          # Save to disk
          saveRDS(new_rq, "data/rolling_quarters.rds")
          saveRDS(new_monthly, "data/monthly_sidra.rds")

          # Recompute de-seasonalized series for top series
          incProgress(0.7, detail = "Updating seasonal adjustments...")

          deseason_available <- check_deseasonalization_available()
          if (deseason_available["x13"] || deseason_available["stl"]) {

            # Get dates from monthly data
            dates <- as.Date(paste0(substr(new_monthly$anomesexato, 1, 4), "-",
                                    substr(new_monthly$anomesexato, 5, 6), "-15"))

            # Top series to precompute (same as precompute script)
            top_series <- c(
              "taxadesocup", "taxapartic", "nivelocup", "niveldesocup",
              "taxacombdesosub", "taxacompsubutlz", "percdesalento", "perccontribprev",
              "populacao", "pop14mais", "popnaforca", "popocup", "popdesocup", "popforadaforca",
              "rendhabnominaltodos", "rendhabrealtodos", "rendefetnominaltodos", "rendefetrealtodos",
              "empregado", "contapropria", "empregador",
              "agropecuaria", "industria", "comercio", "construcao"
            )

            new_cache <- list()
            for (series_name in top_series) {
              monthly_col <- paste0("m_", series_name)
              if (!monthly_col %in% names(new_monthly)) {
                monthly_col <- series_name
              }

              if (monthly_col %in% names(new_monthly)) {
                values <- new_monthly[[monthly_col]]

                tryCatch({
                  result <- list(
                    series_name = series_name,
                    original = values
                  )

                  if (deseason_available["x13"]) {
                    result$x13 <- deseasonalize_x13(values, dates)
                  }
                  if (deseason_available["stl"]) {
                    result$stl <- deseasonalize_stl(values, dates)
                  }

                  new_cache[[series_name]] <- result
                }, error = function(e) {
                  # Skip series that fail de-seasonalization
                })
              }
            }

            # Update and save de-seasonalized cache
            if (length(new_cache) > 0) {
              shared_data$deseasonalized_cache <- new_cache
              saveRDS(new_cache, "data/deseasonalized_cache.rds")
            }
          }

          incProgress(0.3, detail = "Done!")
          showNotification(
            paste0("Data updated successfully! Added ", n_new_periods, " new periods."),
            type = "message"
          )

        }, error = function(e) {
          showNotification(paste("Error:", e$message), type = "error")
        })
      })

      # Re-enable button after 60 seconds (if shinyjs available)
      if (exists("shinyjs_available") && shinyjs_available) {
        shinyjs::delay(60000, shinyjs::enable("refresh_sidra"))
      }
    })

  })
}
