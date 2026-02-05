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

      # Date range (dynamic UI for proper i18n formatting)
      div(
        style = "padding: 0 5px;",  # Extra padding for slider labels
        uiOutput(ns("date_range_ui"))
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
          div(
            tags$h6(class = "mb-0", style = "font-weight: 600; font-size: 0.9rem;",
                    textOutput(ns("plot_title"), inline = TRUE)),
            tags$small(class = "text-muted",
                       style = "font-size: 0.75rem;",
                       textOutput(ns("plot_subtitle"), inline = TRUE))
          ),
          uiOutput(ns("series_info_popover"))
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
      checkboxInput(session$ns("show_table"), label_text,
                    value = isolate(input$show_table) %||% FALSE, width = "auto")
    })

    # --------------------------------------------------------------------------
    # Dynamic UI: Series info popover (for i18n title)
    # --------------------------------------------------------------------------

    output$series_info_popover <- renderUI({
      lang_val <- get_lang()
      popover(
        icon("info-circle", class = "text-muted",
             style = "font-size: 0.9rem; cursor: pointer;"),
        title = i18n("data_panel.series_info", lang_val),
        textOutput(session$ns("series_description"))
      )
    })

    # --------------------------------------------------------------------------
    # Dynamic UI: Date range selection (year-month dropdowns)
    # --------------------------------------------------------------------------

    # Reactive values to track date range state
    date_range_state <- reactiveValues(
      available_months = NULL,  # Vector of YYYYMM integers
      start_month = NULL,
      end_month = NULL
    )

    # Generate month choices with i18n labels
    generate_month_choices <- function(months_yyyymm, lang) {
      if (is.null(months_yyyymm) || length(months_yyyymm) == 0) {
        return(character(0))
      }

      # Sort months
      months_sorted <- sort(unique(months_yyyymm))

      # Generate labels
      labels <- sapply(months_sorted, function(m) {
        format_date_i18n(m, lang, format = "short")
      })

      setNames(as.character(months_sorted), labels)
    }

    output$date_range_ui <- renderUI({
      lang_val <- get_lang()
      months <- isolate(date_range_state$available_months)

      if (is.null(months) || length(months) == 0) {
        return(NULL)
      }

      choices <- generate_month_choices(months, lang_val)
      start_val <- isolate(date_range_state$start_month)
      end_val <- isolate(date_range_state$end_month)

      # Ensure values are valid
      if (is.null(start_val) || !as.character(start_val) %in% choices) {
        start_val <- names(choices)[1]
      }
      if (is.null(end_val) || !as.character(end_val) %in% choices) {
        end_val <- names(choices)[length(choices)]
      }

      tagList(
        tags$label(
          style = "font-size: 0.75rem; font-weight: 600;",
          toupper(i18n("controls.date_range", lang_val))
        ),
        div(
          class = "d-flex gap-2 align-items-center",
          style = "margin-top: 5px;",
          div(
            style = "flex: 1;",
            selectInput(
              session$ns("date_start"),
              label = NULL,
              choices = choices,
              selected = start_val,
              width = "100%"
            )
          ),
          tags$span("—", style = "color: #6c757d;"),
          div(
            style = "flex: 1;",
            selectInput(
              session$ns("date_end"),
              label = NULL,
              choices = choices,
              selected = end_val,
              width = "100%"
            )
          )
        )
      )
    })

    # Update available months when data changes
    observeEvent(shared_data$monthly_sidra, {
      dt <- shared_data$monthly_sidra
      if (!is.null(dt)) {
        months <- sort(unique(dt$anomesexato))
        date_range_state$available_months <- months
        date_range_state$start_month <- min(months)
        date_range_state$end_month <- max(months)
      }
    }, ignoreNULL = TRUE)

    # Track user changes to preserve across language switches
    observeEvent(input$date_start, {
      if (!is.null(input$date_start)) {
        date_range_state$start_month <- as.integer(input$date_start)
      }
    }, ignoreInit = TRUE)

    observeEvent(input$date_end, {
      if (!is.null(input$date_end)) {
        date_range_state$end_month <- as.integer(input$date_end)
      }
    }, ignoreInit = TRUE)

    # Computed date range (for filtering)
    date_range <- reactive({
      start <- input$date_start
      end <- input$date_end

      if (is.null(start) || is.null(end)) {
        return(NULL)
      }

      # Convert YYYYMM to Date
      start_date <- yyyymm_to_date(as.integer(start))
      end_date <- yyyymm_to_date(as.integer(end))

      c(start_date, end_date)
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
    # Reactive: Update dropdown labels when language changes
    # --------------------------------------------------------------------------

    observeEvent(get_lang(), {
      metadata <- shared_data$series_metadata
      if (is.null(metadata)) return()

      lang_val <- get_lang()

      # Get current selections to preserve them
      current_theme <- isolate(input$theme)
      current_category <- isolate(input$theme_category)
      current_series <- isolate(input$series)

      # Update theme dropdown with new language labels
      themes <- get_theme_choices(metadata, lang_val)
      updateSelectInput(
        session, "theme",
        choices = themes,
        selected = if (!is.null(current_theme) && current_theme %in% themes) {
          current_theme
        } else if ("labor_market" %in% themes) {
          "labor_market"
        } else {
          themes[1]
        }
      )

      # Update category dropdown if theme is selected
      if (!is.null(current_theme) && current_theme != "") {
        category_choices <- get_theme_category_choices(metadata, current_theme, lang_val)
        if (length(category_choices) > 0) {
          updateSelectInput(
            session, "theme_category",
            choices = category_choices,
            selected = if (!is.null(current_category) && current_category %in% category_choices) {
              current_category
            } else {
              category_choices[1]
            }
          )
        }
      }

      # Update series dropdown if category is selected
      if (!is.null(current_theme) && !is.null(current_category) &&
          current_theme != "" && current_category != "") {
        subcat <- isolate(input$subcategory)
        series_choices <- get_series_choices(metadata, current_theme, current_category, subcat, lang_val)

        # Filter to only available series
        if (!is.null(shared_data$monthly_sidra) && length(series_choices) > 0) {
          dt_cols <- names(shared_data$monthly_sidra)
          available_series <- vapply(series_choices, function(s) {
            paste0("m_", s) %in% dt_cols || s %in% dt_cols
          }, FUN.VALUE = logical(1))
          series_choices <- series_choices[available_series]
        }

        if (length(series_choices) > 0) {
          updateSelectInput(
            session, "series",
            choices = series_choices,
            selected = if (!is.null(current_series) && current_series %in% series_choices) {
              current_series
            } else {
              series_choices[1]
            }
          )
        }
      }
    }, ignoreInit = TRUE)

    # --------------------------------------------------------------------------
    # Reactive: Update theme choices (initial load)
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
    }) |> bindEvent(shared_data$series_metadata, once = TRUE)

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

    # NOTE: This observer should NOT observe input$theme directly!
    # When theme changes, the category dropdown is updated first.
    # This observer should only fire when category/subcategory change.
    # Observing theme causes a race condition where this fires before
    # theme_category is updated, leading to wrong series selections.
    observeEvent(list(input$theme_category, input$subcategory), {
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

      # Get month range from data (YYYYMM integers)
      months <- sort(unique(dt$anomesexato))

      # Update the date range state to full range for new series
      date_range_state$available_months <- months
      date_range_state$start_month <- min(months)
      date_range_state$end_month <- max(months)

      # Update the dropdowns directly for immediate feedback
      lang_val <- get_lang()
      choices <- generate_month_choices(months, lang_val)

      updateSelectInput(session, "date_start",
                        choices = choices,
                        selected = as.character(min(months)))
      updateSelectInput(session, "date_end",
                        choices = choices,
                        selected = as.character(max(months)))

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

      # Convert thousands to millions for display
      series_unit <- get_series_unit(shared_data$series_metadata, input$series)
      if (series_unit == "thousands") {
        result[, monthly := monthly / 1000]
      }

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
          # Convert quarterly to millions if data is in thousands
          if (series_unit == "thousands") {
            rq_subset[, quarterly := quarterly / 1000]
          }
          result <- merge(result, rq_subset, by = "anomesexato", all.x = TRUE)
        }
      }

      # Filter by date range (using the computed date_range reactive)
      dr <- date_range()
      if (!is.null(dr) && length(dr) == 2) {
        result <- result[date >= dr[1] & date <= dr[2]]
      }

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
    # Reactive: Plot title text (explicit dependencies for reliable updates)
    # --------------------------------------------------------------------------

    plot_title_text <- reactive({
      req(input$series)
      lang_val <- get_lang()  # Explicit reactive dependency
      metadata <- shared_data$series_metadata
      desc <- get_series_description(metadata, input$series, lang_val)
      # DEBUG: Print to console to verify reactive is firing
      message("[DEBUG plot_title_text] series=", input$series, " -> ", substr(desc, 1, 30))
      desc
    })

    # --------------------------------------------------------------------------
    # Reactive: Plot subtitle text (explicit dependencies)
    # --------------------------------------------------------------------------

    plot_subtitle_text <- reactive({
      req(input$series)
      series_name <- input$series
      lang_val <- get_lang()  # Explicit reactive dependency

      # Check if this is a "real" (deflated) series
      is_real <- grepl("real", series_name, ignore.case = TRUE)

      if (is_real) {
        ref_date <- if (lang_val == "en") "Dec/2024" else "Dez/2024"
        sprintf(i18n("plots.deflation_ref", lang_val), ref_date)
      } else {
        ""
      }
    })

    # --------------------------------------------------------------------------
    # Output: Plot title (uses reactive for proper dependency tracking)
    # --------------------------------------------------------------------------

    output$plot_title <- renderText({
      plot_title_text()
    })

    # --------------------------------------------------------------------------
    # Output: Plot subtitle (uses reactive for proper dependency tracking)
    # --------------------------------------------------------------------------

    output$plot_subtitle <- renderText({
      plot_subtitle_text()
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

      # Get current language for labels
      lang_val <- get_lang()

      # Handle de-seasonalization display
      if (!is.null(deseason_method) && deseason_method == "both") {
        # Compare both methods: show original as faded background
        p <- p %>% add_lines(
          y = ~monthly,
          name = i18n("plots.monthly_original", lang_val),
          line = list(color = ibge_colors$gray, width = 1.5),
          opacity = 0.7
        )
        p <- p %>% add_lines(
          y = ~monthly_x13,
          name = i18n("plots.monthly_adjusted_x13", lang_val),
          line = list(color = ibge_colors$primary, width = 2)
        )
        p <- p %>% add_lines(
          y = ~monthly_stl,
          name = i18n("plots.monthly_adjusted_stl", lang_val),
          line = list(color = "#4CAF50", width = 2)
        )
      } else if (!is.null(deseason_method) && deseason_method == "x13") {
        # X-13 ARIMA method: show both original and adjusted
        p <- p %>% add_lines(
          y = ~monthly,
          name = i18n("plots.monthly_original", lang_val),
          line = list(color = ibge_colors$gray, width = 1.5, dash = "dot"),
          opacity = 0.7
        )
        p <- p %>% add_lines(
          y = ~monthly_x13,
          name = i18n("plots.monthly_adjusted_x13", lang_val),
          line = list(color = ibge_colors$secondary, width = 2)
        )
      } else if (!is.null(deseason_method) && deseason_method == "stl") {
        # STL method: show both original and adjusted
        p <- p %>% add_lines(
          y = ~monthly,
          name = i18n("plots.monthly_original", lang_val),
          line = list(color = ibge_colors$gray, width = 1.5, dash = "dot"),
          opacity = 0.7
        )
        p <- p %>% add_lines(
          y = ~monthly_stl,
          name = i18n("plots.monthly_adjusted_stl", lang_val),
          line = list(color = ibge_colors$secondary, width = 2)
        )
      } else {
        # No de-seasonalization: show monthly series normally
        p <- p %>% add_lines(
          y = ~monthly,
          name = i18n("plots.monthly", lang_val),
          line = list(color = ibge_colors$secondary, width = 2)
        )
      }

      # Add quarterly overlay if requested and available (only when not comparing de-seasonalization)
      if (input$show_quarterly && "quarterly" %in% names(dt) &&
          (is.null(deseason_method) || deseason_method == "none")) {
        p <- p %>% add_lines(
          y = ~quarterly,
          name = i18n("plots.quarterly", lang_val),
          line = list(color = ibge_colors$gray, width = 2, dash = "dash")
        )
      }

      # Get tick format and locale separators based on display unit type
      series_unit <- display_unit()
      tick_fmt <- get_plotly_tickformat(series_unit, lang_val)
      separators <- get_plotly_separators(lang_val)

      # Build y-axis config
      yaxis_config <- list(
        title = "",
        tickfont = list(size = 10),
        tickformat = tick_fmt,
        hoverformat = tick_fmt
      )

      # Add R$ prefix for currency series (including currency_millions)
      if (series_unit %in% c("currency", "currency_millions")) {
        yaxis_config$tickprefix <- "R$ "
      }

      # Add % suffix for percent series
      if (series_unit == "percent") {
        yaxis_config$ticksuffix <- "%"
        yaxis_config$tickformat <- ",.1f"
      }

      # Add millions suffix for converted population/level series
      if (series_unit == "millions_display") {
        yaxis_config$ticksuffix <- if (lang_val == "en") " M" else " mi"
      }

      # Configure layout
      p %>% layout(
        xaxis = list(title = "", tickfont = list(size = 10)),
        yaxis = yaxis_config,
        separators = separators,  # Locale-specific decimal/thousands separators
        hovermode = "x unified",
        legend = list(orientation = "h", y = 1.08, x = 0.5,
                      xanchor = "center", font = list(size = 10)),
        margin = list(t = 30, b = 40, l = 50, r = 20),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      ) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE,
               modeBarButtonsToRemove = c("lasso2d", "select2d"))
    })

    # --------------------------------------------------------------------------
    # Reactive: Difference plot title text (explicit dependencies)
    # --------------------------------------------------------------------------

    difference_title_text <- reactive({
      deseason_method <- input$deseasonalize
      lang_val <- get_lang()  # Explicit reactive dependency

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
    # Output: Difference plot title (uses reactive for proper dependency tracking)
    # --------------------------------------------------------------------------

    output$difference_plot_title <- renderText({
      difference_title_text()
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

        # Get current language for labels
        lang_val <- get_lang()

        p <- plot_ly(dt_plot, x = ~date) %>%
          add_lines(
            y = ~difference,
            name = i18n("plots.monthly_minus_quarterly", lang_val),
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

        # Get current language for labels
        lang_val <- get_lang()

        p <- plot_ly(dt_plot, x = ~date) %>%
          add_lines(
            y = ~seasonal_x13,
            name = i18n("plots.seasonal_x13", lang_val),
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

        # Get current language for labels
        lang_val <- get_lang()

        p <- plot_ly(dt_plot, x = ~date) %>%
          add_lines(
            y = ~seasonal_stl,
            name = i18n("plots.seasonal_stl", lang_val),
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

        # Get current language for labels
        lang_val <- get_lang()

        p <- plot_ly(dt_plot, x = ~date)

        if ("seasonal_x13" %in% names(dt_plot)) {
          p <- p %>% add_lines(
            y = ~seasonal_x13,
            name = i18n("plots.x13_arima", lang_val),
            line = list(color = ibge_colors$primary, width = 1.5)
          )
        }
        if ("seasonal_stl" %in% names(dt_plot)) {
          p <- p %>% add_lines(
            y = ~seasonal_stl,
            name = i18n("plots.stl", lang_val),
            line = list(color = "#4CAF50", width = 1.5)
          )
        }
      }

      # Get tick format and locale separators based on display unit type
      lang_val <- get_lang()
      series_unit <- display_unit()
      tick_fmt <- get_plotly_tickformat(series_unit, lang_val)
      separators <- get_plotly_separators(lang_val)

      # Build y-axis config
      yaxis_config <- list(
        title = "",
        tickfont = list(size = 10),
        tickformat = tick_fmt,
        hoverformat = tick_fmt
      )

      # Add R$ prefix for currency series (including currency_millions)
      if (series_unit %in% c("currency", "currency_millions")) {
        yaxis_config$tickprefix <- "R$ "
      }

      # Add % suffix for percent series
      if (series_unit == "percent") {
        yaxis_config$ticksuffix <- "%"
        yaxis_config$tickformat <- ",.1f"
      }

      # Add millions suffix for converted population/level series
      if (series_unit == "millions_display") {
        yaxis_config$ticksuffix <- if (lang_val == "en") " M" else " mi"
      }

      # Common layout
      p %>% layout(
        xaxis = list(title = "", tickfont = list(size = 10)),
        yaxis = yaxis_config,
        separators = separators,  # Locale-specific decimal/thousands separators
        hovermode = "x unified",
        showlegend = (deseason_method == "both"),
        legend = list(orientation = "h", y = 1.1, x = 0.5,
                      xanchor = "center", font = list(size = 9)),
        margin = list(t = 10, b = 30, l = 50, r = 20),
        paper_bgcolor = "white",
        plot_bgcolor = "white",
        shapes = list(
          list(type = "line", x0 = min(dt_plot$date), x1 = max(dt_plot$date),
               y0 = 0, y1 = 0,
               line = list(color = ibge_colors$gray, dash = "dash", width = 1))
        )
      ) %>%
        config(displayModeBar = FALSE)
    })

    # --------------------------------------------------------------------------
    # Output: Summary statistics
    # --------------------------------------------------------------------------

    # Helper to get current series unit (from metadata)
    get_current_unit <- reactive({
      req(input$series)
      metadata <- shared_data$series_metadata
      get_series_unit(metadata, input$series)
    })

    # Display unit: "thousands" data is converted to millions for display
    display_unit <- reactive({
      series_unit <- get_current_unit()
      if (series_unit == "thousands") "millions_display" else series_unit
    })

    output$stat_min <- renderText({
      req(filtered_data())
      dt <- filtered_data()
      format_series_value(min(dt$monthly, na.rm = TRUE),
                          unit = display_unit(),
                          lang = get_lang(),
                          include_unit = TRUE)
    })

    output$stat_max <- renderText({
      req(filtered_data())
      dt <- filtered_data()
      format_series_value(max(dt$monthly, na.rm = TRUE),
                          unit = display_unit(),
                          lang = get_lang(),
                          include_unit = TRUE)
    })

    output$stat_mean <- renderText({
      req(filtered_data())
      dt <- filtered_data()
      format_series_value(mean(dt$monthly, na.rm = TRUE),
                          unit = display_unit(),
                          lang = get_lang(),
                          include_unit = TRUE)
    })

    output$stat_latest <- renderText({
      req(filtered_data())
      dt <- filtered_data()
      latest <- dt[which.max(date), monthly]
      format_series_value(latest,
                          unit = display_unit(),
                          lang = get_lang(),
                          include_unit = TRUE)
    })

    output$stat_yoy <- renderText({
      req(filtered_data())
      dt <- filtered_data()
      lang_val <- get_lang()

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
        # YoY change is always a percentage
        formatted <- format_number_i18n(change, digits = 1, lang = lang_val)
        paste0(ifelse(change > 0, "+", ""), formatted, "%")
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

      # Unit-aware decimal digits
      current_unit <- display_unit()
      unit_digits <- switch(current_unit,
        "percent" = 1,
        "currency" = 0,
        "currency_millions" = 0,
        "thousands" = 0,
        "millions" = 0,
        "millions_display" = 1,
        "index" = 2,
        2  # default
      )

      # Locale-aware number formatting
      big_mark <- if(lang_val == "pt") "." else ","
      dec_mark <- if(lang_val == "pt") "," else "."

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
        formatRound(columns = 2:ncol(dt_display), digits = unit_digits,
                    mark = big_mark, dec.mark = dec_mark)
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

    # Helper to get plot title (uses the reactive for consistency)
    get_plot_title <- function() {
      plot_title_text()
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

        # Unit-aware y-axis labels
        current_unit <- display_unit()
        big_mark <- if(lang_val == "pt") "." else ","
        dec_mark <- if(lang_val == "pt") "," else "."

        y_labels <- switch(current_unit,
          "percent" = scales::label_number(suffix = "%", accuracy = 0.1,
                                            big.mark = big_mark, decimal.mark = dec_mark),
          "currency" = scales::label_number(prefix = "R$ ", accuracy = 1,
                                             big.mark = big_mark, decimal.mark = dec_mark),
          "currency_millions" = scales::label_number(prefix = "R$ ", suffix = if(lang_val == "pt") " mi" else " M",
                                                      accuracy = 1,
                                                      big.mark = big_mark, decimal.mark = dec_mark),
          "millions_display" = scales::label_number(suffix = if(lang_val == "pt") " mi" else " M",
                                                     accuracy = 0.1,
                                                     big.mark = big_mark, decimal.mark = dec_mark),
          "index" = scales::label_number(accuracy = 0.01,
                                          big.mark = big_mark, decimal.mark = dec_mark),
          scales::label_number(big.mark = big_mark, decimal.mark = dec_mark)
        )

        p <- ggplot(dt, aes(x = date)) +
          geom_line(aes(y = monthly), color = ibge_colors$secondary, linewidth = 1) +
          scale_y_continuous(labels = y_labels) +
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

          # Check if PNADCperiods package is available (use variable to avoid packrat detection)
          pkg_name <- "PNADCperiods"
          if (!requireNamespace(pkg_name, quietly = TRUE)) {
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
          fetch_fn <- getFromNamespace("fetch_sidra_rolling_quarters", pkg_name)
          new_rq <- fetch_fn(verbose = FALSE)

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
          mensalize_fn <- getFromNamespace("mensalize_sidra_series", pkg_name)
          new_monthly <- mensalize_fn(new_rq, verbose = FALSE)

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
