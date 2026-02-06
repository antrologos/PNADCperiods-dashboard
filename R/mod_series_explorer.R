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

# Note: %||% operator is available from rlang (loaded by Shiny)

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
          # Refresh button with dynamic i18n tooltip
          uiOutput(ns("refresh_button_ui"))
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

    # Deflation reference date (update when IPCA base changes)
    DEFLATION_REF_DATE <- list(en = "Dec/2024", pt = "Dez/2024")

    # UI dimension constants
    SIDEBAR_WIDTH <- "320px"
    MAIN_PLOT_HEIGHT <- "380px"
    DIFF_PLOT_HEIGHT <- "180px"
    PNG_EXPORT_DPI <- 150
    BUTTON_REENABLE_DELAY_MS <- 60000

    # Top series for de-seasonalization precompute
    TOP_SERIES_FOR_PRECOMPUTE <- c(
      "taxadesocup", "taxapartic", "nivelocup", "niveldesocup",
      "taxacombdesosub", "taxacompsubutlz", "percdesalento", "perccontribprev",
      "populacao", "pop14mais", "popnaforca", "popocup", "popdesocup", "popforadaforca",
      "rendhabnominaltodos", "rendhabrealtodos", "rendefetnominaltodos", "rendefetrealtodos",
      "empregado", "contapropria", "empregador",
      "agropecuaria", "industria", "comercio", "construcao"
    )

    # Default theme when none is selected
    DEFAULT_THEME <- "labor_market"

    # --------------------------------------------------------------------------
    # Helper Functions (avoid code duplication)
    # --------------------------------------------------------------------------

    # Filter series choices to only those available in the data
    filter_available_series <- function(series_choices, monthly_data) {
      if (is.null(monthly_data) || !inherits(monthly_data, c("data.frame", "data.table")) ||
          length(series_choices) == 0) {
        return(series_choices)
      }
      dt_cols <- names(monthly_data)
      available <- vapply(series_choices, function(s) {
        paste0("m_", s) %in% dt_cols || s %in% dt_cols
      }, FUN.VALUE = logical(1))
      series_choices[available]
    }

    # Resolve monthly column name (with or without m_ prefix)
    resolve_monthly_column <- function(data, series_name) {
      monthly_col <- paste0("m_", series_name)
      if (!monthly_col %in% names(data)) {
        monthly_col <- series_name
      }
      if (!monthly_col %in% names(data)) {
        return(NULL)
      }
      monthly_col
    }

    # Helper to get current language (simple alias for clarity)
    get_lang <- lang

    # --------------------------------------------------------------------------
    # i18n Label Outputs (sidebar)
    # --------------------------------------------------------------------------

    output$label_last_updated <- renderText({ i18n("messages.last_updated", get_lang()) })
    output$label_theme <- renderText({ toupper(i18n("controls.theme", get_lang())) })
    output$label_category <- renderText({ toupper(i18n("controls.category", get_lang())) })
    output$label_subcategory <- renderText({ toupper(i18n("controls.subcategory", get_lang())) })
    output$label_series <- renderText({ toupper(i18n("controls.series", get_lang())) })
    output$label_date_range <- renderText({ toupper(i18n("controls.date_range", get_lang())) })
    output$label_display <- renderText({ toupper(i18n("controls.display", get_lang())) })
    output$label_show_quarterly <- renderText({ i18n("controls.show_quarterly", get_lang()) })
    output$label_show_difference <- renderText({ i18n("controls.show_difference", get_lang()) })
    output$label_seasonal_adjustment <- renderText({ toupper(i18n("controls.deseasonalization", get_lang())) })
    output$label_export <- renderText({ toupper(i18n("controls.export", get_lang())) })

    # i18n Label Outputs (main panel)
    output$label_summary_stats <- renderText({ i18n("stats.title", get_lang()) })
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
      checkboxInput(session$ns("show_table"), i18n("buttons.show", lang_val),
                    value = isolate(input$show_table) %||% FALSE, width = "auto")
    })

    # --------------------------------------------------------------------------
    # Dynamic UI: Refresh button (for i18n tooltip)
    # --------------------------------------------------------------------------

    output$refresh_button_ui <- renderUI({
      lang_val <- get_lang()
      actionButton(
        session$ns("refresh_sidra"),
        label = NULL,
        icon = icon("sync", class = "fa-sm"),
        class = "btn-sm btn-outline-primary",
        title = i18n("buttons.refresh_tooltip", lang_val),
        style = "width: 28px; height: 28px; padding: 0; border-radius: 50%;"
      )
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
    # Dynamic UI: Date range selection (slider with month/year labels)
    # --------------------------------------------------------------------------

    # Reactive values to track date range state
    date_range_state <- reactiveValues(
      available_months = NULL,  # Vector of YYYYMM integers
      start_month = NULL,
      end_month = NULL
    )

    # Convert Date back to YYYYMM (snap to nearest month on the 15th)
    date_to_yyyymm <- function(d) {
      # Snap to the 15th of the month to avoid day drift issues
      as.integer(format(d, "%Y%m"))
    }

    output$date_range_ui <- renderUI({
      lang_val <- get_lang()
      months <- isolate(date_range_state$available_months)

      if (is.null(months) || length(months) == 0) {
        return(NULL)
      }

      # Convert YYYYMM to Date objects for slider (use global yyyymm_to_date)
      months_sorted <- sort(unique(months))

      # Handle single-month edge case
      if (length(months_sorted) == 1) {
        return(tagList(
          tags$label(
            style = "font-size: 0.75rem; font-weight: 600;",
            toupper(i18n("controls.date_range", lang_val))
          ),
          div(
            style = "margin-top: 8px; padding: 8px; background: #f8f9fa; border-radius: 4px;",
            tags$span(
              style = "font-size: 0.85rem;",
              format_date_i18n(months_sorted[1], lang_val, format = "short")
            )
          )
        ))
      }

      min_date <- yyyymm_to_date(min(months_sorted))
      max_date <- yyyymm_to_date(max(months_sorted))

      # Get current values
      start_val <- isolate(date_range_state$start_month)
      end_val <- isolate(date_range_state$end_month)

      # Ensure values are valid
      if (is.null(start_val) || !start_val %in% months_sorted) {
        start_val <- min(months_sorted)
      }
      if (is.null(end_val) || !end_val %in% months_sorted) {
        end_val <- max(months_sorted)
      }

      start_date <- yyyymm_to_date(start_val)
      end_date <- yyyymm_to_date(end_val)

      # Time format: %b gives abbreviated month name (Jan, Feb, etc.)
      # Note: Browser locale determines actual month name language
      time_format <- "%b %Y"

      tagList(
        tags$label(
          style = "font-size: 0.75rem; font-weight: 600;",
          toupper(i18n("controls.date_range", lang_val))
        ),
        div(
          style = "margin-top: 8px;",
          sliderInput(
            session$ns("date_slider"),
            label = NULL,
            min = min_date,
            max = max_date,
            value = c(start_date, end_date),
            step = 1,  # Daily step - Shiny handles month snapping internally
            timeFormat = time_format,
            width = "100%"
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

    # Track slider changes
    observeEvent(input$date_slider, {
      if (!is.null(input$date_slider) && length(input$date_slider) == 2) {
        # Convert slider Date values to YYYYMM
        date_range_state$start_month <- date_to_yyyymm(input$date_slider[1])
        date_range_state$end_month <- date_to_yyyymm(input$date_slider[2])
      }
    }, ignoreInit = TRUE)

    # Computed date range (for filtering) - debounced to prevent excessive updates during slider drag
    date_range <- reactive({
      slider_val <- input$date_slider

      if (is.null(slider_val) || length(slider_val) != 2) {
        return(NULL)
      }

      # Validate slider values are Date objects
      if (!inherits(slider_val, "Date")) {
        return(NULL)
      }

      # Ensure start <= end (swap if reversed)
      if (slider_val[1] > slider_val[2]) {
        return(c(slider_val[2], slider_val[1]))
      }

      c(slider_val[1], slider_val[2])
    }) %>% debounce(300)  # 300ms delay to prevent rapid updates during slider drag

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
        } else if (DEFAULT_THEME %in% themes) {
          DEFAULT_THEME
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
        series_choices <- filter_available_series(series_choices, shared_data$monthly_sidra)

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
          selected = if (DEFAULT_THEME %in% themes) DEFAULT_THEME else themes[1]
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
        if (input$theme == DEFAULT_THEME && "unemployment" %in% category_choices) {
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
        series_choices <- filter_available_series(series_choices, shared_data$monthly_sidra)

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
      series_choices <- filter_available_series(series_choices, shared_data$monthly_sidra)

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

      # Update the slider directly for immediate feedback (use global yyyymm_to_date)
      min_date <- yyyymm_to_date(min(months))
      max_date <- yyyymm_to_date(max(months))

      updateSliderInput(session, "date_slider",
                        min = min_date,
                        max = max_date,
                        value = c(min_date, max_date))

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

      # IMPORTANT: Compute de-seasonalization on FULL series BEFORE filtering
      # Seasonal adjustment algorithms need the complete time series to work correctly
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

        # Compute de-seasonalization on FULL data (before date filtering)
        # Try to use cached values first (computed on full data during refresh)
        if (!is.null(cached_data) && "x13" %in% names(cached_data)) {
          # Cache stores values aligned with shared_data$monthly_sidra$anomesexato
          result[, monthly_x13 := cached_data$x13[match(anomesexato,
                                  shared_data$monthly_sidra$anomesexato)]]
        } else if (deseason_available["x13"]) {
          # Compute on-the-fly using FULL series (result is still full at this point)
          result[, monthly_x13 := deseasonalize_x13(monthly, date)]
        }

        if (!is.null(cached_data) && "stl" %in% names(cached_data)) {
          result[, monthly_stl := cached_data$stl[match(anomesexato,
                                  shared_data$monthly_sidra$anomesexato)]]
        } else if (deseason_available["stl"]) {
          # Compute on-the-fly using FULL series
          result[, monthly_stl := deseasonalize_stl(monthly, date)]
        }
      }

      # NOW filter by date range (AFTER de-seasonalization is computed on full series)
      dr <- date_range()
      if (!is.null(dr) && length(dr) == 2) {
        result <- result[date >= dr[1] & date <= dr[2]]
      }

      # Calculate difference (after filtering)
      if ("quarterly" %in% names(result)) {
        result[, difference := monthly - quarterly]
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
        i18n("messages.not_available", get_lang())
      }
    })

    # --------------------------------------------------------------------------
    # Reactive: Plot title text (explicit dependencies for reliable updates)
    # --------------------------------------------------------------------------

    plot_title_text <- reactive({
      req(input$series)
      lang_val <- get_lang()  # Explicit reactive dependency
      metadata <- shared_data$series_metadata
      get_series_description(metadata, input$series, lang_val)
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
        ref_date <- DEFLATION_REF_DATE[[lang_val]]
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
          i18n("data_panel.no_description", lang_val)
        }
      } else {
        i18n("data_panel.no_description", lang_val)
      }
    })

    # --------------------------------------------------------------------------
    # Output: Main plot
    # --------------------------------------------------------------------------

    output$main_plot <- renderPlotly({
      dt <- filtered_data()
      lang_val <- get_lang()

      # Validate data
      validate(
        need(!is.null(dt), i18n("messages.loading_data", lang_val)),
        need(nrow(dt) > 0, i18n("messages.no_data_range", lang_val))
      )

      # Build plot with plotly directly for better control
      p <- plot_ly(dt, x = ~date)

      deseason_method <- input$deseasonalize

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
          line = list(color = ibge_colors$tertiary, width = 2)
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
      separators <- get_plotly_separators(lang_val)
      yaxis_config <- build_yaxis_config(series_unit, lang_val)

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
        i18n("plots.diff_monthly_quarterly", lang_val)
      } else if (deseason_method == "x13") {
        i18n("plots.diff_seasonal_x13", lang_val)
      } else if (deseason_method == "stl") {
        i18n("plots.diff_seasonal_stl", lang_val)
      } else if (deseason_method == "both") {
        i18n("plots.diff_seasonal_comparison", lang_val)
      } else {
        i18n("plots.diff_generic", lang_val)
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
      lang_val <- get_lang()

      validate(need(!is.null(dt) && nrow(dt) > 0, i18n("messages.no_data", lang_val)))

      deseason_method <- input$deseasonalize

      # Determine what to plot based on seasonal adjustment selection
      if (is.null(deseason_method) || deseason_method == "none") {
        # Show Monthly - Quarterly difference
        validate(
          need("difference" %in% names(dt),
               i18n("messages.quarterly_not_available", lang_val)),
          need(!all(is.na(dt$difference)),
               i18n("messages.no_difference_data", lang_val))
        )

        dt_plot <- copy(dt[!is.na(difference)])
        validate(need(nrow(dt_plot) > 0,
                      i18n("messages.no_difference_range", lang_val)))

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
          need("monthly_x13" %in% names(dt),
               i18n("messages.x13_not_available", lang_val)),
          need(!all(is.na(dt$monthly_x13)),
               i18n("messages.no_x13_data", lang_val))
        )

        dt_plot <- copy(dt[!is.na(monthly_x13)])
        dt_plot[, seasonal_x13 := monthly - monthly_x13]

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
          need("monthly_stl" %in% names(dt),
               i18n("messages.stl_not_available", lang_val)),
          need(!all(is.na(dt$monthly_stl)),
               i18n("messages.no_stl_data", lang_val))
        )

        dt_plot <- copy(dt[!is.na(monthly_stl)])
        dt_plot[, seasonal_stl := monthly - monthly_stl]

        p <- plot_ly(dt_plot, x = ~date) %>%
          add_lines(
            y = ~seasonal_stl,
            name = i18n("plots.seasonal_stl", lang_val),
            line = list(color = ibge_colors$tertiary, width = 1.5),
            fill = "tozeroy",
            fillcolor = "rgba(0, 172, 193, 0.15)"
          )

      } else if (deseason_method == "both") {
        # Show both seasonal components
        validate(
          need("monthly_x13" %in% names(dt) || "monthly_stl" %in% names(dt),
               i18n("messages.no_data", lang_val))
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
            name = i18n("plots.x13_arima", lang_val),
            line = list(color = ibge_colors$primary, width = 1.5)
          )
        }
        if ("seasonal_stl" %in% names(dt_plot)) {
          p <- p %>% add_lines(
            y = ~seasonal_stl,
            name = i18n("plots.stl", lang_val),
            line = list(color = ibge_colors$tertiary, width = 1.5)
          )
        }
      }

      # Get tick format and locale separators based on display unit type
      series_unit <- display_unit()
      separators <- get_plotly_separators(lang_val)
      yaxis_config <- build_yaxis_config(series_unit, lang_val)

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

    # Helper to build plotly y-axis config based on series unit and language
    build_yaxis_config <- function(series_unit, lang_val) {
      tick_fmt <- get_plotly_tickformat(series_unit, lang_val)

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

      yaxis_config
    }

    output$stat_min <- renderText({
      dt <- filtered_data()
      validate(
        need(!is.null(dt) && nrow(dt) > 0, ""),
        need(any(!is.na(dt$monthly)), "")
      )
      val <- min(dt$monthly, na.rm = TRUE)
      if (!is.finite(val)) return("N/A")
      format_series_value(val,
                          unit = display_unit(),
                          lang = get_lang(),
                          include_unit = TRUE)
    })

    output$stat_max <- renderText({
      dt <- filtered_data()
      validate(
        need(!is.null(dt) && nrow(dt) > 0, ""),
        need(any(!is.na(dt$monthly)), "")
      )
      val <- max(dt$monthly, na.rm = TRUE)
      if (!is.finite(val)) return("N/A")
      format_series_value(val,
                          unit = display_unit(),
                          lang = get_lang(),
                          include_unit = TRUE)
    })

    output$stat_mean <- renderText({
      dt <- filtered_data()
      validate(
        need(!is.null(dt) && nrow(dt) > 0, ""),
        need(any(!is.na(dt$monthly)), "")
      )
      val <- mean(dt$monthly, na.rm = TRUE)
      if (!is.finite(val)) return("N/A")
      format_series_value(val,
                          unit = display_unit(),
                          lang = get_lang(),
                          include_unit = TRUE)
    })

    output$stat_latest <- renderText({
      dt <- filtered_data()
      validate(
        need(!is.null(dt) && nrow(dt) > 0, ""),
        need("date" %in% names(dt), ""),
        need(any(!is.na(dt$date)), "")
      )
      latest <- dt[which.max(date), monthly]
      if (length(latest) == 0 || !is.finite(latest)) return("N/A")
      format_series_value(latest,
                          unit = display_unit(),
                          lang = get_lang(),
                          include_unit = TRUE)
    })

    output$stat_yoy <- renderText({
      dt <- filtered_data()
      validate(
        need(!is.null(dt) && nrow(dt) > 0, ""),
        need("date" %in% names(dt) && inherits(dt$date, "Date"), ""),
        need(any(!is.na(dt$date)), "")
      )
      lang_val <- get_lang()

      # Get latest and value from 12 months ago
      dt <- dt[order(date)]
      latest_date <- max(dt$date, na.rm = TRUE)
      if (is.na(latest_date) || !is.finite(as.numeric(latest_date))) return("N/A")

      # Use proper 12-month offset instead of 365-day approximation
      # This handles leap years correctly and ensures same-month comparison
      yoy_date <- seq(latest_date, by = "-12 months", length.out = 2)[2]
      latest_val <- dt[date == latest_date, monthly][1]
      if (is.na(latest_val) || !is.finite(latest_val)) return("N/A")

      # Find exact same month last year, or closest available
      dt_past <- dt[date <= yoy_date]
      if (nrow(dt_past) == 0) {
        return("N/A")
      }
      yoy_val <- dt_past[which.max(date), monthly]

      if (length(yoy_val) > 0 && !is.na(yoy_val) && is.finite(yoy_val) && yoy_val != 0) {
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

    # Helper to get actual value multiplier based on unit type
    #
    # IMPORTANT: filtered_data() already converts for display:
    # - "thousands" data is divided by 1000 (displayed as millions, e.g., "110.0 mi")
    # - "currency_millions" data is kept as-is (displayed as millions)
    #
    # To get ACTUAL values for export:
    # - "thousands" (now in millions): × 1,000,000 → actual people
    #   e.g., 110.001 mi × 1,000,000 = 110,001,000 people
    # - "currency_millions": × 1,000,000 → actual R$
    #   e.g., 370.1 mi × 1,000,000 = R$ 370,100,000,000
    get_actual_multiplier <- function(original_unit) {
      if (original_unit == "thousands") {
        1000000  # millions (after /1000 in filtered_data) → actual people
      } else if (original_unit == "currency_millions") {
        1000000  # millions → actual R$
      } else {
        1  # percent, currency, index are already actual values
      }
    }

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

      dt_display <- copy(dt[, ..cols])

      # Convert display values to actual values for data table
      # Plot axes show formatted values (e.g., "110.0 mi") but data table
      # should show exact numbers (e.g., 110,000,000)
      original_unit <- get_current_unit()
      actual_multiplier <- get_actual_multiplier(original_unit)

      # Apply multiplier to numeric columns (skip date column - columns not yet renamed)
      if (actual_multiplier != 1) {
        numeric_cols <- setdiff(names(dt_display), "date")
        for (col in numeric_cols) {
          if (is.numeric(dt_display[[col]])) {
            dt_display[, (col) := get(col) * actual_multiplier]
          }
        }
      }

      # i18n column names (after conversion to preserve column matching)
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

      # i18n table language options (full localization)
      dt_language <- list(
        search = i18n("data_panel.search", lang_val),
        lengthMenu = i18n("data_panel.length_menu", lang_val),
        info = i18n("data_panel.info", lang_val),
        infoEmpty = i18n("data_panel.info_empty", lang_val),
        infoFiltered = i18n("data_panel.info_filtered", lang_val),
        zeroRecords = i18n("data_panel.zero_records", lang_val),
        paginate = list(
          first = i18n("data_panel.paginate_first", lang_val),
          last = i18n("data_panel.paginate_last", lang_val),
          `next` = i18n("data_panel.paginate_next", lang_val),
          previous = i18n("data_panel.paginate_previous", lang_val)
        )
      )

      # Decimal digits for actual values
      unit_digits <- switch(original_unit,
        "percent" = 1,
        "currency" = 0,
        "currency_millions" = 0,
        "thousands" = 0,
        "index" = 2,
        0  # default for counts
      )

      # Locale-aware number formatting
      big_mark <- if(lang_val == "pt") "." else ","
      dec_mark <- if(lang_val == "pt") "," else "."

      # Get numeric column names after renaming for formatRound
      date_col_name <- i18n("data_panel.date", lang_val)
      numeric_col_names <- setdiff(names(dt_display), date_col_name)

      datatable(
        dt_display,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip',
          language = dt_language
        ),
        rownames = FALSE,
        style = "bootstrap4",
        class = "table-sm table-striped"
      ) %>%
        formatRound(columns = numeric_col_names, digits = unit_digits,
                    mark = big_mark, dec.mark = dec_mark)
    })

    # --------------------------------------------------------------------------
    # Download: CSV
    # --------------------------------------------------------------------------

    # Helper to convert display values to actual values for export
    # Takes original_unit as parameter to avoid calling reactive from non-reactive context
    convert_to_actual_values <- function(dt, original_unit) {
      if (is.null(dt) || nrow(dt) == 0) {
        return(dt)
      }

      dt_export <- copy(dt)
      actual_multiplier <- get_actual_multiplier(original_unit)

      # Apply multiplier to numeric columns (skip date and anomesexato)
      if (actual_multiplier != 1) {
        numeric_cols <- setdiff(names(dt_export), c("date", "anomesexato"))
        for (col in numeric_cols) {
          if (is.numeric(dt_export[[col]])) {
            dt_export[, (col) := get(col) * actual_multiplier]
          }
        }
      }

      dt_export
    }

    # Helper to sanitize series name for safe filenames (with length limit)
    sanitize_filename <- function(x, max_length = 50) {
      if (is.null(x) || x == "") return("series")
      clean <- gsub("[^a-zA-Z0-9_-]", "_", x)
      if (nchar(clean) > max_length) {
        clean <- substr(clean, 1, max_length)
      }
      clean
    }

    output$download_csv <- downloadHandler(
      filename = function() {
        series_name <- input$series %||% "series"
        safe_series <- sanitize_filename(series_name)
        paste0("pnadcperiods_", safe_series, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        # Capture unit in reactive context (downloadHandler content has access to reactives)
        dt <- filtered_data()
        original_unit <- get_current_unit()

        if (is.null(dt) || nrow(dt) == 0) {
          showNotification(i18n("messages.no_data", get_lang()), type = "warning")
          fwrite(data.table(), file, bom = TRUE)
          return()
        }

        dt_export <- convert_to_actual_values(dt, original_unit)
        # Add UTF-8 BOM for Excel compatibility with Brazilian Portuguese characters
        fwrite(dt_export, file, bom = TRUE)
      },
      contentType = "text/csv"
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
        series_name <- input$series %||% "series"
        safe_series <- sanitize_filename(series_name)
        paste0("pnadcperiods_", safe_series, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(filtered_data())
        dt <- filtered_data()
        lang_val <- get_lang()

        # i18n caption
        caption_text <- sprintf(i18n("plots.caption", lang_val), Sys.Date())

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

        # Add de-seasonalized series if selected and available
        deseason_method <- input$deseasonalize
        if (!is.null(deseason_method) && deseason_method != "none") {
          if (deseason_method %in% c("x13", "both") && "monthly_x13" %in% names(dt) && any(!is.na(dt$monthly_x13))) {
            p <- p +
              geom_line(aes(y = monthly_x13), color = ibge_colors$primary,
                        linewidth = 1, linetype = "solid", alpha = 0.8)
          }
          if (deseason_method %in% c("stl", "both") && "monthly_stl" %in% names(dt) && any(!is.na(dt$monthly_stl))) {
            p <- p +
              geom_line(aes(y = monthly_stl), color = ibge_colors$tertiary,
                        linewidth = 1, linetype = "solid", alpha = 0.8)
          }
        }

        ggsave(file, plot = p, width = 10, height = 6, dpi = PNG_EXPORT_DPI, bg = "white")
      },
      contentType = "image/png"
    )

    # --------------------------------------------------------------------------
    # Download: Excel
    # --------------------------------------------------------------------------

    output$download_xlsx <- downloadHandler(
      filename = function() {
        series_name <- input$series %||% "series"
        safe_series <- sanitize_filename(series_name)
        # Check if openxlsx is available; if not, use .csv extension
        if (requireNamespace("openxlsx", quietly = TRUE)) {
          paste0("pnadcperiods_", safe_series, "_", Sys.Date(), ".xlsx")
        } else {
          paste0("pnadcperiods_", safe_series, "_", Sys.Date(), ".csv")
        }
      },
      content = function(file) {
        # Capture unit in reactive context
        dt <- filtered_data()
        original_unit <- get_current_unit()

        if (is.null(dt) || nrow(dt) == 0) {
          showNotification(i18n("messages.no_data", get_lang()), type = "warning")
          if (requireNamespace("openxlsx", quietly = TRUE)) {
            openxlsx::write.xlsx(data.frame(), file)
          } else {
            fwrite(data.table(), file, bom = TRUE)
          }
          return()
        }

        dt_export <- convert_to_actual_values(dt, original_unit)

        if (requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(as.data.frame(dt_export), file)
        } else {
          # Fallback to CSV (filename already adjusted above)
          # Notify user about the fallback
          showNotification(
            i18n("messages.excel_fallback_csv", get_lang()),
            type = "warning",
            duration = 5
          )
          fwrite(dt_export, file, bom = TRUE)
        }
      },
      # contentType must be a static string, not a function
      # When openxlsx unavailable, file is CSV but browser may still handle it correctly
      # based on file extension. This is a known limitation of Shiny's downloadHandler.
      contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
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
          pkg_list <- paste(missing_packages, collapse = ", ")
          showNotification(
            sprintf(i18n("messages.seasonal_unavailable", get_lang()), pkg_list),
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
            sprintf(i18n("messages.refresh_cooldown", get_lang()), remaining),
            type = "warning"
          )
          return()
        }
      }

      # Disable button during fetch (if shinyjs available)
      # Use globalenv() explicitly for reliable scope access
      shinyjs_ok <- exists("shinyjs_available", envir = globalenv()) &&
                    get("shinyjs_available", envir = globalenv())
      if (shinyjs_ok) {
        shinyjs::disable("refresh_sidra")
      }

      # Mark refresh attempt time
      session_cache$last_refresh_attempt <- Sys.time()

      # Capture language before tryCatch for use in error handler
      lang_val <- get_lang()

      withProgress(message = i18n("messages.checking_new_data", lang_val), value = 0, {
        tryCatch({
          incProgress(0.1, detail = i18n("messages.querying_api", lang_val))

          # Check if PNADCperiods package is available (use variable to avoid packrat detection)
          pkg_name <- "PNADCperiods"
          if (!requireNamespace(pkg_name, quietly = TRUE)) {
            showNotification(
              sprintf(i18n("messages.package_unavailable", lang_val), pkg_name),
              type = "error"
            )
            # Re-enable button before early return
            if (shinyjs_ok) {
              shinyjs::enable("refresh_sidra")
            }
            return()
          }

          # First, check if there's actually new data available
          # Get current latest date from our data
          current_max_date <- NULL
          if (!is.null(shared_data$rolling_quarters)) {
            current_max_date <- max(shared_data$rolling_quarters$anomesfinaltrimmovel, na.rm = TRUE)
          }

          # Fetch new rolling quarters data
          # Progress: 0.1 + 0.15 = 0.25 total
          incProgress(0.15, detail = i18n("messages.fetching", lang_val))
          fetch_fn <- getFromNamespace("fetch_sidra_rolling_quarters", pkg_name)
          new_rq <- fetch_fn(verbose = FALSE)

          # Check if there's new data
          new_max_date <- max(new_rq$anomesfinaltrimmovel, na.rm = TRUE)

          if (!is.null(current_max_date) && new_max_date <= current_max_date) {
            # No new data available
            latest_formatted <- paste0(substr(current_max_date, 1, 4), "-",
                                        substr(current_max_date, 5, 6))
            showNotification(
              sprintf(i18n("messages.no_new_data_detail", lang_val), latest_formatted),
              type = "message",
              duration = 5
            )
            # Re-enable button
            if (shinyjs_ok) {
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

          # Progress: 0.25 + 0.15 = 0.4 total
          incProgress(0.15, detail = sprintf(i18n("messages.found_new_periods", lang_val),
                                            n_new_periods))
          mensalize_fn <- getFromNamespace("mensalize_sidra_series", pkg_name)
          new_monthly <- mensalize_fn(new_rq, verbose = FALSE)

          # Update shared data
          shared_data$rolling_quarters <- new_rq
          shared_data$monthly_sidra <- new_monthly
          shared_data$last_updated <- Sys.time()

          # Save to disk (with directory check)
          # Use app directory for reliable path resolution
          data_dir <- file.path(getShinyOption("appDir", getwd()), "data")
          if (dir.exists(data_dir)) {
            saveRDS(new_rq, file.path(data_dir, "rolling_quarters.rds"))
            saveRDS(new_monthly, file.path(data_dir, "monthly_sidra.rds"))
          } else {
            # Notify user, not just console warning
            showNotification(
              i18n("messages.data_dir_not_found", lang_val),
              type = "warning",
              duration = 5
            )
          }

          # Recompute de-seasonalized series for top series
          # Progress: 0.4 + 0.4 = 0.8 total
          incProgress(0.4, detail = i18n("messages.updating_seasonal", lang_val))

          deseason_available <- check_deseasonalization_available()
          if (deseason_available["x13"] || deseason_available["stl"]) {

            # Get dates from monthly data
            dates <- as.Date(paste0(substr(new_monthly$anomesexato, 1, 4), "-",
                                    substr(new_monthly$anomesexato, 5, 6), "-15"))

            # Use constant for top series to precompute
            new_cache <- list()
            for (series_name in TOP_SERIES_FOR_PRECOMPUTE) {
              monthly_col <- resolve_monthly_column(new_monthly, series_name)

              if (!is.null(monthly_col)) {
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
                  # Log error but continue with other series
                  message("De-seasonalization failed for ", series_name, ": ", e$message)
                })
              }
            }

            # Update and save de-seasonalized cache
            if (length(new_cache) > 0) {
              shared_data$deseasonalized_cache <- new_cache
              if (dir.exists(data_dir)) {
                saveRDS(new_cache, file.path(data_dir, "deseasonalized_cache.rds"))
              }
            }
          }

          # Progress: 0.8 + 0.2 = 1.0 total (complete)
          incProgress(0.2, detail = i18n("messages.saving_data", lang_val))
          showNotification(
            sprintf(i18n("messages.data_updated_detail", lang_val), n_new_periods),
            type = "message"
          )

          # Re-enable button immediately on success
          if (shinyjs_ok) {
            shinyjs::enable("refresh_sidra")
          }

        }, error = function(e) {
          # Log detailed error for debugging
          message("SIDRA refresh error: ", e$message)
          # Show generic message to user (lang_val captured before tryCatch)
          showNotification(i18n("messages.error_generic", lang_val), type = "error")
          # Re-enable button on error
          if (shinyjs_ok) {
            shinyjs::enable("refresh_sidra")
          }
        })
      })

      # Re-enable button after delay (if shinyjs available)
      # Use constant instead of magic number
      if (shinyjs_ok) {
        shinyjs::delay(BUTTON_REENABLE_DELAY_MS, shinyjs::enable("refresh_sidra"))
      }
    })

  })
}
