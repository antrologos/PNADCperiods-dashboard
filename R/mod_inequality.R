# ==============================================================================
# PNADCperiods Dashboard - Inequality Tab Module
# ==============================================================================

# ==============================================================================
# UI
# ==============================================================================

inequalityUI <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = TRUE,

    # Sidebar
    sidebar = sidebar(
      width = 320,
      class = "inequality-sidebar",

      # Theme selector
      uiOutput(ns("theme_selector")),

      # Measure selector (dynamic based on theme)
      uiOutput(ns("measure_selector")),

      # Breakdown selector
      uiOutput(ns("breakdown_selector")),

      # Group filter (conditional, for non-overall breakdowns)
      uiOutput(ns("group_filter_ui")),

      # Date range slider (time series themes only)
      uiOutput(ns("date_range_ui")),

      # Period picker (lorenz single / decomposition)
      uiOutput(ns("period_picker_ui")),

      # Period comparison (for Lorenz compare)
      uiOutput(ns("period_comparison_ui")),

      # Display options
      uiOutput(ns("display_options_ui")),

      # Export
      tags$hr(),
      tags$div(
        class = "d-flex gap-2",
        downloadButton(ns("download_csv"), "CSV", class = "btn btn-outline-primary btn-sm flex-fill"),
        downloadButton(ns("download_png"), "PNG", class = "btn btn-outline-primary btn-sm flex-fill"),
        downloadButton(ns("download_excel"), "Excel", class = "btn btn-outline-primary btn-sm flex-fill")
      )
    ),

    # Main panel
    tags$div(
      # Methodology banner
      tags$div(
        class = "methodology-banner mb-3",
        tags$div(
          class = "d-flex align-items-center",
          bs_icon("info-circle"),
          tags$span(class = "ms-2", textOutput(ns("info_banner"), inline = TRUE)),
          tags$a(
            href = "https://antrologos.github.io/PNADCperiods/articles/annual-poverty-analysis.html",
            target = "_blank",
            class = "ms-2 text-white",
            textOutput(ns("learn_more_link"), inline = TRUE)
          )
        )
      ),

      # Main plot
      card(
        card_header(textOutput(ns("main_plot_title"), inline = TRUE)),
        card_body(
          plotlyOutput(ns("main_plot"), height = "400px")
        )
      ),

      # Secondary plot (conditional: stacked shares, decomposition bar)
      uiOutput(ns("secondary_plot_ui")),

      # Stat cards
      uiOutput(ns("stat_cards_ui")),

      # Data table
      uiOutput(ns("data_table_ui"))
    )
  )
}


# ==============================================================================
# Server
# ==============================================================================

inequalityServer <- function(id, shared_data, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ====================================================================
    # Reactive: Available data
    # ====================================================================

    ineq_data <- reactive({
      shared_data$inequality_data
    })

    shares_data <- reactive({
      shared_data$income_shares_data
    })

    lorenz_data_all <- reactive({
      shared_data$lorenz_data
    })

    decomp_data <- reactive({
      shared_data$income_decomposition_data
    })

    # ====================================================================
    # Theme choices
    # ====================================================================

    theme_choices <- reactive({
      lang_val <- lang()
      c(
        setNames("income_level", i18n("inequality.theme_income_level", lang_val)),
        setNames("distribution", i18n("inequality.theme_distribution", lang_val)),
        setNames("shares", i18n("inequality.theme_shares", lang_val)),
        setNames("lorenz", i18n("inequality.theme_lorenz", lang_val)),
        setNames("decomposition", i18n("inequality.theme_decomposition", lang_val))
      )
    })

    output$theme_selector <- renderUI({
      selectInput(ns("theme"), i18n("inequality.theme", lang()),
                  choices = theme_choices(),
                  selected = isolate(input$theme) %||% "distribution")
    })

    # ====================================================================
    # Measure choices (depend on theme)
    # ====================================================================

    measure_choices <- reactive({
      lang_val <- lang()
      theme <- input$theme
      if (is.null(theme)) theme <- "distribution"

      switch(theme,
        income_level = c(
          setNames("mean_income", i18n("inequality.mean_income", lang_val)),
          setNames("median_income", i18n("inequality.median_income", lang_val))
        ),
        distribution = c(
          setNames("gini", i18n("inequality.gini", lang_val)),
          setNames("palma", i18n("inequality.palma", lang_val)),
          setNames("p90p10", i18n("inequality.p90p10", lang_val)),
          setNames("p90p50", i18n("inequality.p90p50", lang_val)),
          setNames("p50p10", i18n("inequality.p50p10", lang_val)),
          setNames("top1_share", i18n("inequality.top1_share", lang_val)),
          setNames("top5_share", i18n("inequality.top5_share", lang_val)),
          setNames("top10_share", i18n("inequality.top10_share", lang_val)),
          setNames("bottom50_share", i18n("inequality.bottom50_share", lang_val))
        ),
        shares = c(
          setNames("quintile", i18n("inequality.quintile_shares", lang_val)),
          setNames("decile", i18n("inequality.decile_shares", lang_val))
        ),
        lorenz = c(
          setNames("lorenz_single", i18n("inequality.lorenz", lang_val)),
          setNames("lorenz_compare", i18n("inequality.compare_periods", lang_val))
        ),
        decomposition = c(
          setNames("gini_by_source", i18n("inequality.gini_by_source", lang_val)),
          setNames("income_shares_by_source", i18n("inequality.income_shares_by_source", lang_val))
        )
      )
    })

    output$measure_selector <- renderUI({
      selectInput(ns("measure"), i18n("inequality.measure", lang()),
                  choices = measure_choices(),
                  selected = isolate(input$measure))
    })

    # ====================================================================
    # Breakdown selector
    # ====================================================================

    breakdown_choices <- reactive({
      lang_val <- lang()
      theme <- input$theme
      measure <- input$measure

      # No breakdown for decomposition or lorenz_compare
      if (!is.null(theme) && theme == "decomposition") return(NULL)
      if (!is.null(theme) && theme == "lorenz" &&
          !is.null(measure) && measure == "lorenz_compare") return(NULL)
      # No breakdown for shares (stacked area doesn't combine well with groups)
      if (!is.null(theme) && theme == "shares") return(NULL)

      all_choices <- c(
        setNames("overall", i18n("demographics.overall", lang_val)),
        setNames("sex", i18n("demographics.by_sex", lang_val)),
        setNames("race", i18n("demographics.by_race", lang_val)),
        setNames("education", i18n("demographics.by_education", lang_val)),
        setNames("region", i18n("demographics.by_region", lang_val)),
        setNames("uf", i18n("demographics.by_uf", lang_val)),
        setNames("urban_rural", i18n("demographics.by_urban_rural", lang_val)),
        setNames("age_group", i18n("demographics.by_age_group", lang_val))
      )

      # Lorenz single: only overall, race, region available
      if (!is.null(theme) && theme == "lorenz") {
        all_choices <- all_choices[all_choices %in% c("overall", "race", "region")]
      }

      all_choices
    })

    output$breakdown_selector <- renderUI({
      choices <- breakdown_choices()
      if (is.null(choices)) return(NULL)

      selectInput(ns("breakdown"), i18n("demographics.breakdown", lang()),
                  choices = choices,
                  selected = isolate(input$breakdown) %||% "overall")
    })

    # ====================================================================
    # Group filter (conditional multi-select for non-overall breakdowns)
    # ====================================================================

    output$group_filter_ui <- renderUI({
      # No group filter when breakdown selector is hidden
      if (is.null(breakdown_choices())) return(NULL)

      breakdown <- input$breakdown
      if (is.null(breakdown) || breakdown == "overall") return(NULL)

      dt <- ineq_data()
      if (is.null(dt)) return(NULL)

      groups <- sort(unique(dt[breakdown_type == breakdown, breakdown_value]))
      if (length(groups) == 0) return(NULL)

      defaults <- get_default_groups(breakdown, groups)

      selectInput(ns("group_filter"),
                  i18n("demographics.select_groups", lang()),
                  choices = groups, selected = defaults,
                  multiple = TRUE, selectize = TRUE)
    })

    # ====================================================================
    # Date range
    # ====================================================================

    date_range <- reactive({
      dt <- ineq_data()
      if (is.null(dt) || nrow(dt) == 0) return(NULL)
      range(dt$period, na.rm = TRUE)
    })

    output$date_range_ui <- renderUI({
      theme <- input$theme
      # No date slider for lorenz or decomposition (use period picker instead)
      if (!is.null(theme) && theme %in% c("lorenz", "decomposition")) return(NULL)

      dr <- date_range()
      if (is.null(dr)) return(NULL)

      sliderInput(ns("date_slider"), i18n("controls.date_range", lang()),
                  min = dr[1], max = dr[2],
                  value = dr,
                  timeFormat = "%b %Y",
                  step = 30)
    })

    # ====================================================================
    # Period picker (for lorenz single / decomposition)
    # ====================================================================

    output$period_picker_ui <- renderUI({
      theme <- input$theme
      measure <- input$measure

      show_picker <- FALSE
      if (!is.null(theme) && theme == "decomposition") show_picker <- TRUE
      if (!is.null(theme) && theme == "lorenz" &&
          !is.null(measure) && measure == "lorenz_single") show_picker <- TRUE

      if (!show_picker) return(NULL)

      # Get available months from the appropriate data source
      if (theme == "lorenz") {
        ldt <- lorenz_data_all()
        if (is.null(ldt) || nrow(ldt) == 0) return(NULL)
        yms <- sort(unique(ldt$ref_month_yyyymm))
      } else {
        ddt <- decomp_data()
        if (is.null(ddt) || nrow(ddt) == 0) return(NULL)
        yms <- sort(unique(ddt$ref_month_yyyymm))
      }

      dates <- as.Date(paste0(yms %/% 100, "-", sprintf("%02d", yms %% 100), "-15"))
      month_choices <- setNames(as.character(yms), format(dates, "%b %Y"))

      selectInput(ns("period_picker"),
                  i18n("controls_shared.select_period", lang()),
                  choices = month_choices,
                  selected = as.character(yms[length(yms)]))
    })

    # ====================================================================
    # Period comparison UI (for Lorenz)
    # ====================================================================

    output$period_comparison_ui <- renderUI({
      theme <- input$theme
      measure <- input$measure
      if (is.null(theme) || theme != "lorenz") return(NULL)
      if (is.null(measure) || measure != "lorenz_compare") return(NULL)

      dr <- date_range()
      if (is.null(dr)) return(NULL)

      months <- sort(unique(ineq_data()$period))
      month_choices <- setNames(as.character(months), format(months, "%b %Y"))

      tagList(
        selectInput(ns("period1"), i18n("inequality.period_before", lang()),
                    choices = month_choices,
                    selected = as.character(months[1])),
        selectInput(ns("period2"), i18n("inequality.period_after", lang()),
                    choices = month_choices,
                    selected = as.character(months[length(months)]))
      )
    })

    # ====================================================================
    # Display options
    # ====================================================================

    output$display_options_ui <- renderUI({
      lang_val <- lang()
      tagList(
        checkboxInput(ns("show_table"), i18n("inequality.show_table", lang_val),
                      value = FALSE)
      )
    })

    # ====================================================================
    # Info banner
    # ====================================================================

    output$info_banner <- renderText({
      i18n("inequality.info_banner", lang())
    })

    output$learn_more_link <- renderText({
      if (lang() == "en") "Learn more" else "Saiba mais"
    })

    # ====================================================================
    # Filtered data
    # ====================================================================

    filtered_data <- reactive({
      dt <- ineq_data()
      if (is.null(dt) || nrow(dt) == 0) return(NULL)

      theme <- input$theme
      sel_measure <- input$measure
      breakdown <- input$breakdown
      if (is.null(theme) || is.null(breakdown)) return(NULL)

      # Filter by breakdown
      sub <- dt[breakdown_type == breakdown]

      # Filter by selected groups
      if (breakdown != "overall" &&
          !is.null(input$group_filter) && length(input$group_filter) > 0) {
        sub <- sub[breakdown_value %in% input$group_filter]
      }

      # Filter by measure (for time series themes)
      if (theme %in% c("income_level", "distribution") && !is.null(sel_measure)) {
        sub <- sub[measure == sel_measure]
      }

      # Filter by date range
      if (!is.null(input$date_slider)) {
        sub <- sub[period >= input$date_slider[1] & period <= input$date_slider[2]]
      }

      sub
    })

    # ====================================================================
    # Main plot title
    # ====================================================================

    output$main_plot_title <- renderText({
      lang_val <- lang()
      theme <- input$theme
      measure <- input$measure
      if (is.null(theme)) return(i18n("inequality.title", lang_val))

      mc <- measure_choices()
      measure_label <- names(mc)[mc == measure]
      if (length(measure_label) == 0) measure_label <- i18n("inequality.title", lang_val)

      measure_label
    })

    # ====================================================================
    # Main plot
    # ====================================================================

    output$main_plot <- renderPlotly({
      theme <- input$theme
      measure <- input$measure
      breakdown <- input$breakdown
      lang_val <- lang()

      if (is.null(theme)) return(plot_ly())

      # ---- SHARES: stacked area ----
      if (theme == "shares") {
        sdt <- shares_data()
        if (is.null(sdt) || nrow(sdt) == 0) return(plot_ly())

        sel_group_type <- if (!is.null(measure) && measure == "decile") "decile" else "quintile"
        # Shares always uses overall (breakdown hidden for this theme)
        sdt_sub <- sdt[group_type == sel_group_type &
                          breakdown_type == "overall"]

        if (!is.null(input$date_slider)) {
          sdt_sub <- sdt_sub[period >= input$date_slider[1] &
                               period <= input$date_slider[2]]
        }

        if (nrow(sdt_sub) == 0) return(plot_ly())

        # Stacked area
        p <- plot_ly()
        groups <- unique(sdt_sub$group_label)
        colors <- if (sel_group_type == "quintile") {
          c("#2166ac", "#67a9cf", "#d1e5f0", "#fddbc7", "#ef8a62")
        } else {
          grDevices::colorRampPalette(c("#2166ac", "#ef8a62"))(10)
        }

        for (i in seq_along(groups)) {
          gdata <- sdt_sub[group_label == groups[i]]
          setorder(gdata, period)
          p <- p %>% add_trace(
            data = gdata,
            x = ~period, y = ~share,
            type = "scatter", mode = "lines",
            stackgroup = "one",
            fillcolor = colors[i],
            line = list(color = colors[i], width = 0.5),
            name = groups[i]
          )
        }

        p <- p %>% layout(
          xaxis = list(title = ""),
          yaxis = list(title = "", tickformat = ".0%",
                       range = c(0, 1)),
          legend = list(orientation = "h", y = -0.15),
          separators = get_plotly_separators(lang_val),
          hovermode = "x unified"
        )

        return(p)
      }

      # ---- LORENZ ----
      if (theme == "lorenz") {
        ldt <- lorenz_data_all()
        if (is.null(ldt) || nrow(ldt) == 0) return(plot_ly())

        if (!is.null(measure) && measure == "lorenz_compare") {
          # Compare two periods
          p1_date <- input$period1
          p2_date <- input$period2
          if (is.null(p1_date) || is.null(p2_date)) return(plot_ly())

          p1_ym <- as.numeric(format(as.Date(p1_date), "%Y%m"))
          p2_ym <- as.numeric(format(as.Date(p2_date), "%Y%m"))

          ldt1 <- ldt[ref_month_yyyymm == p1_ym & breakdown_type == "overall"]
          ldt2 <- ldt[ref_month_yyyymm == p2_ym & breakdown_type == "overall"]

          p <- plot_ly() %>%
            add_trace(x = c(0, 1), y = c(0, 1), type = "scatter", mode = "lines",
                      line = list(dash = "dash", color = "gray"),
                      name = i18n("inequality.equality_line", lang_val),
                      showlegend = TRUE)

          if (nrow(ldt1) > 0) {
            p <- p %>% add_trace(data = ldt1, x = ~p, y = ~lorenz,
                                  type = "scatter", mode = "lines",
                                  line = list(color = "#1976D2", width = 2),
                                  name = format(as.Date(p1_date), "%b %Y"))
          }
          if (nrow(ldt2) > 0) {
            p <- p %>% add_trace(data = ldt2, x = ~p, y = ~lorenz,
                                  type = "scatter", mode = "lines",
                                  line = list(color = "#E53935", width = 2),
                                  name = format(as.Date(p2_date), "%b %Y"))
          }
        } else {
          # Single period (user-selected or latest)
          months <- sort(unique(ldt$ref_month_yyyymm))
          selected_ym <- if (!is.null(input$period_picker)) {
            as.numeric(input$period_picker)
          } else {
            months[length(months)]
          }

          # Lorenz only has overall, race, region breakdowns
          breakdown_eff <- breakdown %||% "overall"
          if (!breakdown_eff %in% c("overall", "race", "region")) {
            breakdown_eff <- "overall"
          }

          ldt_sub <- ldt[ref_month_yyyymm == selected_ym &
                           breakdown_type == breakdown_eff]

          # Apply group filter for Lorenz too
          if (breakdown_eff != "overall" &&
              !is.null(input$group_filter) && length(input$group_filter) > 0) {
            ldt_sub <- ldt_sub[breakdown_value %in% input$group_filter]
          }

          if (breakdown_eff != "overall") {
            # Multiple lines by group
            p <- plot_ly() %>%
              add_trace(x = c(0, 1), y = c(0, 1), type = "scatter", mode = "lines",
                        line = list(dash = "dash", color = "gray"),
                        name = i18n("inequality.equality_line", lang_val))

            groups <- unique(ldt_sub$breakdown_value)
            colors <- grDevices::colorRampPalette(
              c("#1976D2", "#E53935", "#00ACC1", "#FF7043", "#4CAF50")
            )(length(groups))

            for (i in seq_along(groups)) {
              gdata <- ldt_sub[breakdown_value == groups[i]]
              p <- p %>% add_trace(data = gdata, x = ~p, y = ~lorenz,
                                    type = "scatter", mode = "lines",
                                    line = list(color = colors[i], width = 2),
                                    name = groups[i])
            }
          } else {
            p <- plot_ly() %>%
              add_trace(x = c(0, 1), y = c(0, 1), type = "scatter", mode = "lines",
                        line = list(dash = "dash", color = "gray"),
                        name = i18n("inequality.equality_line", lang_val)) %>%
              add_trace(data = ldt_sub, x = ~p, y = ~lorenz,
                        type = "scatter", mode = "lines",
                        line = list(color = "#1976D2", width = 2),
                        name = format(as.Date(paste0(selected_ym %/% 100, "-",
                                                      sprintf("%02d", selected_ym %% 100),
                                                      "-15")),
                                       "%b %Y"))
          }
        }

        p <- p %>% layout(
          xaxis = list(title = i18n("inequality.cumulative_pop", lang_val),
                       range = c(0, 1), constrain = "domain"),
          yaxis = list(title = i18n("inequality.cumulative_income", lang_val),
                       range = c(0, 1), scaleanchor = "x", scaleratio = 1),
          legend = list(orientation = "h", y = -0.15),
          separators = get_plotly_separators(lang_val)
        )

        return(p)
      }

      # ---- DECOMPOSITION ----
      if (theme == "decomposition") {
        ddt <- decomp_data()
        if (is.null(ddt) || nrow(ddt) == 0) return(plot_ly())

        # Use selected period or latest
        selected_ym <- if (!is.null(input$period_picker)) {
          as.numeric(input$period_picker)
        } else {
          max(ddt$ref_month_yyyymm)
        }
        ddt_sub <- ddt[ref_month_yyyymm == selected_ym]

        if (nrow(ddt_sub) == 0) return(plot_ly())

        # Map source labels
        source_labels <- sapply(ddt_sub$income_source, function(s) {
          key <- paste0("inequality.source_", s)
          i18n(key, lang_val)
        })

        y_var <- if (!is.null(measure) && measure == "income_shares_by_source") {
          "income_share"
        } else {
          "contribution_to_gini"
        }

        y_title <- if (y_var == "income_share") {
          i18n("inequality.income_share", lang_val)
        } else {
          i18n("inequality.contribution", lang_val)
        }

        colors <- c("#1976D2", "#FF7043", "#00ACC1", "#4CAF50",
                     "#9C27B0", "#FFC107", "#795548", "#607D8B")

        p <- plot_ly(
          data = ddt_sub,
          x = ~reorder(source_labels, -get(y_var)),
          y = ~get(y_var),
          type = "bar",
          marker = list(color = colors[seq_len(nrow(ddt_sub))])
        ) %>% layout(
          xaxis = list(title = ""),
          yaxis = list(title = y_title, tickformat = ".1%"),
          separators = get_plotly_separators(lang_val),
          showlegend = FALSE
        )

        return(p)
      }

      # ---- TIME SERIES (income_level, distribution) ----
      sub <- filtered_data()
      if (is.null(sub) || nrow(sub) == 0) return(plot_ly())

      if (breakdown == "overall") {
        # Single line
        setorder(sub, period)
        p <- plot_ly(data = sub, x = ~period, y = ~value,
                     type = "scatter", mode = "lines+markers",
                     line = list(color = "#1976D2", width = 2),
                     marker = list(size = 3, color = "#1976D2"),
                     name = i18n("demographics.nacional", lang_val))
      } else {
        # Multiple lines by group
        groups <- unique(sub$breakdown_value)
        colors <- grDevices::colorRampPalette(
          c("#1976D2", "#E53935", "#00ACC1", "#FF7043", "#4CAF50",
            "#9C27B0", "#FFC107", "#795548")
        )(length(groups))

        p <- plot_ly()
        for (i in seq_along(groups)) {
          gdata <- sub[breakdown_value == groups[i]]
          setorder(gdata, period)
          p <- p %>% add_trace(
            data = gdata, x = ~period, y = ~value,
            type = "scatter", mode = "lines",
            line = list(color = colors[i], width = 2),
            name = groups[i]
          )
        }
      }

      # Format y-axis based on measure
      yformat <- if (!is.null(measure) && measure %in%
                     c("top1_share", "top5_share", "top10_share", "bottom50_share")) {
        ".1%"
      } else if (!is.null(measure) && measure %in% c("mean_income", "median_income")) {
        ",.0f"
      } else {
        ",.3f"
      }

      # Adaptive legend: vertical for many groups, horizontal for few
      n_groups <- length(unique(sub$breakdown_value))
      legend_config <- if (n_groups > 6) {
        list(orientation = "v", x = 1.02, y = 1, font = list(size = 10))
      } else {
        list(orientation = "h", y = -0.15)
      }

      p <- p %>% layout(
        xaxis = list(title = ""),
        yaxis = list(title = "", tickformat = yformat),
        legend = legend_config,
        separators = get_plotly_separators(lang_val),
        hovermode = "x unified"
      )

      p
    })

    # ====================================================================
    # Secondary plot (conditional)
    # ====================================================================

    output$secondary_plot_ui <- renderUI({
      # For now, no secondary plot
      NULL
    })

    # ====================================================================
    # Stat cards
    # ====================================================================

    output$stat_cards_ui <- renderUI({
      theme <- input$theme
      if (is.null(theme) || theme %in% c("lorenz", "shares", "decomposition")) {
        return(NULL)
      }

      sub <- filtered_data()
      if (is.null(sub) || nrow(sub) == 0) return(NULL)

      lang_val <- lang()
      measure <- input$measure
      breakdown <- input$breakdown

      # For overall, compute stats
      if (breakdown == "overall") {
        vals <- sub$value
        vals <- vals[!is.na(vals)]

        if (length(vals) == 0) return(NULL)

        latest_val <- vals[length(vals)]
        min_val <- min(vals)
        max_val <- max(vals)
        mean_val <- mean(vals)

        # YoY
        if (nrow(sub) > 12) {
          yoy <- (vals[length(vals)] - vals[length(vals) - 12]) /
            abs(vals[length(vals) - 12]) * 100
        } else {
          yoy <- NA
        }

        # Format
        is_pct <- measure %in% c("top1_share", "top5_share", "top10_share",
                                   "bottom50_share")
        is_currency <- measure %in% c("mean_income", "median_income")

        fmt <- function(x) {
          if (is.na(x)) return(i18n("stats.na", lang_val))
          if (is_pct) sprintf("%.1f%%", x * 100)
          else if (is_currency) format_series_value(x, "currency", lang_val)
          else format_number_i18n(x, digits = 3, lang = lang_val)
        }

        tags$div(
          class = "row g-2 mt-3",
          tags$div(class = "col",
            tags$div(class = "stat-card stat-card-highlight text-center p-2",
              tags$small(i18n("stats.latest", lang_val)),
              tags$div(class = "fw-bold", fmt(latest_val)))),
          tags$div(class = "col",
            tags$div(class = "stat-card text-center p-2",
              tags$small(i18n("stats.min", lang_val)),
              tags$div(class = "fw-bold", fmt(min_val)))),
          tags$div(class = "col",
            tags$div(class = "stat-card text-center p-2",
              tags$small(i18n("stats.max", lang_val)),
              tags$div(class = "fw-bold", fmt(max_val)))),
          tags$div(class = "col",
            tags$div(class = "stat-card text-center p-2",
              tags$small(i18n("stats.mean", lang_val)),
              tags$div(class = "fw-bold", fmt(mean_val)))),
          tags$div(class = "col",
            tags$div(class = "stat-card text-center p-2",
              tags$small(i18n("stats.yoy_change", lang_val)),
              tags$div(class = "fw-bold",
                if (!is.na(yoy)) sprintf("%+.1f%%", yoy) else i18n("stats.na", lang_val))))
        )
      }
    })

    # ====================================================================
    # Data table
    # ====================================================================

    output$data_table_ui <- renderUI({
      if (!isTRUE(input$show_table)) return(NULL)

      card(
        card_body(DTOutput(ns("data_table")))
      )
    })

    output$data_table <- renderDT({
      sub <- filtered_data()
      if (is.null(sub) || nrow(sub) == 0) return(NULL)

      display <- sub[, .(
        Period = format(period, "%b %Y"),
        Group = breakdown_value,
        Measure = measure,
        Value = round(value, 4),
        `N obs` = n_obs
      )]

      datatable(display, options = list(pageLength = 20, scrollX = TRUE),
                rownames = FALSE)
    })

    # ====================================================================
    # Downloads
    # ====================================================================

    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("inequality_", Sys.Date(), ".csv")
      },
      content = function(file) {
        sub <- filtered_data()
        if (!is.null(sub)) {
          fwrite(sub, file)
        }
      }
    )

    output$download_png <- downloadHandler(
      filename = function() {
        paste0("inequality_", Sys.Date(), ".png")
      },
      content = function(file) {
        grDevices::png(file, width = 1200, height = 600, res = 150)
        plot.new()
        title("Export not available - use plotly download button")
        grDevices::dev.off()
      }
    )

    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("inequality_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        sub <- filtered_data()
        if (!is.null(sub) && requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(as.data.frame(sub), file)
        } else if (!is.null(sub)) {
          fwrite(sub, file)
        }
      }
    )
  })
}
