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

      # Data freshness — replicates mod_geographic.R / mod_series_explorer.R
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
          )
        )
      ),

      # Income variable selector (top-level, Phase 2-3)
      uiOutput(ns("income_var_selector")),

      # Theme selector (analysis type — depends on income_var)
      uiOutput(ns("theme_selector")),

      # Measure selector (dynamic based on income_var × theme)
      uiOutput(ns("measure_selector")),

      # Breakdown selector
      uiOutput(ns("breakdown_selector")),

      # Group filter (conditional, for non-overall breakdowns)
      uiOutput(ns("group_filter_ui")),

      # Date range slider (time series themes only)
      uiOutput(ns("date_range_ui")),

      # Seasonal adjustment toggle (time series themes only)
      uiOutput(ns("deseason_ui")),

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

      # Difference plot (conditional: only when show_difference is checked)
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("show_difference")),
        card(
          card_header(textOutput(ns("difference_plot_title"), inline = TRUE)),
          card_body(
            plotlyOutput(ns("difference_plot"), height = "200px")
          )
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
    # Phase 2-3: income_var taxonomy
    # ====================================================================

    # 4 hh income_vars come from the annual stack (inequality_data),
    # 4 indiv income_vars come from the quarterly mensalized stack
    # (quarterly_income_data). The two assets share the same long-format
    # schema (ref_month_yyyymm, measure, value, n_obs, breakdown_type,
    # breakdown_value, income_var, period [, value_x13, value_stl]) so
    # the rest of the module reads them transparently.
    HH_INCOME_VARS    <- c("hh_total_efe", "hh_pc_efe",
                            "hh_total_hab", "hh_pc_hab")
    INDIV_INCOME_VARS <- c("indiv_hab_princ", "indiv_efe_princ",
                            "indiv_hab_todos", "indiv_efe_todos")
    DEFAULT_INCOME_VAR <- "hh_pc_hab"

    # Map income_var -> decomposition kind (for `theme = decomposition`).
    # hh_*_efe -> efetiva ; hh_*_hab -> habitual. indiv_* has no decomp.
    decomp_kind_for <- function(iv) {
      if (is.null(iv)) return(NA_character_)
      if (grepl("_efe$", iv)) return("efetiva")
      if (grepl("_hab$", iv)) return("habitual")
      NA_character_
    }

    # ====================================================================
    # Reactive: Available data
    # ====================================================================

    annual_ineq_data <- reactive({
      shared_data$get_inequality_data()
    })

    quarterly_income_data <- reactive({
      if (is.function(shared_data$get_quarterly_income_data)) {
        tryCatch(shared_data$get_quarterly_income_data(),
                 error = function(e) NULL)
      } else {
        NULL
      }
    })

    # Routing reactive: returns the long-format data.table for the
    # currently selected income_var. hh_* -> annual (inequality_data),
    # indiv_* -> quarterly_income_data.
    ineq_data <- reactive({
      iv <- input$income_var %||% DEFAULT_INCOME_VAR
      if (iv %in% INDIV_INCOME_VARS) {
        d <- quarterly_income_data()
        if (!is.null(d) && nrow(d) > 0) return(d)
      }
      annual_ineq_data()
    })

    shares_data <- reactive({
      shared_data$get_income_shares_data()
    })

    lorenz_data_all <- reactive({
      shared_data$get_lorenz_data()
    })

    decomp_data <- reactive({
      shared_data$get_income_decomposition_data()
    })

    # ====================================================================
    # Data freshness badge
    # ====================================================================

    output$label_last_updated <- renderText({
      i18n("messages.last_updated", lang())
    })

    output$last_updated <- renderText({
      if (!is.null(shared_data$ineq_last_updated)) {
        paste0(format(shared_data$ineq_last_updated, "%Y-%m-%d %H:%M",
                      tz = "America/Sao_Paulo"), " BRT")
      } else {
        i18n("messages.not_available", lang())
      }
    })

    # ====================================================================
    # Income variable selector (Phase 2-3 — top-level)
    # ====================================================================

    income_var_choices <- reactive({
      lang_val <- lang()
      hh_label    <- i18n("inequality.income_var_group_hh", lang_val)
      indiv_label <- i18n("inequality.income_var_group_indiv", lang_val)
      hh_codes    <- HH_INCOME_VARS
      indiv_codes <- INDIV_INCOME_VARS
      hh_labels    <- vapply(hh_codes,
        function(c) i18n(paste0("inequality.income_var_", c), lang_val),
        character(1))
      indiv_labels <- vapply(indiv_codes,
        function(c) i18n(paste0("inequality.income_var_", c), lang_val),
        character(1))
      list(
        setNames(list(setNames(hh_codes, hh_labels)), hh_label),
        setNames(list(setNames(indiv_codes, indiv_labels)), indiv_label)
      )
    })

    output$income_var_selector <- renderUI({
      lang_val <- lang()
      groups <- income_var_choices()
      # selectInput with grouped options (Domiciliares / Individuais).
      # `choices` accepts a named list of named vectors which Shiny renders
      # as <optgroup> entries.
      selectInput(
        ns("income_var"),
        i18n("inequality.income_var", lang_val),
        choices = c(groups[[1]], groups[[2]]),
        selected = isolate(input$income_var) %||% DEFAULT_INCOME_VAR
      )
    })

    # ====================================================================
    # Theme choices (Phase 2-3: shares folded into income_level)
    # ====================================================================

    theme_choices <- reactive({
      lang_val <- lang()
      iv <- input$income_var %||% DEFAULT_INCOME_VAR
      # 4 hh income variants have full coverage (income_level, indices,
      # lorenz, decomposition). 4 indiv variants currently only have
      # mean/median in the pipeline, so we expose just income_level.
      # Phase 2-6 will extend the indiv pipeline; until then the other
      # themes are hidden to avoid empty plots.
      if (iv %in% INDIV_INCOME_VARS) {
        return(c(
          setNames("income_level",
                   i18n("inequality.theme_income_level", lang_val))
        ))
      }
      base <- c(
        setNames("income_level", i18n("inequality.theme_income_level", lang_val)),
        setNames("inequality_indexes", i18n("inequality.theme_inequality_indexes", lang_val)),
        setNames("lorenz", i18n("inequality.theme_lorenz", lang_val))
      )
      # Decomposição só está disponível para as 4 vars domiciliares
      if (iv %in% HH_INCOME_VARS) {
        base <- c(
          base,
          setNames("decomposition",
                   i18n("inequality.theme_decomposition", lang_val))
        )
      }
      base
    })

    output$theme_selector <- renderUI({
      selectInput(ns("theme"), i18n("inequality.theme", lang()),
                  choices = theme_choices(),
                  selected = isolate(input$theme) %||% "inequality_indexes")
    })

    # ====================================================================
    # Measure choices (depend on theme)
    # ====================================================================

    measure_choices <- reactive({
      lang_val <- lang()
      theme <- input$theme
      if (is.null(theme)) theme <- "inequality_indexes"

      iv <- input$income_var %||% DEFAULT_INCOME_VAR

      switch(theme,
        # Phase 2-3 + 2-4: income_level lists statistical measures
        # (mean, min, P10, P25, median, P75, P90, max — ordered by
        # ascending percentile so the dropdown reads naturally), plus
        # the income shares (quintile/decile) folded from the old
        # "shares" theme. Indiv income variants currently expose only
        # mean and median (Phase 2-6 will extend the pipeline).
        income_level = if (iv %in% INDIV_INCOME_VARS) {
          c(
            setNames("mean",   i18n("inequality.mean",   lang_val)),
            setNames("median", i18n("inequality.median", lang_val))
          )
        } else {
          c(
            setNames("mean",     i18n("inequality.mean",     lang_val)),
            setNames("min",      i18n("inequality.min",      lang_val)),
            setNames("p10",      i18n("inequality.p10",      lang_val)),
            setNames("p25",      i18n("inequality.p25",      lang_val)),
            setNames("median",   i18n("inequality.median",   lang_val)),
            setNames("p75",      i18n("inequality.p75",      lang_val)),
            setNames("p90",      i18n("inequality.p90",      lang_val)),
            setNames("max",      i18n("inequality.max",      lang_val)),
            setNames("quintile",
                     i18n("inequality.quintile_shares", lang_val)),
            setNames("decile",
                     i18n("inequality.decile_shares", lang_val))
          )
        },
        inequality_indexes = c(
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
      theme <- input$theme
      # Use "View" label for Lorenz instead of "Measure"
      label <- if (!is.null(theme) && theme == "lorenz") {
        i18n("inequality.lorenz_view", lang())
      } else {
        i18n("inequality.measure", lang())
      }
      selectInput(ns("measure"), label,
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

    # Debounced date slider (prevents 100+ re-renders during drag)
    date_slider_debounced <- reactive({
      input$date_slider
    }) %>% debounce(300)

    # ====================================================================
    # Seasonal adjustment toggle (only for time-series themes; only when
    # the precomputed value_x13 / value_stl columns are present in the
    # loaded data.)
    # ====================================================================

    output$deseason_ui <- renderUI({
      theme <- input$theme
      if (is.null(theme) || !(theme %in% c("income_level", "inequality_indexes"))) {
        return(NULL)
      }
      d <- ineq_data()
      if (is.null(d) || !all(c("value_x13", "value_stl") %in% names(d))) {
        return(NULL)
      }
      lang_val <- lang()
      radioButtons(
        ns("deseason"),
        i18n("controls.deseasonalization", lang_val),
        choices = c(
          setNames("none", i18n("controls.none", lang_val)),
          setNames("x13",  i18n("controls.x13_arima", lang_val)),
          setNames("stl",  i18n("controls.stl", lang_val)),
          setNames("both", i18n("controls.compare_both", lang_val))
        ),
        selected = isolate(input$deseason) %||% "none",
        inline = FALSE
      )
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

      lang_val <- lang()
      month_choices <- setNames(
        as.character(yms),
        vapply(yms, function(ym) format_date_i18n(ym, lang_val, "short"), character(1))
      )

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
      lang_val <- lang()
      month_choices <- setNames(
        as.character(months),
        vapply(months, function(m) format_date_i18n(m, lang_val, "short"), character(1))
      )

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
      theme <- input$theme
      is_time_series <- !is.null(theme) &&
        theme %in% c("income_level", "inequality_indexes")
      tagList(
        if (is_time_series) {
          tagList(
            checkboxInput(
              ns("show_quarterly"),
              i18n("controls.show_quarterly", lang_val),
              value = FALSE
            ),
            checkboxInput(
              ns("show_difference"),
              i18n("controls.show_difference", lang_val),
              value = FALSE
            )
          )
        },
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
      iv <- input$income_var %||% DEFAULT_INCOME_VAR
      if (is.null(theme) || is.null(breakdown)) return(NULL)

      # Phase 2-3: filter by income_var (column present in both annual
      # and quarterly assets after the Phase 2-2 schema change).
      if ("income_var" %in% names(dt)) {
        dt <- dt[income_var == iv]
      }
      if (!nrow(dt)) return(NULL)

      # Filter by breakdown
      sub <- dt[breakdown_type == breakdown]

      # Filter by selected groups
      if (breakdown != "overall" &&
          !is.null(input$group_filter) && length(input$group_filter) > 0) {
        sub <- sub[breakdown_value %in% input$group_filter]
      }

      # Filter by measure (for time series themes). When the user picks
      # "quintile" or "decile" inside income_level, the shares branch
      # of the plot reactives reads income_shares_data instead — this
      # filter is a no-op there.
      if (theme %in% c("income_level", "inequality_indexes") &&
          !is.null(sel_measure) &&
          !sel_measure %in% c("quintile", "decile")) {
        sub <- sub[measure == sel_measure]
      }

      # Filter by date range (debounced)
      dr <- date_slider_debounced()
      if (!is.null(dr)) {
        sub <- sub[period >= dr[1] & period <= dr[2]]
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

      # ---- SHARES: stacked area (Phase 2-3: folded into income_level
      # when measure is quintile or decile) ----
      shares_active <- (!is.null(theme) && theme == "income_level" &&
                       !is.null(measure) && measure %in% c("quintile", "decile"))
      if (shares_active) {
        sdt <- shares_data()
        if (is.null(sdt) || nrow(sdt) == 0) return(plot_ly())

        # Filter by income_var (Phase 2-3: shares now carry the
        # income_var dimension so the same theme can show parcelas for
        # any of the 4 hh income variants).
        iv <- input$income_var %||% DEFAULT_INCOME_VAR
        if ("income_var" %in% names(sdt)) {
          sdt <- sdt[income_var == iv]
        }

        sel_group_type <- if (!is.null(measure) && measure == "decile") "decile" else "quintile"
        # Shares always uses overall (breakdown hidden for this theme)
        sdt_sub <- sdt[group_type == sel_group_type &
                          breakdown_type == "overall"]

        dr <- date_slider_debounced()
        if (!is.null(dr)) {
          sdt_sub <- sdt_sub[period >= dr[1] & period <= dr[2]]
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

        # Phase 2-3: filter Lorenz by selected income_var (annual asset
        # — only the 4 hh variants are available in lorenz_data.rds)
        iv <- input$income_var %||% DEFAULT_INCOME_VAR
        if ("income_var" %in% names(ldt) && iv %in% HH_INCOME_VARS) {
          ldt <- ldt[income_var == iv]
        }
        if (!nrow(ldt)) return(plot_ly())

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
                                  name = format_date_i18n(as.Date(p1_date), lang_val, "short"))
          }
          if (nrow(ldt2) > 0) {
            p <- p %>% add_trace(data = ldt2, x = ~p, y = ~lorenz,
                                  type = "scatter", mode = "lines",
                                  line = list(color = "#E53935", width = 2),
                                  name = format_date_i18n(as.Date(p2_date), lang_val, "short"))
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
                        name = format_date_i18n(selected_ym, lang_val, "short"))
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

        # Phase 2-3: pick the decomposition kind based on the user's
        # income_var (hh_*_efe -> efetiva ; hh_*_hab -> habitual).
        iv <- input$income_var %||% DEFAULT_INCOME_VAR
        kind <- decomp_kind_for(iv)
        if ("decomp_kind" %in% names(ddt) && !is.na(kind)) {
          ddt <- ddt[decomp_kind == kind]
        }
        if (!nrow(ddt)) return(plot_ly())

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

      # ---- TIME SERIES (income_level, inequality_indexes) ----
      sub <- filtered_data()
      if (is.null(sub) || nrow(sub) == 0) return(plot_ly())
      if (is.null(breakdown)) return(plot_ly())

      # Seasonal adjustment method (none / x13 / stl / both). Mirrors the
      # SIDRA Series Explorer pattern: the original series is shown as a
      # faded dotted overlay when an adjustment is selected.
      deseason <- input$deseason %||% "none"
      has_x13  <- "value_x13" %in% names(sub)
      has_stl  <- "value_stl" %in% names(sub)
      if (!has_x13 && !has_stl) deseason <- "none"

      if (breakdown == "overall") {
        setorder(sub, period)
        nacional <- i18n("demographics.nacional", lang_val)
        p <- plot_ly()

        if (deseason == "none") {
          p <- p %>% add_trace(
            data = sub, x = ~period, y = ~value,
            type = "scatter", mode = "lines+markers",
            line = list(color = "#1976D2", width = 2),
            marker = list(size = 3, color = "#1976D2"),
            name = nacional
          )
        } else {
          # Original (faded background)
          orig_color <- if (deseason == "both") "#9E9E9E" else "#9E9E9E"
          p <- p %>% add_trace(
            data = sub, x = ~period, y = ~value,
            type = "scatter", mode = "lines",
            line = list(color = orig_color, width = 1.5, dash = "dot"),
            opacity = 0.7,
            name = paste0(nacional, " - ",
                           i18n("plots.original", lang_val))
          )
          if (deseason %in% c("x13", "both") && has_x13) {
            p <- p %>% add_trace(
              data = sub, x = ~period, y = ~value_x13,
              type = "scatter", mode = "lines",
              line = list(color = "#1976D2", width = 2),
              name = paste0(nacional, " - X-13")
            )
          }
          if (deseason %in% c("stl", "both") && has_stl) {
            stl_color <- if (deseason == "both") "#00ACC1" else "#1976D2"
            p <- p %>% add_trace(
              data = sub, x = ~period, y = ~value_stl,
              type = "scatter", mode = "lines",
              line = list(color = stl_color, width = 2),
              name = paste0(nacional, " - STL")
            )
          }
        }
      } else {
        # Multiple groups: per-group color, with raw faded + adjusted solid
        groups <- unique(sub$breakdown_value)
        colors <- grDevices::colorRampPalette(
          c("#1976D2", "#E53935", "#00ACC1", "#FF7043", "#4CAF50",
            "#9C27B0", "#FFC107", "#795548")
        )(length(groups))

        p <- plot_ly()
        for (i in seq_along(groups)) {
          gdata <- sub[breakdown_value == groups[i]]
          setorder(gdata, period)
          gcol <- colors[i]
          if (deseason == "none") {
            p <- p %>% add_trace(
              data = gdata, x = ~period, y = ~value,
              type = "scatter", mode = "lines",
              line = list(color = gcol, width = 2),
              name = groups[i]
            )
          } else {
            # Original faded
            p <- p %>% add_trace(
              data = gdata, x = ~period, y = ~value,
              type = "scatter", mode = "lines",
              line = list(color = gcol, width = 1.2, dash = "dot"),
              opacity = 0.45,
              showlegend = FALSE,
              name = paste0(groups[i], " - ",
                             i18n("plots.original", lang_val))
            )
            if (deseason %in% c("x13", "both") && has_x13) {
              p <- p %>% add_trace(
                data = gdata, x = ~period, y = ~value_x13,
                type = "scatter", mode = "lines",
                line = list(color = gcol, width = 2),
                name = if (deseason == "both")
                  paste0(groups[i], " - X-13") else groups[i]
              )
            }
            if (deseason %in% c("stl", "both") && has_stl) {
              p <- p %>% add_trace(
                data = gdata, x = ~period, y = ~value_stl,
                type = "scatter", mode = "lines",
                line = list(color = gcol, width = 2,
                             dash = if (deseason == "both") "dash" else "solid"),
                name = if (deseason == "both")
                  paste0(groups[i], " - STL") else groups[i]
              )
            }
          }
        }
      }

      # Optional 3-month rolling-mean overlay (mirror SIDRA "show quarterly").
      # Mirrors SIDRA: only shown when deseason == "none" (otherwise the
      # adjusted line already supplies the smoothing reference).
      if (isTRUE(input$show_quarterly) && deseason == "none") {
        if (breakdown == "overall") {
          setorder(sub, period)
          rolling <- data.table::frollmean(sub$value, 3, align = "right")
          p <- p %>% add_trace(
            x = sub$period, y = rolling,
            type = "scatter", mode = "lines",
            line = list(color = "#9E9E9E", width = 2, dash = "dash"),
            name = i18n("plots.quarterly", lang_val)
          )
        } else {
          for (i in seq_along(groups)) {
            gdata <- sub[breakdown_value == groups[i]]
            setorder(gdata, period)
            gcol <- colors[i]
            rolling <- data.table::frollmean(gdata$value, 3, align = "right")
            p <- p %>% add_trace(
              x = gdata$period, y = rolling,
              type = "scatter", mode = "lines",
              line = list(color = gcol, width = 1.5, dash = "dash"),
              showlegend = FALSE,
              name = paste0(groups[i], " - ",
                             i18n("plots.quarterly", lang_val))
            )
          }
        }
      }

      # Format y-axis based on theme × measure. Phase 2-3 + 2-4: under
      # the income_level theme any percentile statistic is currency;
      # share-style measures use percent format.
      is_currency_meas <- !is.null(theme) && theme == "income_level" &&
        !is.null(measure) && measure %in% c("mean", "min", "p10", "p25",
                                             "median", "p75", "p90", "max")
      yformat <- if (!is.null(measure) && measure %in%
                     c("top1_share", "top5_share", "top10_share", "bottom50_share")) {
        ".1%"
      } else if (is_currency_meas) {
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
    # Difference plot — adapts to the seasonal-adjustment toggle:
    #   - none: value − rolling-3 mean (deviation from local trend)
    #   - x13:  value − value_x13 (X-13 seasonal component)
    #   - stl:  value − value_stl (STL seasonal component)
    #   - both: overlay both seasonal components
    # Mirrors the SIDRA Series Explorer "Mostrar diferença" pattern.
    # ====================================================================

    output$difference_plot_title <- renderText({
      lang_val <- lang()
      m <- input$deseason %||% "none"
      switch(m,
        x13  = i18n("plots.diff_seasonal_x13", lang_val),
        stl  = i18n("plots.diff_seasonal_stl", lang_val),
        i18n("plots.difference_title", lang_val))
    })

    output$difference_plot <- renderPlotly({
      req(isTRUE(input$show_difference))
      lang_val <- lang()
      sub <- filtered_data()
      validate(need(!is.null(sub) && nrow(sub) > 0, ""))
      breakdown <- input$breakdown
      validate(need(!is.null(breakdown), ""))

      m <- input$deseason %||% "none"
      has_x13 <- "value_x13" %in% names(sub)
      has_stl <- "value_stl" %in% names(sub)
      if (m %in% c("x13", "both") && !has_x13) m <- "none"
      if (m == "stl" && !has_stl) m <- "none"

      # Helper: difference series for one (sub-)data frame.
      compute_diff <- function(df, method) {
        switch(method,
          none = df$value - data.table::frollmean(df$value, 3, align = "right"),
          x13  = df$value - df$value_x13,
          stl  = df$value - df$value_stl
        )
      }

      x13_label <- i18n("plots.seasonal_x13", lang_val)
      stl_label <- i18n("plots.seasonal_stl", lang_val)
      mq_label  <- i18n("plots.monthly_minus_quarterly", lang_val)

      add_diff_traces <- function(p, df, base_color, name_prefix = "",
                                  show_legend_default = TRUE) {
        if (m == "both") {
          if (has_x13) {
            p <- p %>% add_trace(
              x = df$period, y = compute_diff(df, "x13"),
              type = "scatter", mode = "lines",
              line = list(color = base_color, width = 1.5),
              name = paste0(name_prefix, x13_label),
              showlegend = show_legend_default
            )
          }
          if (has_stl) {
            p <- p %>% add_trace(
              x = df$period, y = compute_diff(df, "stl"),
              type = "scatter", mode = "lines",
              line = list(color = base_color, width = 1.5, dash = "dash"),
              name = paste0(name_prefix, stl_label),
              showlegend = show_legend_default
            )
          }
        } else {
          lab <- switch(m,
            none = mq_label,
            x13  = x13_label,
            stl  = stl_label)
          p <- p %>% add_trace(
            x = df$period, y = compute_diff(df, m),
            type = "scatter", mode = "lines",
            line = list(color = base_color, width = 1.5),
            fill = if (breakdown == "overall") "tozeroy" else "none",
            fillcolor = if (breakdown == "overall")
              "rgba(25, 118, 210, 0.15)" else NULL,
            name = paste0(name_prefix, lab),
            showlegend = show_legend_default
          )
        }
        p
      }

      if (breakdown == "overall") {
        setorder(sub, period)
        p <- plot_ly() %>%
          add_diff_traces(sub, "#1976D2",
                           name_prefix = "",
                           show_legend_default = m == "both")
        p %>% layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""),
          separators = get_plotly_separators(lang_val),
          hovermode = "x unified",
          showlegend = m == "both"
        )
      } else {
        groups <- unique(sub$breakdown_value)
        colors <- grDevices::colorRampPalette(
          c("#1976D2", "#E53935", "#00ACC1", "#FF7043", "#4CAF50",
            "#9C27B0", "#FFC107", "#795548")
        )(length(groups))
        p <- plot_ly()
        for (i in seq_along(groups)) {
          gdata <- sub[breakdown_value == groups[i]]
          setorder(gdata, period)
          p <- add_diff_traces(p, gdata, colors[i],
                                name_prefix = paste0(groups[i], " "),
                                show_legend_default = TRUE)
        }
        legend_cfg <- if (length(groups) > 6) {
          list(orientation = "v", x = 1.02, y = 1, font = list(size = 10))
        } else {
          list(orientation = "h", y = -0.15)
        }
        p %>% layout(
          xaxis = list(title = ""),
          yaxis = list(title = ""),
          separators = get_plotly_separators(lang_val),
          hovermode = "x unified",
          legend = legend_cfg
        )
      }
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
      measure <- input$measure
      if (is.null(theme) || theme %in% c("lorenz", "decomposition")) {
        return(NULL)
      }
      # Phase 2-3: hide stat cards in the shares sub-mode of income_level
      if (theme == "income_level" &&
          !is.null(measure) && measure %in% c("quintile", "decile")) {
        return(NULL)
      }

      sub <- filtered_data()
      if (is.null(sub) || nrow(sub) == 0) return(NULL)

      lang_val <- lang()
      measure <- input$measure
      breakdown <- input$breakdown
      if (is.null(breakdown)) return(NULL)

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
        # Phase 2-3 + 2-4: currency for any percentile statistic under
        # the income_level theme (income_var routed upstream).
        is_currency <- !is.null(theme) && theme == "income_level" &&
                       measure %in% c("mean", "min", "p10", "p25",
                                       "median", "p75", "p90", "max")

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

      lang_val <- lang()
      display <- sub[, .(
        Period = vapply(period, function(p) format_date_i18n(p, lang_val, "short"), character(1)),
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
        theme <- input$theme
        lang_val <- lang()

        # Time series themes
        if (theme %in% c("income_level", "inequality_indexes")) {
          sub <- filtered_data()
          if (is.null(sub) || nrow(sub) == 0) return()

          mc <- measure_choices()
          title_label <- names(mc)[mc == input$measure]
          if (length(title_label) == 0) title_label <- ""

          p <- ggplot2::ggplot(sub, ggplot2::aes(x = period, y = value,
                                                   color = breakdown_value)) +
            ggplot2::geom_line(linewidth = 0.8) +
            ggplot2::labs(title = title_label, x = "", y = "", color = "") +
            ggplot2::theme_minimal(base_size = 12) +
            ggplot2::theme(legend.position = "bottom")
          ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 150)

        } else if (theme == "decomposition") {
          ddt <- decomp_data()
          if (is.null(ddt) || nrow(ddt) == 0) return()
          # Phase 2-3: filter by decomp_kind from the chosen income_var
          iv <- input$income_var %||% DEFAULT_INCOME_VAR
          kind <- decomp_kind_for(iv)
          if ("decomp_kind" %in% names(ddt) && !is.na(kind)) {
            ddt <- ddt[decomp_kind == kind]
          }
          if (!nrow(ddt)) return()
          sel_ym <- if (!is.null(input$period_picker)) as.numeric(input$period_picker) else max(ddt$ref_month_yyyymm)
          ddt_sub <- ddt[ref_month_yyyymm == sel_ym]
          if (nrow(ddt_sub) == 0) return()

          y_var <- if (!is.null(input$measure) && input$measure == "income_shares_by_source") "income_share" else "contribution_to_gini"
          ddt_sub[, label := sapply(income_source, function(s) i18n(paste0("inequality.source_", s), lang_val))]

          p <- ggplot2::ggplot(ddt_sub, ggplot2::aes(x = reorder(label, -get(y_var)), y = get(y_var))) +
            ggplot2::geom_col(fill = "#1976D2") +
            ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), "%")) +
            ggplot2::labs(title = format_date_i18n(sel_ym, lang_val, "short"), x = "", y = "") +
            ggplot2::theme_minimal(base_size = 12) +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
          ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 150)

        } else {
          # Fallback for lorenz/shares
          grDevices::png(file, width = 1200, height = 600, res = 150)
          plot.new()
          title(main = "Use the plotly camera icon to export this chart type")
          grDevices::dev.off()
        }
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
