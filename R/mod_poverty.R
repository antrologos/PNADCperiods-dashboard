# ==============================================================================
# PNADCperiods Dashboard - Poverty Tab Module
# ==============================================================================

# ==============================================================================
# UI
# ==============================================================================

povertyUI <- function(id) {
  ns <- NS(id)

  layout_sidebar(
    fillable = TRUE,

    # Sidebar
    sidebar = sidebar(
      width = 320,
      class = "poverty-sidebar",

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

      # Income variable (Habitual / Efetiva — Phase 2-5)
      uiOutput(ns("poverty_income_var_selector")),

      # Poverty line selector
      uiOutput(ns("poverty_line_selector")),

      # MW help note (Phase 2-5: deprecated, kept as a no-op outlet)
      uiOutput(ns("poverty_line_help")),

      # Measure selector
      uiOutput(ns("measure_selector")),

      # Breakdown selector
      uiOutput(ns("breakdown_selector")),

      # Group filter (conditional, for non-overall breakdowns)
      uiOutput(ns("group_filter_ui")),

      # Date range slider
      uiOutput(ns("date_range_ui")),

      # Seasonal adjustment toggle
      uiOutput(ns("deseason_ui")),

      # Display options (smoothing radio + show_quarterly + show_difference + show_table)
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

      # Difference plot (conditional: show_difference only)
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("show_difference")),
        card(
          card_header(textOutput(ns("difference_plot_title"), inline = TRUE)),
          card_body(
            plotlyOutput(ns("difference_plot"), height = "200px")
          )
        )
      ),

      # Secondary plot (for "show all three" FGT measures)
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

povertyServer <- function(id, shared_data, lang) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ====================================================================
    # Reactive: Available data
    # ====================================================================

    pov_data <- reactive({
      shared_data$get_poverty_data()
    })

    # ====================================================================
    # Data freshness badge
    # ====================================================================

    output$label_last_updated <- renderText({
      i18n("messages.last_updated", lang())
    })

    output$last_updated <- renderText({
      if (!is.null(shared_data$pov_last_updated)) {
        paste0(format(shared_data$pov_last_updated, "%Y-%m-%d %H:%M",
                      tz = "America/Sao_Paulo"), " BRT")
      } else {
        i18n("messages.not_available", lang())
      }
    })

    # ====================================================================
    # Poverty line choices
    # ====================================================================

    poverty_line_choices <- reactive({
      lang_val <- lang()
      # Phase 2-5: only the 3 World Bank lines remain (br_quarter_mw and
      # br_half_mw were dropped together with the IBGE-XLS deflator).
      c(
        setNames("wb_300", i18n("poverty.line_300", lang_val)),
        setNames("wb_420", i18n("poverty.line_420", lang_val)),
        setNames("wb_830", i18n("poverty.line_830", lang_val))
      )
    })

    output$poverty_line_selector <- renderUI({
      selectInput(ns("poverty_line"), i18n("poverty.poverty_line", lang()),
                  choices = poverty_line_choices(),
                  selected = isolate(input$poverty_line) %||% "wb_830")
    })

    # Phase 2-5: income_var selector for poverty (Habitual = hh_pc_hab,
    # Efetiva = hh_pc_efe). Only shown when the loaded poverty_data
    # carries the income_var dimension.
    output$poverty_income_var_selector <- renderUI({
      d <- shared_data$get_poverty_data()
      if (is.null(d) || !"income_var" %in% names(d)) return(NULL)
      lang_val <- lang()
      selectInput(
        ns("poverty_income_var"),
        i18n("inequality.income_var", lang_val),
        choices = c(
          setNames("hh_pc_hab",
                   i18n("inequality.income_var_hh_pc_hab", lang_val)),
          setNames("hh_pc_efe",
                   i18n("inequality.income_var_hh_pc_efe", lang_val))
        ),
        selected = isolate(input$poverty_income_var) %||% "hh_pc_hab"
      )
    })

    # MW help note removed (Phase 2-5: lines no longer exist)
    output$poverty_line_help <- renderUI({
      NULL
    })

    # ====================================================================
    # Measure choices
    # ====================================================================

    measure_choices <- reactive({
      lang_val <- lang()
      c(
        setNames("fgt0", i18n("poverty.headcount", lang_val)),
        setNames("fgt1", i18n("poverty.poverty_gap", lang_val)),
        setNames("fgt2", i18n("poverty.severity", lang_val)),
        setNames("all_fgt", i18n("poverty.show_all_fgt", lang_val))
      )
    })

    output$measure_selector <- renderUI({
      selectInput(ns("measure"), i18n("poverty.measure", lang()),
                  choices = measure_choices(),
                  selected = isolate(input$measure) %||% "fgt0")
    })

    # ====================================================================
    # Breakdown selector
    # ====================================================================

    breakdown_choices <- reactive({
      lang_val <- lang()
      c(
        setNames("overall", i18n("demographics.overall", lang_val)),
        setNames("sex", i18n("demographics.by_sex", lang_val)),
        setNames("race", i18n("demographics.by_race", lang_val)),
        setNames("education", i18n("demographics.by_education", lang_val)),
        setNames("region", i18n("demographics.by_region", lang_val)),
        setNames("uf", i18n("demographics.by_uf", lang_val)),
        setNames("urban_rural", i18n("demographics.by_urban_rural", lang_val)),
        setNames("age_group", i18n("demographics.by_age_group", lang_val))
      )
    })

    output$breakdown_selector <- renderUI({
      selectInput(ns("breakdown"), i18n("demographics.breakdown", lang()),
                  choices = breakdown_choices(),
                  selected = isolate(input$breakdown) %||% "overall")
    })

    # ====================================================================
    # Group filter (conditional multi-select for non-overall breakdowns)
    # ====================================================================

    output$group_filter_ui <- renderUI({
      breakdown <- input$breakdown
      if (is.null(breakdown) || breakdown == "overall") return(NULL)

      dt <- pov_data()
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
      dt <- pov_data()
      if (is.null(dt) || nrow(dt) == 0) return(NULL)
      range(dt$period, na.rm = TRUE)
    })

    output$date_range_ui <- renderUI({
      dr <- date_range()
      if (is.null(dr)) return(NULL)

      sliderInput(ns("date_slider"), i18n("controls.date_range", lang()),
                  min = dr[1], max = dr[2],
                  value = dr,
                  timeFormat = "%b %Y",
                  step = 30)
    })

    # Seasonal adjustment toggle (only when precomputed fgtN_x13 / fgtN_stl
    # columns are present in the loaded poverty_data)
    output$deseason_ui <- renderUI({
      d <- shared_data$get_poverty_data()
      if (is.null(d) ||
          !all(c("fgt0_x13", "fgt0_stl") %in% names(d))) {
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

    # Debounced date slider (prevents 100+ re-renders during drag)
    date_slider_debounced <- reactive({
      input$date_slider
    }) %>% debounce(300)

    # ====================================================================
    # Display options
    # ====================================================================

    output$display_options_ui <- renderUI({
      lang_val <- lang()
      tagList(
        radioButtons(ns("smoothing"), i18n("poverty.smoothing", lang_val),
                     choices = c(
                       setNames("raw", i18n("poverty.smoothing_raw", lang_val)),
                       setNames("smooth", i18n("poverty.smoothing_3mo", lang_val))
                     ),
                     selected = isolate(input$smoothing) %||% "raw",
                     inline = TRUE),
        checkboxInput(
          ns("show_quarterly"),
          i18n("controls.show_quarterly", lang_val),
          value = FALSE
        ),
        checkboxInput(
          ns("show_difference"),
          i18n("controls.show_difference", lang_val),
          value = FALSE
        ),
        checkboxInput(ns("show_table"), i18n("poverty.show_table", lang_val),
                      value = FALSE)
      )
    })

    # ====================================================================
    # Info banner
    # ====================================================================

    output$info_banner <- renderText({
      i18n("poverty.info_banner", lang())
    })

    output$learn_more_link <- renderText({
      if (lang() == "en") "Learn more" else "Saiba mais"
    })

    # ====================================================================
    # Filtered data
    # ====================================================================

    filtered_data <- reactive({
      dt <- pov_data()
      if (is.null(dt) || nrow(dt) == 0) return(NULL)

      pline <- input$poverty_line
      breakdown <- input$breakdown
      if (is.null(pline) || is.null(breakdown)) return(NULL)

      # Phase 2-5: filter by income_var (Habitual / Efetiva). Defaults
      # to hh_pc_hab. Only kicks in when the loaded data carries the
      # column.
      if ("income_var" %in% names(dt)) {
        iv <- input$poverty_income_var %||% "hh_pc_hab"
        dt <- dt[income_var == iv]
      }

      # Filter by poverty line and breakdown
      sub <- dt[poverty_line_id == pline & breakdown_type == breakdown]

      # Filter by selected groups
      if (breakdown != "overall" &&
          !is.null(input$group_filter) && length(input$group_filter) > 0) {
        sub <- sub[breakdown_value %in% input$group_filter]
      }

      # Filter by date range (debounced)
      dr <- date_slider_debounced()
      if (!is.null(dr)) {
        sub <- sub[period >= dr[1] & period <= dr[2]]
      }

      sub
    })

    # ====================================================================
    # Y-variable selector (raw vs smoothed)
    # ====================================================================

    y_var_name <- reactive({
      measure <- input$measure
      smoothing <- input$smoothing
      if (is.null(measure)) measure <- "fgt0"
      if (is.null(smoothing)) smoothing <- "raw"

      if (smoothing == "smooth") {
        switch(measure,
          fgt0 = "fgt0_smooth",
          fgt1 = "fgt1_smooth",
          fgt2 = "fgt2_smooth",
          "fgt0_smooth"  # default for all_fgt
        )
      } else {
        switch(measure,
          fgt0 = "fgt0",
          fgt1 = "fgt1",
          fgt2 = "fgt2",
          "fgt0"  # default for all_fgt
        )
      }
    })

    # ====================================================================
    # Main plot title
    # ====================================================================

    output$main_plot_title <- renderText({
      lang_val <- lang()
      measure <- input$measure
      pline <- input$poverty_line

      mc <- measure_choices()
      measure_label <- names(mc)[mc == measure]
      if (length(measure_label) == 0) measure_label <- i18n("poverty.title", lang_val)

      plc <- poverty_line_choices()
      line_label <- names(plc)[plc == pline]
      if (length(line_label) == 0) line_label <- ""

      paste0(measure_label, " - ", line_label)
    })

    # ====================================================================
    # Main plot
    # ====================================================================

    output$main_plot <- renderPlotly({
      measure <- input$measure
      breakdown <- input$breakdown
      smoothing <- input$smoothing
      lang_val <- lang()

      if (is.null(measure)) return(plot_ly())
      if (is.null(breakdown)) return(plot_ly())

      sub <- filtered_data()
      if (is.null(sub) || nrow(sub) == 0) return(plot_ly())

      # ---- ALL THREE FGT ----
      if (measure == "all_fgt") {
        return(render_all_fgt_plot(sub, breakdown, smoothing,
                                    input$deseason %||% "none", lang_val))
      }

      # ---- SINGLE FGT ----
      y_col <- y_var_name()

      # Seasonal adjustment (none / x13 / stl / both). Mirrors the SIDRA
      # Series Explorer pattern: original faded, adjusted solid.
      deseason <- input$deseason %||% "none"
      x13_col <- paste0(measure, "_x13")
      stl_col <- paste0(measure, "_stl")
      has_x13 <- x13_col %in% names(sub)
      has_stl <- stl_col %in% names(sub)
      if (!has_x13 && !has_stl) deseason <- "none"
      # When deseason is active, the comparison overlay always uses the
      # raw FGT (not the smoothed variant) so it matches what was fed to
      # X-13/STL upstream.
      raw_col <- if (deseason != "none") measure else y_col

      if (breakdown == "overall") {
        setorder(sub, period)
        nacional <- i18n("demographics.nacional", lang_val)
        p <- plot_ly()
        if (deseason == "none") {
          p <- p %>% add_trace(
            data = sub, x = ~period, y = as.formula(paste0("~", y_col)),
            type = "scatter", mode = "lines+markers",
            line = list(color = "#1976D2", width = 2),
            marker = list(size = 3, color = "#1976D2"),
            name = nacional,
            hovertemplate = "%{x|%b %Y}<br>%{y:.1%}<extra></extra>"
          )
        } else {
          # Original (faded background)
          p <- p %>% add_trace(
            data = sub, x = ~period, y = as.formula(paste0("~", raw_col)),
            type = "scatter", mode = "lines",
            line = list(color = "#9E9E9E", width = 1.5, dash = "dot"),
            opacity = 0.7,
            name = paste0(nacional, " - ", i18n("plots.original", lang_val)),
            hovertemplate = "%{x|%b %Y}<br>%{y:.1%}<extra></extra>"
          )
          if (deseason %in% c("x13", "both") && has_x13) {
            p <- p %>% add_trace(
              data = sub, x = ~period, y = as.formula(paste0("~", x13_col)),
              type = "scatter", mode = "lines",
              line = list(color = "#1976D2", width = 2),
              name = paste0(nacional, " - X-13"),
              hovertemplate = "%{x|%b %Y}<br>%{y:.1%}<extra></extra>"
            )
          }
          if (deseason %in% c("stl", "both") && has_stl) {
            stl_color <- if (deseason == "both") "#00ACC1" else "#1976D2"
            p <- p %>% add_trace(
              data = sub, x = ~period, y = as.formula(paste0("~", stl_col)),
              type = "scatter", mode = "lines",
              line = list(color = stl_color, width = 2),
              name = paste0(nacional, " - STL"),
              hovertemplate = "%{x|%b %Y}<br>%{y:.1%}<extra></extra>"
            )
          }
        }
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
          gcol <- colors[i]
          if (deseason == "none") {
            p <- p %>% add_trace(
              data = gdata, x = ~period, y = as.formula(paste0("~", y_col)),
              type = "scatter", mode = "lines",
              line = list(color = gcol, width = 2),
              name = groups[i],
              hovertemplate = paste0(groups[i], "<br>",
                                       "%{x|%b %Y}<br>%{y:.1%}<extra></extra>")
            )
          } else {
            # Original faded (same color as group, dotted)
            p <- p %>% add_trace(
              data = gdata, x = ~period,
              y = as.formula(paste0("~", raw_col)),
              type = "scatter", mode = "lines",
              line = list(color = gcol, width = 1.2, dash = "dot"),
              opacity = 0.45, showlegend = FALSE,
              name = paste0(groups[i], " - ",
                             i18n("plots.original", lang_val)),
              hovertemplate = "%{x|%b %Y}<br>%{y:.1%}<extra></extra>"
            )
            if (deseason %in% c("x13", "both") && has_x13) {
              p <- p %>% add_trace(
                data = gdata, x = ~period,
                y = as.formula(paste0("~", x13_col)),
                type = "scatter", mode = "lines",
                line = list(color = gcol, width = 2),
                name = if (deseason == "both")
                  paste0(groups[i], " - X-13") else groups[i],
                hovertemplate = paste0(groups[i], "<br>",
                                         "%{x|%b %Y}<br>%{y:.1%}<extra></extra>")
              )
            }
            if (deseason %in% c("stl", "both") && has_stl) {
              p <- p %>% add_trace(
                data = gdata, x = ~period,
                y = as.formula(paste0("~", stl_col)),
                type = "scatter", mode = "lines",
                line = list(color = gcol, width = 2,
                             dash = if (deseason == "both") "dash" else "solid"),
                name = if (deseason == "both")
                  paste0(groups[i], " - STL") else groups[i],
                hovertemplate = paste0(groups[i], "<br>",
                                         "%{x|%b %Y}<br>%{y:.1%}<extra></extra>")
              )
            }
          }
        }
      }

      # Optional 3-month rolling-mean overlay (mirror SIDRA "show quarterly").
      # Mirrors SIDRA: only shown when deseason == "none" (otherwise the
      # adjusted line already supplies the smoothing reference).
      if (isTRUE(input$show_quarterly) && deseason == "none") {
        # Use raw_col so the comparison stays on the same baseline as the
        # adjusted series.
        if (breakdown == "overall") {
          setorder(sub, period)
          rolling <- data.table::frollmean(sub[[raw_col]], 3, align = "right")
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
            rolling <- data.table::frollmean(gdata[[raw_col]], 3, align = "right")
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

      # Adaptive legend: vertical for many groups, horizontal for few
      n_groups <- length(unique(sub$breakdown_value))
      legend_config <- if (n_groups > 6) {
        list(orientation = "v", x = 1.02, y = 1, font = list(size = 10))
      } else {
        list(orientation = "h", y = -0.15)
      }

      p <- p %>% layout(
        xaxis = list(title = ""),
        yaxis = list(title = "", tickformat = ".1%"),
        legend = legend_config,
        separators = get_plotly_separators(lang_val),
        hovermode = "x unified"
      )

      p
    })

    # ====================================================================
    # Helper: Render all-three-FGT subplot
    # ====================================================================

    render_all_fgt_plot <- function(sub, breakdown, smoothing, deseason,
                                     lang_val) {
      if (is.null(smoothing)) smoothing <- "raw"
      if (is.null(deseason))  deseason  <- "none"

      suffix <- if (smoothing == "smooth") "_smooth" else ""
      fgt_base <- c("fgt0", "fgt1", "fgt2")
      # When deseason is active, the comparison overlay always uses the
      # raw FGT (matching what was fed to X-13/STL upstream).
      raw_cols <- if (deseason != "none") fgt_base else paste0(fgt_base, suffix)
      x13_cols <- paste0(fgt_base, "_x13")
      stl_cols <- paste0(fgt_base, "_stl")
      fgt_colors <- c("#1976D2", "#E53935", "#FF7043")
      fgt_labels <- c(
        i18n("poverty.headcount", lang_val),
        i18n("poverty.poverty_gap", lang_val),
        i18n("poverty.severity", lang_val)
      )
      has_x13 <- all(x13_cols %in% names(sub))
      has_stl <- all(stl_cols %in% names(sub))
      if (!has_x13 && !has_stl) deseason <- "none"

      build_panel <- function(raw_col, x13_col, stl_col, color, label) {
        p <- plot_ly()
        if (deseason == "none") {
          p <- p %>% add_trace(
            data = sub, x = ~period, y = as.formula(paste0("~", raw_col)),
            type = "scatter", mode = "lines+markers",
            line = list(color = color, width = 2),
            marker = list(size = 2, color = color),
            name = label,
            hovertemplate = "%{x|%b %Y}<br>%{y:.1%}<extra></extra>"
          )
        } else {
          p <- p %>% add_trace(
            data = sub, x = ~period, y = as.formula(paste0("~", raw_col)),
            type = "scatter", mode = "lines",
            line = list(color = "#9E9E9E", width = 1.5, dash = "dot"),
            opacity = 0.7, showlegend = FALSE,
            name = paste0(label, " - ", i18n("plots.original", lang_val)),
            hovertemplate = "%{x|%b %Y}<br>%{y:.1%}<extra></extra>"
          )
          if (deseason %in% c("x13", "both") && has_x13) {
            p <- p %>% add_trace(
              data = sub, x = ~period, y = as.formula(paste0("~", x13_col)),
              type = "scatter", mode = "lines",
              line = list(color = color, width = 2),
              name = if (deseason == "both") paste0(label, " - X-13") else label,
              hovertemplate = "%{x|%b %Y}<br>%{y:.1%}<extra></extra>"
            )
          }
          if (deseason %in% c("stl", "both") && has_stl) {
            stl_color <- if (deseason == "both") "#00ACC1" else color
            p <- p %>% add_trace(
              data = sub, x = ~period, y = as.formula(paste0("~", stl_col)),
              type = "scatter", mode = "lines",
              line = list(color = stl_color, width = 2,
                           dash = if (deseason == "both") "dash" else "solid"),
              name = if (deseason == "both") paste0(label, " - STL") else label,
              hovertemplate = "%{x|%b %Y}<br>%{y:.1%}<extra></extra>"
            )
          }
        }
        p %>% layout(yaxis = list(title = "", tickformat = ".1%"))
      }

      # Use overall for 3-panel
      if (breakdown == "overall") {
        setorder(sub, period)

        p <- plotly::subplot(
          build_panel(raw_cols[1], x13_cols[1], stl_cols[1],
                       fgt_colors[1], fgt_labels[1]),
          build_panel(raw_cols[2], x13_cols[2], stl_cols[2],
                       fgt_colors[2], fgt_labels[2]),
          build_panel(raw_cols[3], x13_cols[3], stl_cols[3],
                       fgt_colors[3], fgt_labels[3]),
          nrows = 3, shareX = TRUE, titleY = FALSE
        ) %>% layout(
          annotations = list(
            list(text = fgt_labels[1], x = -0.06, y = 0.88,
                 xref = "paper", yref = "paper",
                 showarrow = FALSE, textangle = -90,
                 font = list(size = 11)),
            list(text = fgt_labels[2], x = -0.06, y = 0.50,
                 xref = "paper", yref = "paper",
                 showarrow = FALSE, textangle = -90,
                 font = list(size = 11)),
            list(text = fgt_labels[3], x = -0.06, y = 0.12,
                 xref = "paper", yref = "paper",
                 showarrow = FALSE, textangle = -90,
                 font = list(size = 11))
          ),
          margin = list(l = 80)
        )
      } else {
        # With breakdown: plot only fgt0 by group (simplification: 3 FGTs ×
        # N groups would be unreadable). Same overlay pattern as above.
        raw_col <- raw_cols[1]
        x13_col <- x13_cols[1]
        stl_col <- stl_cols[1]
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
              data = gdata, x = ~period,
              y = as.formula(paste0("~", raw_col)),
              type = "scatter", mode = "lines",
              line = list(color = gcol, width = 2),
              name = groups[i]
            )
          } else {
            p <- p %>% add_trace(
              data = gdata, x = ~period,
              y = as.formula(paste0("~", raw_col)),
              type = "scatter", mode = "lines",
              line = list(color = gcol, width = 1.2, dash = "dot"),
              opacity = 0.45, showlegend = FALSE,
              name = paste0(groups[i], " - ",
                             i18n("plots.original", lang_val))
            )
            if (deseason %in% c("x13", "both") && has_x13) {
              p <- p %>% add_trace(
                data = gdata, x = ~period,
                y = as.formula(paste0("~", x13_col)),
                type = "scatter", mode = "lines",
                line = list(color = gcol, width = 2),
                name = if (deseason == "both")
                  paste0(groups[i], " - X-13") else groups[i]
              )
            }
            if (deseason %in% c("stl", "both") && has_stl) {
              p <- p %>% add_trace(
                data = gdata, x = ~period,
                y = as.formula(paste0("~", stl_col)),
                type = "scatter", mode = "lines",
                line = list(color = gcol, width = 2,
                             dash = if (deseason == "both") "dash" else "solid"),
                name = if (deseason == "both")
                  paste0(groups[i], " - STL") else groups[i]
              )
            }
          }
        }

        legend_cfg <- if (length(groups) > 6) {
          list(orientation = "v", x = 1.02, y = 1, font = list(size = 10))
        } else {
          list(orientation = "h", y = -0.15)
        }

        p <- p %>% layout(
          yaxis = list(tickformat = ".1%"),
          legend = legend_cfg
        )
      }

      p <- p %>% layout(
        separators = get_plotly_separators(lang_val),
        hovermode = "x unified"
      )

      p
    }

    # ====================================================================
    # Difference plot — adapts to the seasonal-adjustment toggle:
    #   - none: fgt − rolling-3 mean
    #   - x13:  fgt − fgtN_x13 (X-13 seasonal component)
    #   - stl:  fgt − fgtN_stl (STL seasonal component)
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
      measure <- input$measure
      breakdown <- input$breakdown
      validate(need(!is.null(measure), ""), need(!is.null(breakdown), ""))

      # Choose the FGT column whose difference we compute (for "all_fgt"
      # mode the difference plot focuses on FGT-0 as a representative).
      fgt_base <- if (measure == "all_fgt") "fgt0" else measure
      raw_col  <- fgt_base
      x13_col  <- paste0(fgt_base, "_x13")
      stl_col  <- paste0(fgt_base, "_stl")

      m <- input$deseason %||% "none"
      has_x13 <- x13_col %in% names(sub)
      has_stl <- stl_col %in% names(sub)
      if (m %in% c("x13", "both") && !has_x13) m <- "none"
      if (m == "stl" && !has_stl) m <- "none"

      compute_diff <- function(df, method) {
        switch(method,
          none = df[[raw_col]] -
                   data.table::frollmean(df[[raw_col]], 3, align = "right"),
          x13  = df[[raw_col]] - df[[x13_col]],
          stl  = df[[raw_col]] - df[[stl_col]]
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
          yaxis = list(title = "", tickformat = ".1%"),
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
          yaxis = list(title = "", tickformat = ".1%"),
          separators = get_plotly_separators(lang_val),
          hovermode = "x unified",
          legend = legend_cfg
        )
      }
    })

    # ====================================================================
    # Secondary plot (conditional: show gap + severity when headcount selected)
    # ====================================================================

    output$secondary_plot_ui <- renderUI({
      # No secondary plot needed — "all_fgt" mode handled in main plot
      NULL
    })

    # ====================================================================
    # Stat cards
    # ====================================================================

    output$stat_cards_ui <- renderUI({
      sub <- filtered_data()
      if (is.null(sub) || nrow(sub) == 0) return(NULL)

      measure <- input$measure
      breakdown <- input$breakdown
      lang_val <- lang()
      if (is.null(breakdown)) return(NULL)

      if (is.null(measure) || measure == "all_fgt") measure <- "fgt0"

      # Use overall data for stat cards; if breakdown is already overall, use sub directly
      if (breakdown == "overall") {
        sub_overall <- sub
      } else {
        sub_overall <- sub[breakdown_type == "overall"]
        if (nrow(sub_overall) == 0) sub_overall <- sub
      }

      y_col <- y_var_name()
      vals <- sub_overall[[y_col]]
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0) return(NULL)

      setorder(sub_overall, period)
      latest_val <- vals[length(vals)]
      min_val <- min(vals)
      max_val <- max(vals)

      # Number of poor and population (latest)
      latest_row <- sub_overall[nrow(sub_overall)]
      n_poor_latest <- latest_row$n_poor
      pop_latest <- latest_row$total_pop

      # YoY
      if (length(vals) > 12) {
        yoy_diff <- vals[length(vals)] - vals[length(vals) - 12]
      } else {
        yoy_diff <- NA
      }

      # Format functions
      fmt_pct <- function(x) {
        if (is.na(x)) return(i18n("stats.na", lang_val))
        sprintf("%.1f%%", x * 100)
      }

      fmt_pop <- function(x) {
        if (is.na(x) || is.null(x)) return(i18n("stats.na", lang_val))
        if (lang_val == "pt") {
          paste0(format(round(x / 1e6, 1), decimal.mark = ",", big.mark = "."), " mi")
        } else {
          paste0(format(round(x / 1e6, 1), big.mark = ","), " M")
        }
      }

      fmt_yoy <- function(x) {
        if (is.na(x)) return(i18n("stats.na", lang_val))
        pp <- x * 100  # percentage points
        sprintf("%+.1f p.p.", pp)
      }

      tags$div(
        class = "row g-2 mt-3",
        tags$div(class = "col",
          tags$div(class = "stat-card stat-card-highlight text-center p-2",
            tags$small(i18n("stats.latest", lang_val)),
            tags$div(class = "fw-bold", fmt_pct(latest_val)))),
        tags$div(class = "col",
          tags$div(class = "stat-card text-center p-2",
            tags$small(i18n("poverty.n_poor", lang_val)),
            tags$div(class = "fw-bold", fmt_pop(n_poor_latest)))),
        tags$div(class = "col",
          tags$div(class = "stat-card text-center p-2",
            tags$small(i18n("stats.min", lang_val)),
            tags$div(class = "fw-bold", fmt_pct(min_val)))),
        tags$div(class = "col",
          tags$div(class = "stat-card text-center p-2",
            tags$small(i18n("stats.max", lang_val)),
            tags$div(class = "fw-bold", fmt_pct(max_val)))),
        tags$div(class = "col",
          tags$div(class = "stat-card text-center p-2",
            tags$small(i18n("stats.yoy_change", lang_val)),
            tags$div(class = "fw-bold", fmt_yoy(yoy_diff))))
      )
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
        `FGT-0` = round(fgt0, 4),
        `FGT-1` = round(fgt1, 4),
        `FGT-2` = round(fgt2, 4),
        `N Poor` = round(n_poor, 0),
        Population = round(total_pop, 0),
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
        paste0("poverty_", input$poverty_line, "_", Sys.Date(), ".csv")
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
        paste0("poverty_", input$poverty_line, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        sub <- filtered_data()
        if (is.null(sub) || nrow(sub) == 0) return()

        lang_val <- lang()
        y_col <- y_var_name()
        measure <- input$measure
        if (is.null(measure) || measure == "all_fgt") y_col <- "fgt0"

        mc <- measure_choices()
        title_label <- names(mc)[mc == measure]
        if (length(title_label) == 0) title_label <- ""

        plc <- poverty_line_choices()
        line_label <- names(plc)[plc == input$poverty_line]
        if (length(line_label) == 0) line_label <- ""

        p <- ggplot2::ggplot(sub, ggplot2::aes(x = period, y = get(y_col),
                                                 color = breakdown_value)) +
          ggplot2::geom_line(linewidth = 0.8) +
          ggplot2::scale_y_continuous(labels = function(x) paste0(round(x * 100, 1), "%")) +
          ggplot2::labs(title = paste0(title_label, " - ", line_label),
                        x = "", y = "", color = "") +
          ggplot2::theme_minimal(base_size = 12) +
          ggplot2::theme(legend.position = "bottom")
        ggplot2::ggsave(file, p, width = 10, height = 6, dpi = 150)
      }
    )

    output$download_excel <- downloadHandler(
      filename = function() {
        paste0("poverty_", input$poverty_line, "_", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        sub <- filtered_data()
        if (!is.null(sub) && requireNamespace("openxlsx", quietly = TRUE)) {
          openxlsx::write.xlsx(as.data.frame(sub), file)
        } else if (!is.null(sub)) {
          fwrite(sub, sub(".xlsx$", ".csv", file))
        }
      }
    )
  })
}
