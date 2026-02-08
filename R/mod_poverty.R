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

      # Poverty line selector
      uiOutput(ns("poverty_line_selector")),

      # Measure selector
      uiOutput(ns("measure_selector")),

      # Breakdown selector
      uiOutput(ns("breakdown_selector")),

      # Date range slider
      uiOutput(ns("date_range_ui")),

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
      shared_data$poverty_data
    })

    # ====================================================================
    # Poverty line choices
    # ====================================================================

    poverty_line_choices <- reactive({
      lang_val <- lang()
      c(
        setNames("wb_300", i18n("poverty.line_300", lang_val)),
        setNames("wb_420", i18n("poverty.line_420", lang_val)),
        setNames("wb_830", i18n("poverty.line_830", lang_val)),
        setNames("br_quarter_mw", i18n("poverty.line_quarter_mw", lang_val)),
        setNames("br_half_mw", i18n("poverty.line_half_mw", lang_val))
      )
    })

    output$poverty_line_selector <- renderUI({
      selectInput(ns("poverty_line"), i18n("poverty.poverty_line", lang()),
                  choices = poverty_line_choices(),
                  selected = isolate(input$poverty_line) %||% "wb_830")
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

      # Filter by poverty line and breakdown
      sub <- dt[poverty_line_id == pline & breakdown_type == breakdown]

      # Filter by date range
      if (!is.null(input$date_slider)) {
        sub <- sub[period >= input$date_slider[1] & period <= input$date_slider[2]]
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

      sub <- filtered_data()
      if (is.null(sub) || nrow(sub) == 0) return(plot_ly())

      # ---- ALL THREE FGT ----
      if (measure == "all_fgt") {
        return(render_all_fgt_plot(sub, breakdown, smoothing, lang_val))
      }

      # ---- SINGLE FGT ----
      y_col <- y_var_name()

      if (breakdown == "overall") {
        setorder(sub, period)
        p <- plot_ly(data = sub, x = ~period, y = as.formula(paste0("~", y_col)),
                     type = "scatter", mode = "lines+markers",
                     line = list(color = "#1976D2", width = 2),
                     marker = list(size = 3, color = "#1976D2"),
                     name = i18n("demographics.nacional", lang_val),
                     hovertemplate = paste0(
                       "%{x|%b %Y}<br>",
                       "%{y:.1%}<extra></extra>"
                     ))
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
          p <- p %>% add_trace(
            data = gdata, x = ~period, y = as.formula(paste0("~", y_col)),
            type = "scatter", mode = "lines",
            line = list(color = colors[i], width = 2),
            name = groups[i],
            hovertemplate = paste0(
              groups[i], "<br>",
              "%{x|%b %Y}<br>",
              "%{y:.1%}<extra></extra>"
            )
          )
        }
      }

      p <- p %>% layout(
        xaxis = list(title = ""),
        yaxis = list(title = "", tickformat = ".1%"),
        legend = list(orientation = "h", y = -0.15),
        separators = get_plotly_separators(lang_val),
        hovermode = "x unified"
      )

      p
    })

    # ====================================================================
    # Helper: Render all-three-FGT subplot
    # ====================================================================

    render_all_fgt_plot <- function(sub, breakdown, smoothing, lang_val) {
      if (is.null(smoothing)) smoothing <- "raw"

      suffix <- if (smoothing == "smooth") "_smooth" else ""
      fgt_cols <- paste0(c("fgt0", "fgt1", "fgt2"), suffix)
      fgt_labels <- c(
        i18n("poverty.headcount", lang_val),
        i18n("poverty.poverty_gap", lang_val),
        i18n("poverty.severity", lang_val)
      )

      # Use overall for 3-panel
      if (breakdown == "overall") {
        setorder(sub, period)

        p <- plotly::subplot(
          plot_ly(data = sub, x = ~period, y = as.formula(paste0("~", fgt_cols[1])),
                  type = "scatter", mode = "lines+markers",
                  line = list(color = "#1976D2", width = 2),
                  marker = list(size = 2, color = "#1976D2"),
                  name = fgt_labels[1],
                  hovertemplate = "%{x|%b %Y}<br>%{y:.1%}<extra></extra>") %>%
            layout(yaxis = list(title = fgt_labels[1], tickformat = ".1%")),

          plot_ly(data = sub, x = ~period, y = as.formula(paste0("~", fgt_cols[2])),
                  type = "scatter", mode = "lines+markers",
                  line = list(color = "#E53935", width = 2),
                  marker = list(size = 2, color = "#E53935"),
                  name = fgt_labels[2],
                  hovertemplate = "%{x|%b %Y}<br>%{y:.1%}<extra></extra>") %>%
            layout(yaxis = list(title = fgt_labels[2], tickformat = ".1%")),

          plot_ly(data = sub, x = ~period, y = as.formula(paste0("~", fgt_cols[3])),
                  type = "scatter", mode = "lines+markers",
                  line = list(color = "#FF7043", width = 2),
                  marker = list(size = 2, color = "#FF7043"),
                  name = fgt_labels[3],
                  hovertemplate = "%{x|%b %Y}<br>%{y:.1%}<extra></extra>") %>%
            layout(yaxis = list(title = fgt_labels[3], tickformat = ".1%")),

          nrows = 3, shareX = TRUE, titleY = TRUE
        )
      } else {
        # With breakdown: just plot fgt0 by group (simplification for multi-group)
        y_col <- paste0("fgt0", suffix)
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
            data = gdata, x = ~period, y = as.formula(paste0("~", y_col)),
            type = "scatter", mode = "lines",
            line = list(color = colors[i], width = 2),
            name = groups[i]
          )
        }

        p <- p %>% layout(
          yaxis = list(tickformat = ".1%"),
          legend = list(orientation = "h", y = -0.15)
        )
      }

      p <- p %>% layout(
        separators = get_plotly_separators(lang_val),
        hovermode = "x unified"
      )

      p
    }

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

      display <- sub[, .(
        Period = format(period, "%b %Y"),
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
        grDevices::png(file, width = 1200, height = 600, res = 150)
        plot.new()
        title("Export not available - use plotly download button")
        grDevices::dev.off()
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
