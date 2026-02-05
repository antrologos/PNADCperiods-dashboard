# ==============================================================================
# Geographic Analysis Module
# ==============================================================================
#
# Phase 3: Geographic visualization of labor market indicators by state (UF)
#
# This module provides:
#   - Choropleth map of Brazil showing state-level indicators
#   - Time slider/animation for temporal analysis
#   - State comparison table
#   - Multiple indicator selection
#
# ==============================================================================

# Null-coalescing operator (if not available)
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# ==============================================================================
# Brazil State Codes and Names
# ==============================================================================

# UF codes mapping (IBGE codes to abbreviations and names)
uf_mapping <- data.table::data.table(
  uf_code = c(11L, 12L, 13L, 14L, 15L, 16L, 17L,
              21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L,
              31L, 32L, 33L, 35L,
              41L, 42L, 43L,
              50L, 51L, 52L, 53L),
  uf_abbrev = c("RO", "AC", "AM", "RR", "PA", "AP", "TO",
                "MA", "PI", "CE", "RN", "PB", "PE", "AL", "SE", "BA",
                "MG", "ES", "RJ", "SP",
                "PR", "SC", "RS",
                "MS", "MT", "GO", "DF"),
  uf_name_pt = c("Rondonia", "Acre", "Amazonas", "Roraima", "Para", "Amapa", "Tocantins",
                 "Maranhao", "Piaui", "Ceara", "Rio Grande do Norte", "Paraiba",
                 "Pernambuco", "Alagoas", "Sergipe", "Bahia",
                 "Minas Gerais", "Espirito Santo", "Rio de Janeiro", "Sao Paulo",
                 "Parana", "Santa Catarina", "Rio Grande do Sul",
                 "Mato Grosso do Sul", "Mato Grosso", "Goias", "Distrito Federal"),
  uf_name_en = c("Rondonia", "Acre", "Amazonas", "Roraima", "Para", "Amapa", "Tocantins",
                 "Maranhao", "Piaui", "Ceara", "Rio Grande do Norte", "Paraiba",
                 "Pernambuco", "Alagoas", "Sergipe", "Bahia",
                 "Minas Gerais", "Espirito Santo", "Rio de Janeiro", "Sao Paulo",
                 "Parana", "Santa Catarina", "Rio Grande do Sul",
                 "Mato Grosso do Sul", "Mato Grosso", "Goias", "Distrito Federal"),
  region_pt = c(rep("Norte", 7), rep("Nordeste", 9), rep("Sudeste", 4),
                rep("Sul", 3), rep("Centro-Oeste", 4)),
  region_en = c(rep("North", 7), rep("Northeast", 9), rep("Southeast", 4),
                rep("South", 3), rep("Central-West", 4))
)

# Geographic indicator definitions (subset of series available at UF level)
geographic_indicators <- data.table::data.table(
  indicator_id = c("taxadesocup", "taxapartic", "nivelocup"),
  table_id = c(4093L, 4092L, 4094L),
  variable_id = c(4099L, 4096L, 4097L),
  description_pt = c(
    "Taxa de desocupacao",
    "Taxa de participacao na forca de trabalho",
    "Nivel de ocupacao"
  ),
  description_en = c(
    "Unemployment rate",
    "Labor force participation rate",
    "Employment-population ratio"
  ),
  unit = c("percent", "percent", "percent"),
  color_scale = c("Reds", "Blues", "Greens")
)

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

geographicUI <- function(id) {

  ns <- NS(id)

  layout_sidebar(
    fillable = TRUE,

    # Sidebar with controls
    sidebar = sidebar(
      width = "300px",
      open = "desktop",
      bg = "white",

      # Data freshness
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

      # Indicator selection
      selectInput(
        ns("indicator"),
        label = tags$span(style = "font-size: 0.75rem; font-weight: 600;",
                          textOutput(ns("label_indicator"), inline = TRUE)),
        choices = NULL,
        selected = NULL
      ),

      tags$hr(style = "margin: 0.75rem 0;"),

      # Period selection
      tags$label(
        style = "font-size: 0.75rem; font-weight: 600;",
        textOutput(ns("label_period"), inline = TRUE)
      ),

      uiOutput(ns("period_slider_ui")),

      # Animation controls
      div(
        class = "d-flex gap-2 mt-2",
        actionButton(
          ns("animate"),
          label = NULL,
          icon = icon("play"),
          class = "btn-sm btn-outline-primary",
          style = "width: 40px;"
        ),
        actionButton(
          ns("stop_animation"),
          label = NULL,
          icon = icon("stop"),
          class = "btn-sm btn-outline-secondary",
          style = "width: 40px;"
        ),
        div(
          class = "ms-auto",
          selectInput(
            ns("animation_speed"),
            label = NULL,
            choices = c("Slow" = "2000", "Normal" = "1000", "Fast" = "500"),
            selected = "1000",
            width = "100px"
          )
        )
      ),

      tags$hr(style = "margin: 0.75rem 0;"),

      # Display options
      tags$p(style = "font-size: 0.7rem; font-weight: 600; color: #6c757d; margin-bottom: 0.5rem;",
             textOutput(ns("label_display_options"), inline = TRUE)),

      checkboxInput(
        ns("show_labels"),
        label = tags$span(style = "font-size: 0.8rem;",
                          textOutput(ns("label_show_labels"), inline = TRUE)),
        value = TRUE
      ),

      checkboxInput(
        ns("show_table"),
        label = tags$span(style = "font-size: 0.8rem;",
                          textOutput(ns("label_show_table"), inline = TRUE)),
        value = TRUE
      ),

      tags$hr(style = "margin: 0.75rem 0;"),

      # Download
      tags$p(style = "font-size: 0.7rem; font-weight: 600; color: #6c757d; margin-bottom: 0.5rem;",
             "EXPORT"),
      div(
        class = "d-flex gap-1 flex-wrap",
        downloadButton(ns("download_csv"), "CSV", class = "btn-sm btn-outline-secondary", style = "font-size: 0.75rem;"),
        downloadButton(ns("download_png"), "PNG", class = "btn-sm btn-outline-secondary", style = "font-size: 0.75rem;")
      )
    ),

    # Main content area
    div(
      class = "p-3",

      # Map card
      div(
        class = "card mb-3",
        style = "border: none; box-shadow: 0 1px 3px rgba(0,0,0,0.08);",
        div(
          class = "card-header bg-white d-flex justify-content-between align-items-center",
          style = "padding: 0.75rem 1rem; border-bottom: 1px solid #eee;",
          div(
            tags$h6(class = "mb-0", style = "font-weight: 600; font-size: 0.9rem;",
                    textOutput(ns("map_title"), inline = TRUE)),
            tags$small(class = "text-muted",
                       style = "font-size: 0.75rem;",
                       textOutput(ns("map_subtitle"), inline = TRUE))
          )
        ),
        div(
          class = "card-body",
          style = "padding: 0.5rem;",
          plotlyOutput(ns("choropleth_map"), height = "500px")
        )
      ),

      # Summary statistics row
      div(
        class = "card mb-3",
        style = "border: none; box-shadow: 0 1px 3px rgba(0,0,0,0.08);",
        div(
          class = "card-header bg-white",
          style = "padding: 0.5rem 1rem; border-bottom: 1px solid #eee;",
          tags$span(style = "font-weight: 600; font-size: 0.85rem;",
                    textOutput(ns("label_summary"), inline = TRUE))
        ),
        div(
          class = "card-body",
          style = "padding: 1rem;",
          div(
            class = "row g-3",
            # Minimum (best performing state)
            div(
              class = "col",
              div(
                style = "text-align: center; padding: 0.5rem; background: #e8f5e9; border-radius: 6px;",
                tags$div(style = "font-size: 0.65rem; color: #2e7d32; text-transform: uppercase;",
                         textOutput(ns("label_min_state"), inline = TRUE)),
                tags$div(style = "font-size: 1.1rem; font-weight: 700; color: #2e7d32;",
                         textOutput(ns("stat_min"), inline = TRUE)),
                tags$div(style = "font-size: 0.7rem; color: #666;",
                         textOutput(ns("stat_min_state"), inline = TRUE))
              )
            ),
            # Maximum (needs attention)
            div(
              class = "col",
              div(
                style = "text-align: center; padding: 0.5rem; background: #ffebee; border-radius: 6px;",
                tags$div(style = "font-size: 0.65rem; color: #c62828; text-transform: uppercase;",
                         textOutput(ns("label_max_state"), inline = TRUE)),
                tags$div(style = "font-size: 1.1rem; font-weight: 700; color: #c62828;",
                         textOutput(ns("stat_max"), inline = TRUE)),
                tags$div(style = "font-size: 0.7rem; color: #666;",
                         textOutput(ns("stat_max_state"), inline = TRUE))
              )
            ),
            # National average
            div(
              class = "col",
              div(
                style = "text-align: center; padding: 0.5rem; background: #e3f2fd; border-radius: 6px;",
                tags$div(style = "font-size: 0.65rem; color: #1565c0; text-transform: uppercase;",
                         textOutput(ns("label_brazil_avg"), inline = TRUE)),
                tags$div(style = "font-size: 1.1rem; font-weight: 700; color: #1565c0;",
                         textOutput(ns("stat_brazil"), inline = TRUE))
              )
            ),
            # Coefficient of variation
            div(
              class = "col",
              div(
                style = "text-align: center; padding: 0.5rem; background: #f5f5f5; border-radius: 6px;",
                tags$div(style = "font-size: 0.65rem; color: #666; text-transform: uppercase;",
                         textOutput(ns("label_cv"), inline = TRUE)),
                tags$div(style = "font-size: 1.1rem; font-weight: 700; color: #333;",
                         textOutput(ns("stat_cv"), inline = TRUE))
              )
            )
          )
        )
      ),

      # State data table (conditional)
      conditionalPanel(
        condition = sprintf("input['%s']", ns("show_table")),
        div(
          class = "card",
          style = "border: none; box-shadow: 0 1px 3px rgba(0,0,0,0.08);",
          div(
            class = "card-header bg-white",
            style = "padding: 0.5rem 1rem; border-bottom: 1px solid #eee;",
            tags$span(style = "font-weight: 600; font-size: 0.85rem;",
                      textOutput(ns("label_table"), inline = TRUE))
          ),
          div(
            class = "card-body",
            style = "padding: 0.75rem;",
            DTOutput(ns("state_table"))
          )
        )
      )
    )
  )
}


# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

geographicServer <- function(id, shared_data, lang = reactive("pt")) {
  moduleServer(id, function(input, output, session) {

    # --------------------------------------------------------------------------
    # Reactive values for animation
    # --------------------------------------------------------------------------

    animation_state <- reactiveValues(
      running = FALSE,
      timer = NULL
    )

    # Helper to get current language
    get_lang <- reactive({ lang() })

    # --------------------------------------------------------------------------
    # i18n Label Outputs
    # --------------------------------------------------------------------------

    output$label_last_updated <- renderText({ i18n("messages.last_updated", get_lang()) })
    output$label_indicator <- renderText({ toupper(i18n("geographic.select_indicator", get_lang())) })
    output$label_period <- renderText({ toupper(i18n("geographic.select_period", get_lang())) })
    output$label_display_options <- renderText({ toupper(if(get_lang() == "en") "OPTIONS" else "OPCOES") })
    output$label_show_labels <- renderText({ if(get_lang() == "en") "Show state labels" else "Mostrar siglas dos estados" })
    output$label_show_table <- renderText({ if(get_lang() == "en") "Show state table" else "Mostrar tabela por estado" })
    output$label_summary <- renderText({ if(get_lang() == "en") "Regional Summary" else "Resumo Regional" })
    output$label_table <- renderText({ if(get_lang() == "en") "State Data" else "Dados por Estado" })
    output$label_min_state <- renderText({ if(get_lang() == "en") "Lowest" else "Menor" })
    output$label_max_state <- renderText({ if(get_lang() == "en") "Highest" else "Maior" })
    output$label_brazil_avg <- renderText({ i18n("geographic.brazil", get_lang()) })
    output$label_cv <- renderText({ if(get_lang() == "en") "Dispersion (CV)" else "Dispersao (CV)" })

    # --------------------------------------------------------------------------
    # Load geographic data
    # --------------------------------------------------------------------------

    geo_data <- reactive({
      shared_data$geographic_data
    })

    # --------------------------------------------------------------------------
    # Update indicator choices
    # --------------------------------------------------------------------------

    observe({
      lang_val <- get_lang()

      # Get indicator choices with translated labels
      desc_col <- if(lang_val == "en") "description_en" else "description_pt"
      choices <- setNames(
        geographic_indicators$indicator_id,
        geographic_indicators[[desc_col]]
      )

      updateSelectInput(
        session, "indicator",
        choices = choices,
        selected = "taxadesocup"  # Default to unemployment rate
      )
    }) |> bindEvent(get_lang(), once = FALSE)

    # --------------------------------------------------------------------------
    # Dynamic period slider
    # --------------------------------------------------------------------------

    available_periods <- reactive({
      data <- geo_data()
      if (is.null(data)) return(NULL)
      sort(unique(data$anomesfinaltrimmovel))
    })

    output$period_slider_ui <- renderUI({
      periods <- available_periods()
      if (is.null(periods) || length(periods) == 0) {
        return(tags$p(class = "text-muted", "No data available"))
      }

      lang_val <- get_lang()

      # Create named choices with formatted labels
      labels <- sapply(periods, function(p) {
        format_date_i18n(p, lang_val, format = "short")
      })
      choices <- setNames(as.character(periods), labels)

      selectInput(
        session$ns("period"),
        label = NULL,
        choices = choices,
        selected = as.character(max(periods)),
        width = "100%"
      )
    })

    # --------------------------------------------------------------------------
    # Filtered data for selected period and indicator
    # --------------------------------------------------------------------------

    filtered_geo_data <- reactive({
      data <- geo_data()
      req(data, input$indicator, input$period)

      period_val <- as.integer(input$period)
      indicator_val <- input$indicator

      # Filter data
      result <- data[anomesfinaltrimmovel == period_val & indicator == indicator_val]

      # Merge with UF mapping for names
      lang_val <- get_lang()
      name_col <- if(lang_val == "en") "uf_name_en" else "uf_name_pt"
      region_col <- if(lang_val == "en") "region_en" else "region_pt"

      result <- merge(result, uf_mapping, by = "uf_code", all.x = TRUE)
      result[, uf_name := get(name_col)]
      result[, region := get(region_col)]

      result
    })

    # --------------------------------------------------------------------------
    # Output: Last updated
    # --------------------------------------------------------------------------

    output$last_updated <- renderText({
      if (!is.null(shared_data$geo_last_updated)) {
        format(shared_data$geo_last_updated, "%Y-%m-%d %H:%M")
      } else {
        "Not available"
      }
    })

    # --------------------------------------------------------------------------
    # Output: Map title
    # --------------------------------------------------------------------------

    output$map_title <- renderText({
      req(input$indicator)
      lang_val <- get_lang()
      desc_col <- if(lang_val == "en") "description_en" else "description_pt"
      idx <- which(geographic_indicators$indicator_id == input$indicator)
      if (length(idx) > 0) {
        geographic_indicators[[desc_col]][idx]
      } else {
        input$indicator
      }
    })

    output$map_subtitle <- renderText({
      req(input$period)
      lang_val <- get_lang()
      period_label <- format_date_i18n(as.integer(input$period), lang_val, format = "long")

      if(lang_val == "en") {
        paste("Rolling quarter ending in", period_label)
      } else {
        paste("Trimestre movel encerrado em", period_label)
      }
    })

    # --------------------------------------------------------------------------
    # Output: Choropleth map
    # --------------------------------------------------------------------------

    output$choropleth_map <- renderPlotly({
      data <- filtered_geo_data()

      validate(
        need(!is.null(data), "Loading data..."),
        need(nrow(data) > 0, "No data available for the selected period")
      )

      lang_val <- get_lang()

      # Get color scale based on indicator
      idx <- which(geographic_indicators$indicator_id == input$indicator)
      color_scale <- if (length(idx) > 0) geographic_indicators$color_scale[idx] else "Blues"

      # Create hover text
      data[, hover_text := paste0(
        "<b>", uf_abbrev, " - ", uf_name, "</b><br>",
        if(lang_val == "en") "Value: " else "Valor: ",
        format_number_i18n(value, 1, lang_val), "%<br>",
        if(lang_val == "en") "Region: " else "Regiao: ",
        region
      )]

      # Prepare data for plotly choropleth
      # We'll use a simple bar chart styled as a map alternative
      # since plotly choropleth requires geojson

      # Sort by value for visualization
      data <- data[order(-value)]

      # Create color scale
      n_colors <- nrow(data)
      if (color_scale == "Reds") {
        colors <- colorRampPalette(c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15"))(n_colors)
      } else if (color_scale == "Greens") {
        colors <- colorRampPalette(c("#edf8e9", "#c7e9c0", "#a1d99b", "#74c476", "#31a354", "#006d2c"))(n_colors)
      } else {
        colors <- colorRampPalette(c("#eff3ff", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c"))(n_colors)
      }

      # Assign colors based on rank
      data[, color := colors[.I]]

      # Create horizontal bar chart (more readable for 27 states)
      p <- plot_ly(
        data = data,
        type = "bar",
        orientation = "h",
        y = ~reorder(uf_abbrev, value),
        x = ~value,
        marker = list(
          color = ~value,
          colorscale = color_scale,
          showscale = TRUE,
          colorbar = list(
            title = list(text = "%", font = list(size = 11)),
            ticksuffix = "%",
            len = 0.6
          )
        ),
        text = ~paste0(format_number_i18n(value, 1, lang_val), "%"),
        textposition = "outside",
        hovertext = ~hover_text,
        hoverinfo = "text"
      )

      # Layout
      p %>% layout(
        xaxis = list(
          title = "",
          ticksuffix = "%",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 10)
        ),
        margin = list(l = 50, r = 30, t = 10, b = 30),
        paper_bgcolor = "white",
        plot_bgcolor = "white"
      ) %>%
        config(displayModeBar = TRUE, displaylogo = FALSE,
               modeBarButtonsToRemove = c("lasso2d", "select2d"))
    })

    # --------------------------------------------------------------------------
    # Output: Summary statistics
    # --------------------------------------------------------------------------

    output$stat_min <- renderText({
      data <- filtered_geo_data()
      req(data, nrow(data) > 0)
      lang_val <- get_lang()
      min_val <- min(data$value, na.rm = TRUE)
      paste0(format_number_i18n(min_val, 1, lang_val), "%")
    })

    output$stat_min_state <- renderText({
      data <- filtered_geo_data()
      req(data, nrow(data) > 0)
      min_idx <- which.min(data$value)
      data$uf_name[min_idx]
    })

    output$stat_max <- renderText({
      data <- filtered_geo_data()
      req(data, nrow(data) > 0)
      lang_val <- get_lang()
      max_val <- max(data$value, na.rm = TRUE)
      paste0(format_number_i18n(max_val, 1, lang_val), "%")
    })

    output$stat_max_state <- renderText({
      data <- filtered_geo_data()
      req(data, nrow(data) > 0)
      max_idx <- which.max(data$value)
      data$uf_name[max_idx]
    })

    output$stat_brazil <- renderText({
      data <- filtered_geo_data()
      req(data, nrow(data) > 0)
      lang_val <- get_lang()
      # Brazil average (population-weighted would be better, but use simple mean for now)
      brazil_avg <- mean(data$value, na.rm = TRUE)
      paste0(format_number_i18n(brazil_avg, 1, lang_val), "%")
    })

    output$stat_cv <- renderText({
      data <- filtered_geo_data()
      req(data, nrow(data) > 0)
      lang_val <- get_lang()
      # Coefficient of variation
      cv <- sd(data$value, na.rm = TRUE) / mean(data$value, na.rm = TRUE) * 100
      paste0(format_number_i18n(cv, 1, lang_val), "%")
    })

    # --------------------------------------------------------------------------
    # Output: State data table
    # --------------------------------------------------------------------------

    output$state_table <- renderDT({
      data <- filtered_geo_data()
      req(data, nrow(data) > 0)
      lang_val <- get_lang()

      # Select and rename columns
      dt_display <- data[, .(
        State = uf_name,
        Abbrev = uf_abbrev,
        Region = region,
        Value = value
      )]

      # Sort by value descending (before renaming columns)
      dt_display <- dt_display[order(-Value)]

      # i18n column names
      if(lang_val == "pt") {
        setnames(dt_display, c("State", "Abbrev", "Region", "Value"),
                 c("Estado", "Sigla", "Regiao", "Valor"))
      }

      datatable(
        dt_display,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'frtip',
          order = list(list(3, 'desc'))
        ),
        rownames = FALSE,
        style = "bootstrap4",
        class = "table-sm table-striped"
      ) %>%
        formatRound(columns = if(lang_val == "en") "Value" else "Valor", digits = 1)
    })

    # --------------------------------------------------------------------------
    # Animation controls
    # --------------------------------------------------------------------------

    observeEvent(input$animate, {
      periods <- available_periods()
      req(periods, length(periods) > 1)

      if (!animation_state$running) {
        animation_state$running <- TRUE

        # Get current position
        current_idx <- which(periods == as.integer(input$period))
        if (length(current_idx) == 0) current_idx <- 1

        # Start animation
        speed <- as.integer(input$animation_speed)

        animation_state$timer <- observe({
          if (animation_state$running) {
            current_idx <- which(periods == as.integer(input$period))
            next_idx <- current_idx + 1

            if (next_idx > length(periods)) {
              next_idx <- 1  # Loop back
            }

            updateSelectInput(
              session, "period",
              selected = as.character(periods[next_idx])
            )

            invalidateLater(speed, session)
          }
        })
      }
    })

    observeEvent(input$stop_animation, {
      animation_state$running <- FALSE
      if (!is.null(animation_state$timer)) {
        animation_state$timer$destroy()
        animation_state$timer <- NULL
      }
    })

    # Stop animation when leaving tab
    onStop(function() {
      animation_state$running <- FALSE
      if (!is.null(animation_state$timer)) {
        animation_state$timer$destroy()
      }
    })

    # --------------------------------------------------------------------------
    # Downloads
    # --------------------------------------------------------------------------

    output$download_csv <- downloadHandler(
      filename = function() {
        paste0("pnadc_geographic_", input$indicator, "_", input$period, ".csv")
      },
      content = function(file) {
        data <- filtered_geo_data()
        data.table::fwrite(data[, .(uf_code, uf_abbrev, uf_name, region, value)], file)
      }
    )

    output$download_png <- downloadHandler(
      filename = function() {
        paste0("pnadc_geographic_", input$indicator, "_", input$period, ".png")
      },
      content = function(file) {
        data <- filtered_geo_data()
        lang_val <- get_lang()

        p <- ggplot(data, aes(x = reorder(uf_abbrev, value), y = value, fill = value)) +
          geom_col() +
          coord_flip() +
          scale_fill_gradient(low = "#c6dbef", high = "#08519c") +
          labs(
            title = output$map_title(),
            subtitle = output$map_subtitle(),
            x = NULL, y = NULL,
            caption = paste("Source: IBGE/PNAD Continua |", Sys.Date())
          ) +
          theme_minimal() +
          theme(
            legend.position = "none",
            plot.title = element_text(face = "bold")
          )

        ggsave(file, plot = p, width = 8, height = 10, dpi = 150, bg = "white")
      },
      contentType = "image/png"
    )

  })
}
