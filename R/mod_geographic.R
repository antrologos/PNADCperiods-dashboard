# ==============================================================================
# Geographic Analysis Module
# ==============================================================================
#
# Phase 3: Geographic visualization of labor market indicators by state (UF)
#
# This module provides:
#   - Choropleth map of Brazil showing state-level indicators (leaflet)
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

# Geographic indicator definitions (monthly estimates from microdata)
# Organized hierarchically: theme -> theme_category -> indicator
geographic_indicators <- data.table::data.table(
  indicator_id = c(
    # Labor market > Unemployment
    "taxadesocup", "unemployed",
    # Labor market > Participation
    "taxapartic", "nivelocup", "pea", "employed",
    # Labor market > Underutilization
    "taxasubocuphoras", "taxainformal",
    # Labor market > Employment type
    "empregpriv", "empregpubl", "domestico", "empregador", "contapropria",
    # Labor market > Economic sector
    "agropecuaria", "industria", "construcao", "comercio", "servicos",
    # Demographics > Working age
    "pop14mais", "fora_forca",
    # Social protection > Social security
    "taxacontribprev"
  ),
  theme = c(
    # Unemployment
    rep("labor_market", 2),
    # Participation
    rep("labor_market", 4),
    # Underutilization
    rep("labor_market", 2),
    # Employment type
    rep("labor_market", 5),
    # Economic sector
    rep("labor_market", 5),
    # Demographics
    rep("demographics", 2),
    # Social protection
    "social_protection"
  ),
  theme_category = c(
    # Unemployment
    rep("unemployment", 2),
    # Participation
    rep("participation", 4),
    # Underutilization
    rep("underutilization", 2),
    # Employment type
    rep("employment_type", 5),
    # Economic sector
    rep("economic_sector", 5),
    # Demographics
    rep("working_age", 2),
    # Social protection
    "social_security"
  ),
  description_pt = c(
    # Unemployment
    "Taxa de desocupacao",
    "Populacao desocupada",
    # Participation
    "Taxa de participacao na forca de trabalho",
    "Nivel de ocupacao",
    "Forca de trabalho (PEA)",
    "Populacao ocupada",
    # Underutilization
    "Taxa de subocupacao por insuficiencia de horas",
    "Taxa de informalidade",
    # Employment by type
    "Empregados setor privado",
    "Empregados setor publico",
    "Trabalhadores domesticos",
    "Empregadores",
    "Conta propria",
    # Employment by sector
    "Agropecuaria",
    "Industria",
    "Construcao",
    "Comercio",
    "Servicos",
    # Demographics
    "Populacao de 14 anos ou mais",
    "Fora da forca de trabalho",
    # Social protection
    "Taxa de contribuicao previdenciaria"
  ),
  description_en = c(
    # Unemployment
    "Unemployment rate",
    "Unemployed population",
    # Participation
    "Labor force participation rate",
    "Employment-population ratio",
    "Labor force",
    "Employed population",
    # Underutilization
    "Time-related underemployment rate",
    "Informality rate",
    # Employment by type
    "Private sector employees",
    "Public sector employees",
    "Domestic workers",
    "Employers",
    "Self-employed",
    # Employment by sector
    "Agriculture",
    "Industry",
    "Construction",
    "Commerce",
    "Services",
    # Demographics
    "Population aged 14 and over",
    "Not in labor force",
    # Social protection
    "Social security contribution rate"
  ),
  unit = c(
    # Unemployment: rate, level
    "percent", "thousands",
    # Participation: rate, rate, level, level
    "percent", "percent", "thousands", "thousands",
    # Underutilization: rate, rate
    "percent", "percent",
    # Employment type: all levels
    rep("thousands", 5),
    # Economic sector: all levels
    rep("thousands", 5),
    # Demographics: all levels
    rep("thousands", 2),
    # Social protection: rate
    "percent"
  ),
  color_scale = c(
    # Unemployment
    "Reds", "Reds",
    # Participation
    "Blues", "Greens", "Blues", "Greens",
    # Underutilization
    "Oranges", "Purples",
    # Employment type
    rep("Blues", 5),
    # Economic sector
    rep("Greens", 5),
    # Demographics
    rep("Blues", 2),
    # Social protection
    "Blues"
  )
)

# ==============================================================================
# Geographic Indicator Helper Functions (analogous to i18n.R series functions)
# ==============================================================================

#' Get geographic theme choices for dropdown
#' @keywords internal
get_geo_theme_choices <- function(lang = "pt") {
  themes <- unique(geographic_indicators$theme)
  labels <- sapply(themes, function(t) get_theme_label(t, lang))
  setNames(themes, labels)
}

#' Get geographic category choices for dropdown
#' @keywords internal
get_geo_category_choices <- function(selected_theme, lang = "pt") {
  if (is.null(selected_theme) || selected_theme == "") return(character(0))

  filtered <- geographic_indicators[theme == selected_theme]
  if (nrow(filtered) == 0) return(character(0))

  categories <- unique(filtered$theme_category)
  labels <- sapply(categories, function(c) get_theme_category_label(c, lang))
  setNames(categories, labels)
}

#' Get geographic indicator choices for dropdown
#' @keywords internal
get_geo_indicator_choices <- function(selected_theme, selected_category, lang = "pt") {
  if (is.null(selected_theme) || selected_theme == "" ||
      is.null(selected_category) || selected_category == "") {
    return(character(0))
  }

  filtered <- geographic_indicators[theme == selected_theme &
                                      theme_category == selected_category]
  if (nrow(filtered) == 0) return(character(0))

  desc_col <- if (lang == "en") "description_en" else "description_pt"
  setNames(filtered$indicator_id, filtered[[desc_col]])
}

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

      # Theme selection (top level)
      selectInput(
        ns("theme"),
        label = tags$span(style = "font-size: 0.75rem; font-weight: 600;",
                          textOutput(ns("label_theme"), inline = TRUE)),
        choices = NULL,
        selected = NULL
      ),

      # Category selection (middle level)
      selectInput(
        ns("category"),
        label = tags$span(style = "font-size: 0.75rem; font-weight: 600;",
                          textOutput(ns("label_category"), inline = TRUE)),
        choices = NULL,
        selected = NULL
      ),

      # Indicator selection (bottom level)
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

      # View type toggle
      radioButtons(
        ns("view_type"),
        label = NULL,
        choices = c("Map" = "map", "Bar Chart" = "bar"),
        selected = "map",
        inline = TRUE
      ),

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
          # Conditional output: leaflet for map, plotly for bar chart
          conditionalPanel(
            condition = sprintf("input['%s'] == 'map'", ns("view_type")),
            leaflet::leafletOutput(ns("choropleth_map"), height = "500px")
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'bar'", ns("view_type")),
            plotlyOutput(ns("bar_chart"), height = "500px")
          )
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
    output$label_theme <- renderText({ toupper(i18n("controls.theme", get_lang())) })
    output$label_category <- renderText({ toupper(i18n("controls.category", get_lang())) })
    output$label_indicator <- renderText({ toupper(i18n("controls.series", get_lang())) })
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
    # Indicator-level data (for computing global color domain across all periods)
    # --------------------------------------------------------------------------

    indicator_data <- reactive({
      data <- geo_data()
      req(data, input$indicator)
      data[indicator == input$indicator]
    }) |> bindEvent(input$indicator, geo_data())

    # --------------------------------------------------------------------------
    # Global color domain (min/max across ALL periods for selected indicator)
    # This ensures consistent colors when navigating between time periods
    # --------------------------------------------------------------------------

    global_color_domain <- reactive({
      data <- indicator_data()
      req(data, nrow(data) > 0)

      values <- data$value[!is.na(data$value)]
      if (length(values) == 0) return(c(0, 1))

      min_val <- min(values, na.rm = TRUE)
      max_val <- max(values, na.rm = TRUE)

      # Handle case where all values are identical
      if (min_val == max_val) return(c(min_val - 0.1, max_val + 0.1))

      c(min_val, max_val)
    })

    # --------------------------------------------------------------------------
    # Update theme choices (initial load and language change)
    # --------------------------------------------------------------------------

    observe({
      lang_val <- get_lang()
      themes <- get_geo_theme_choices(lang_val)

      updateSelectInput(
        session, "theme",
        choices = themes,
        selected = if ("labor_market" %in% themes) "labor_market" else themes[1]
      )
    }) |> bindEvent(get_lang(), once = FALSE)

    # --------------------------------------------------------------------------
    # Update category choices when theme changes
    # --------------------------------------------------------------------------

    observeEvent(input$theme, {
      req(input$theme)
      lang_val <- get_lang()

      category_choices <- get_geo_category_choices(input$theme, lang_val)

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
        session, "category",
        choices = category_choices,
        selected = default_cat
      )
    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    # --------------------------------------------------------------------------
    # Update indicator choices when category changes
    # --------------------------------------------------------------------------

    observeEvent(input$category, {
      req(input$theme, input$category)
      lang_val <- get_lang()

      indicator_choices <- get_geo_indicator_choices(
        input$theme, input$category, lang_val
      )

      # Default to first indicator or unemployment rate if available
      default_ind <- if (length(indicator_choices) > 0) {
        if ("taxadesocup" %in% indicator_choices) {
          "taxadesocup"
        } else {
          indicator_choices[1]
        }
      } else {
        NULL
      }

      updateSelectInput(
        session, "indicator",
        choices = indicator_choices,
        selected = default_ind
      )
    }, ignoreInit = FALSE, ignoreNULL = TRUE)

    # Update view type labels based on language
    observe({
      lang_val <- get_lang()
      if (lang_val == "en") {
        choices <- c("Map" = "map", "Bar Chart" = "bar")
      } else {
        choices <- c("Mapa" = "map", "Grafico de Barras" = "bar")
      }
      updateRadioButtons(session, "view_type", choices = choices, inline = TRUE)
    }) |> bindEvent(get_lang(), once = FALSE)

    # --------------------------------------------------------------------------
    # Dynamic period slider
    # --------------------------------------------------------------------------

    available_periods <- reactive({
      data <- geo_data()
      if (is.null(data)) return(NULL)
      sort(unique(data$ref_month))
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
      result <- data[ref_month == period_val & indicator == indicator_val]

      # Ensure uf_code is integer for merge with uf_mapping
      if (is.character(result$uf_code)) {
        result[, uf_code := as.integer(uf_code)]
      }

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
        paste("Reference month:", period_label)
      } else {
        paste("Mes de referencia:", period_label)
      }
    })

    # --------------------------------------------------------------------------
    # Output: Choropleth map (leaflet)
    # --------------------------------------------------------------------------

    output$choropleth_map <- leaflet::renderLeaflet({
      data <- filtered_geo_data()

      # Get shapefile from shared_data
      brazil_states_sf <- shared_data$brazil_states_sf

      validate(
        need(!is.null(data), "Loading data..."),
        need(nrow(data) > 0, "No data available for the selected period"),
        need(!is.null(brazil_states_sf), "State shapefile not available")
      )

      lang_val <- get_lang()

      # Get color scale based on indicator
      idx <- which(geographic_indicators$indicator_id == input$indicator)
      color_scale_name <- if (length(idx) > 0) {
        geographic_indicators$color_scale[idx]
      } else {
        "Blues"
      }

      # Add data to shapefile by matching uf_code (avoid merge which corrupts sf)
      sf_data <- brazil_states_sf
      sf_data$uf_code <- as.character(sf_data$uf_code)

      # Create lookup from data
      data_lookup <- setNames(as.list(data$value), as.character(data$uf_code))
      abbrev_lookup <- setNames(as.list(data$uf_abbrev), as.character(data$uf_code))
      name_lookup <- setNames(as.list(data$uf_name), as.character(data$uf_code))
      region_lookup <- setNames(as.list(data$region), as.character(data$uf_code))

      # Add columns by matching - use vapply for guaranteed return type
      sf_data$value <- vapply(sf_data$uf_code, function(x) {
        v <- data_lookup[[x]]
        if (is.null(v)) NA_real_ else as.numeric(v)
      }, FUN.VALUE = numeric(1))
      sf_data$uf_abbrev <- vapply(sf_data$uf_code, function(x) {
        v <- abbrev_lookup[[x]]
        if (is.null(v)) NA_character_ else as.character(v)
      }, FUN.VALUE = character(1))
      sf_data$uf_name <- vapply(sf_data$uf_code, function(x) {
        v <- name_lookup[[x]]
        if (is.null(v)) NA_character_ else as.character(v)
      }, FUN.VALUE = character(1))
      sf_data$region <- vapply(sf_data$uf_code, function(x) {
        v <- region_lookup[[x]]
        if (is.null(v)) NA_character_ else as.character(v)
      }, FUN.VALUE = character(1))

      # Get unit type for this indicator
      unit_type <- if (length(idx) > 0) {
        geographic_indicators$unit[idx]
      } else {
        "percent"
      }
      is_rate <- unit_type == "percent"

      # Create color palette with GLOBAL domain (consistent across all periods)
      color_domain <- global_color_domain()

      pal_func <- switch(color_scale_name,
        "Reds" = leaflet::colorNumeric("Reds", domain = color_domain, na.color = "#ccc"),
        "Greens" = leaflet::colorNumeric("Greens", domain = color_domain, na.color = "#ccc"),
        "Oranges" = leaflet::colorNumeric("Oranges", domain = color_domain, na.color = "#ccc"),
        "Purples" = leaflet::colorNumeric("Purples", domain = color_domain, na.color = "#ccc"),
        leaflet::colorNumeric("Blues", domain = color_domain, na.color = "#ccc")
      )

      # Create labels for hover - format based on unit type
      if (is_rate) {
        value_formatted <- paste0(format_number_i18n(sf_data$value, 1, lang_val), "%")
      } else {
        # For levels, show in thousands with proper formatting
        mil_label <- if(lang_val == "en") " thousand" else " mil"
        value_formatted <- paste0(format_number_i18n(sf_data$value / 1000, 0, lang_val), mil_label)
      }

      labels <- sprintf(
        "<strong>%s - %s</strong><br/>%s: %s<br/>%s: %s",
        sf_data$uf_abbrev,
        sf_data$uf_name,
        if(lang_val == "en") "Value" else "Valor",
        value_formatted,
        if(lang_val == "en") "Region" else "Regiao",
        sf_data$region
      ) |> lapply(htmltools::HTML)

      # Build leaflet map
      m <- leaflet::leaflet(sf_data) |>
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron,
          options = leaflet::providerTileOptions(noWrap = TRUE)
        ) |>
        leaflet::addPolygons(
          fillColor = ~pal_func(value),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "",
          fillOpacity = 0.7,
          highlightOptions = leaflet::highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.8,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = leaflet::labelOptions(
            style = list(
              "font-weight" = "normal",
              padding = "3px 8px"
            ),
            textsize = "13px",
            direction = "auto"
          )
        ) |>
        leaflet::addLegend(
          pal = pal_func,
          values = color_domain,  # Use global domain for consistent legend
          opacity = 0.7,
          title = if(is_rate) "%" else if(lang_val == "en") "thousands" else "milhares",
          position = "bottomright",
          labFormat = if(is_rate) {
            leaflet::labelFormat(suffix = "%")
          } else {
            leaflet::labelFormat(transform = function(x) round(x / 1000, 0))
          }
        ) |>
        leaflet::setView(lng = -55, lat = -15, zoom = 4)

      # Add state labels if enabled
      if (isTRUE(input$show_labels)) {
        # Calculate centroids for labels
        centroids <- suppressWarnings(sf::st_centroid(sf_data))
        coords <- sf::st_coordinates(centroids)
        sf_data$lng <- coords[, 1]
        sf_data$lat <- coords[, 2]

        m <- m |>
          leaflet::addLabelOnlyMarkers(
            data = sf_data,
            lng = ~lng,
            lat = ~lat,
            label = ~uf_abbrev,
            labelOptions = leaflet::labelOptions(
              noHide = TRUE,
              direction = "center",
              textOnly = TRUE,
              style = list(
                "color" = "#333",
                "font-size" = "10px",
                "font-weight" = "bold"
              )
            )
          )
      }

      m
    })

    # --------------------------------------------------------------------------
    # Output: Bar chart (plotly)
    # --------------------------------------------------------------------------

    output$bar_chart <- renderPlotly({
      data <- filtered_geo_data()

      validate(
        need(!is.null(data), "Loading data..."),
        need(nrow(data) > 0, "No data available for the selected period")
      )

      lang_val <- get_lang()

      # Get color scale and unit based on indicator
      idx <- which(geographic_indicators$indicator_id == input$indicator)
      color_scale <- if (length(idx) > 0) {
        geographic_indicators$color_scale[idx]
      } else {
        "Blues"
      }
      unit_type <- if (length(idx) > 0) {
        geographic_indicators$unit[idx]
      } else {
        "percent"
      }
      is_rate <- unit_type == "percent"

      # Get global color domain for consistent colors across periods
      color_domain <- global_color_domain()

      # Create hover text with appropriate formatting
      if (is_rate) {
        data[, hover_text := paste0(
          "<b>", uf_abbrev, " - ", uf_name, "</b><br>",
          if(lang_val == "en") "Value: " else "Valor: ",
          format_number_i18n(value, 1, lang_val), "%<br>",
          if(lang_val == "en") "Region: " else "Regiao: ",
          region
        )]
        value_text <- paste0(format_number_i18n(data$value, 1, lang_val), "%")
      } else {
        mil_label <- if(lang_val == "en") " thousand" else " mil"
        data[, hover_text := paste0(
          "<b>", uf_abbrev, " - ", uf_name, "</b><br>",
          if(lang_val == "en") "Value: " else "Valor: ",
          format_number_i18n(value / 1000, 0, lang_val), mil_label, "<br>",
          if(lang_val == "en") "Region: " else "Regiao: ",
          region
        )]
        value_text <- format_number_i18n(data$value / 1000, 0, lang_val)
      }

      # Sort by value for visualization
      data <- data[order(-value)]

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
          cmin = color_domain[1],  # Fix minimum (global domain)
          cmax = color_domain[2],  # Fix maximum (global domain)
          colorbar = list(
            title = list(
              text = if(is_rate) "%" else if(lang_val == "en") "thousands" else "milhares",
              font = list(size = 11)
            ),
            ticksuffix = if(is_rate) "%" else "",
            tickformat = if(is_rate) ",.1f" else ",.0f",
            len = 0.6
          )
        ),
        text = value_text,
        textposition = "outside",
        hovertext = ~hover_text,
        hoverinfo = "text"
      )

      # Layout with proper locale formatting
      separators <- get_plotly_separators(lang_val)

      p %>% layout(
        xaxis = list(
          title = "",
          tickformat = if(is_rate) ",.1f" else ",.0f",
          ticksuffix = if(is_rate) "%" else "",
          zeroline = FALSE
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 10)
        ),
        separators = separators,
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

    # Helper to get current indicator unit type
    current_unit_type <- reactive({
      req(input$indicator)
      idx <- which(geographic_indicators$indicator_id == input$indicator)
      if (length(idx) > 0) {
        geographic_indicators$unit[idx]
      } else {
        "percent"
      }
    })

    # Format value based on unit type
    format_stat_value <- function(val, lang_val, unit_type) {
      if (unit_type == "percent") {
        paste0(format_number_i18n(val, 1, lang_val), "%")
      } else {
        mil_label <- if(lang_val == "en") " thousand" else " mil"
        paste0(format_number_i18n(val / 1000, 0, lang_val), mil_label)
      }
    }

    output$stat_min <- renderText({
      data <- filtered_geo_data()
      req(data, nrow(data) > 0)
      lang_val <- get_lang()
      unit_type <- current_unit_type()
      min_val <- min(data$value, na.rm = TRUE)
      format_stat_value(min_val, lang_val, unit_type)
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
      unit_type <- current_unit_type()
      max_val <- max(data$value, na.rm = TRUE)
      format_stat_value(max_val, lang_val, unit_type)
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
      unit_type <- current_unit_type()
      # Brazil total (sum for levels, mean for rates)
      if (unit_type == "percent") {
        brazil_val <- mean(data$value, na.rm = TRUE)
      } else {
        brazil_val <- sum(data$value, na.rm = TRUE)
      }
      format_stat_value(brazil_val, lang_val, unit_type)
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
      unit_type <- current_unit_type()
      is_rate <- unit_type == "percent"

      # Select and rename columns, format value based on unit type
      if (is_rate) {
        dt_display <- data[, .(
          State = uf_name,
          Abbrev = uf_abbrev,
          Region = region,
          Value = value
        )]
      } else {
        # For levels, show in thousands
        dt_display <- data[, .(
          State = uf_name,
          Abbrev = uf_abbrev,
          Region = region,
          Value = value / 1000
        )]
      }

      # Sort by value descending (before renaming columns)
      dt_display <- dt_display[order(-Value)]

      # i18n column names
      value_col_name <- if(is_rate) {
        if(lang_val == "en") "Value (%)" else "Valor (%)"
      } else {
        if(lang_val == "en") "Value (thousands)" else "Valor (milhares)"
      }

      if(lang_val == "pt") {
        setnames(dt_display, c("State", "Abbrev", "Region", "Value"),
                 c("Estado", "Sigla", "Regiao", value_col_name))
      } else {
        setnames(dt_display, "Value", value_col_name)
      }

      # Locale-aware number formatting
      big_mark <- if(lang_val == "pt") "." else ","
      dec_mark <- if(lang_val == "pt") "," else "."

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
        formatRound(columns = value_col_name, digits = if(is_rate) 1 else 0,
                    mark = big_mark, dec.mark = dec_mark)
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
      tryCatch({
        isolate({
          animation_state$running <- FALSE
          if (!is.null(animation_state$timer)) {
            animation_state$timer$destroy()
          }
        })
      }, error = function(e) {
        # Ignore errors during cleanup
      })
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

        # Get global color domain and color scale for consistent exports
        color_domain <- global_color_domain()

        # Get color scale name based on indicator
        idx <- which(geographic_indicators$indicator_id == input$indicator)
        color_scale_name <- if (length(idx) > 0) {
          geographic_indicators$color_scale[idx]
        } else {
          "Blues"
        }

        # Map color scale names to ggplot gradient colors
        gradient_colors <- switch(color_scale_name,
          "Reds" = c("#fee0d2", "#a50f15"),
          "Greens" = c("#e5f5e0", "#31a354"),
          "Oranges" = c("#fee6ce", "#e6550d"),
          "Purples" = c("#efedf5", "#756bb1"),
          c("#c6dbef", "#08519c")  # Blues default
        )

        p <- ggplot(data, aes(x = reorder(uf_abbrev, value), y = value, fill = value)) +
          geom_col() +
          coord_flip() +
          scale_fill_gradient(
            low = gradient_colors[1],
            high = gradient_colors[2],
            limits = color_domain  # Fixed limits across all periods
          ) +
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
