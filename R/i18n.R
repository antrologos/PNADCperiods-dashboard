# ==============================================================================
# PNADCperiods Dashboard - Internationalization (i18n) System
# ==============================================================================
#
# Simple translation system using native R lists.
# Supports English (en) and Portuguese (pt).
#
# Usage:
#   i18n("nav.series_explorer", "en")  # Returns "Series Explorer"
#   i18n("nav.series_explorer", "pt")  # Returns "Explorador de Séries"
#
# ==============================================================================

#' Translation dictionary
#' @keywords internal
translations <- list(

  # ============================================================================
  # Navigation & Layout
  # ============================================================================
  nav = list(
    series_explorer = list(en = "Series Explorer", pt = "Explorador de Séries"),
    geographic = list(en = "Geographic", pt = "Geográfico"),
    inequality = list(en = "Inequality", pt = "Desigualdade"),
    poverty = list(en = "Poverty", pt = "Pobreza"),
    about = list(en = "About", pt = "Sobre"),
    app_title = list(en = "PNADCperiods Dashboard", pt = "Painel PNADCperiods")
  ),

  # ============================================================================
  # Series Explorer Controls
  # ============================================================================
  controls = list(
    theme = list(en = "Theme", pt = "Tema"),
    category = list(en = "Category", pt = "Categoria"),
    subcategory = list(en = "Subcategory", pt = "Subcategoria"),
    series = list(en = "Series", pt = "Série"),
    date_range = list(en = "Date Range", pt = "Período"),
    all = list(en = "All", pt = "Todos"),
    show_quarterly = list(en = "Show quarterly overlay", pt = "Mostrar trimestre móvel"),
    show_difference = list(en = "Show difference plot", pt = "Mostrar diferença"),
    deseasonalization = list(en = "Seasonal Adjustment", pt = "Ajuste Sazonal"),
    none = list(en = "None", pt = "Nenhum"),
    x13_arima = list(en = "X-13 ARIMA", pt = "X-13 ARIMA"),
    stl = list(en = "STL", pt = "STL"),
    compare_both = list(en = "Compare both", pt = "Comparar ambos"),
    refresh_data = list(en = "Refresh from SIDRA", pt = "Atualizar do SIDRA"),
    select_theme = list(en = "Select theme...", pt = "Selecione o tema..."),
    select_category = list(en = "Select category...", pt = "Selecione a categoria..."),
    select_subcategory = list(en = "Select subcategory...", pt = "Selecione a subcategoria..."),
    select_series = list(en = "Select series...", pt = "Selecione a série...")
  ),

  # ============================================================================
  # Summary Statistics
  # ============================================================================
  stats = list(
    latest = list(en = "Latest", pt = "Último"),
    yoy_change = list(en = "YoY Change", pt = "Var. Anual"),
    min = list(en = "Minimum", pt = "Mínimo"),
    max = list(en = "Maximum", pt = "Máximo"),
    mean = list(en = "Average", pt = "Média"),
    period = list(en = "Period", pt = "Período"),
    observations = list(en = "Observations", pt = "Observações")
  ),

  # ============================================================================
  # Tooltips (stat definitions)
  # ============================================================================
  tooltips = list(
    latest = list(
      en = "Most recent available value in the series",
      pt = "Valor mais recente disponível na série"
    ),
    yoy_change = list(
      en = "Year-over-year percentage change comparing current value to same period last year",
      pt = "Variação percentual comparando o valor atual com o mesmo período do ano anterior"
    ),
    min = list(
      en = "Lowest value in the selected date range",
      pt = "Menor valor no período selecionado"
    ),
    max = list(
      en = "Highest value in the selected date range",
      pt = "Maior valor no período selecionado"
    ),
    mean = list(
      en = "Simple average of all values in the selected date range",
      pt = "Média simples de todos os valores no período selecionado"
    )
  ),

  # ============================================================================
  # Buttons & Actions
  # ============================================================================
  buttons = list(
    download_csv = list(en = "Download CSV", pt = "Baixar CSV"),
    download_png = list(en = "Download PNG", pt = "Baixar PNG"),
    refresh = list(en = "Refresh", pt = "Atualizar"),
    apply = list(en = "Apply", pt = "Aplicar"),
    reset = list(en = "Reset", pt = "Limpar"),
    show_data = list(en = "Show Data", pt = "Ver Dados"),
    hide_data = list(en = "Hide Data", pt = "Ocultar Dados")
  ),

  # ============================================================================
  # Data Panel
  # ============================================================================
  data_panel = list(
    title = list(en = "Data Table", pt = "Tabela de Dados"),
    date = list(en = "Date", pt = "Data"),
    monthly = list(en = "Monthly", pt = "Mensal"),
    quarterly = list(en = "Quarterly", pt = "Trimestral"),
    adjusted_x13 = list(en = "Adjusted (X-13)", pt = "Ajustado (X-13)"),
    adjusted_stl = list(en = "Adjusted (STL)", pt = "Ajustado (STL)"),
    difference = list(en = "Difference", pt = "Diferença"),
    year = list(en = "Year", pt = "Ano"),
    month = list(en = "Month", pt = "Mês"),
    series_info = list(en = "Series Info", pt = "Informação da Série")
  ),

  # ============================================================================
  # Plot Titles & Labels
  # ============================================================================
  plots = list(
    main_title = list(en = "Monthly Estimate", pt = "Estimativa Mensal"),
    quarterly_label = list(en = "Rolling Quarter", pt = "Trimestre Móvel"),
    monthly_label = list(en = "Monthly", pt = "Mensal"),
    difference_title = list(en = "Monthly vs Quarterly Difference", pt = "Diferença Mensal vs Trimestral"),
    seasonal_title = list(en = "Seasonal Component", pt = "Componente Sazonal"),
    seasonally_adjusted = list(en = "Seasonally Adjusted", pt = "Dessazonalizado"),
    original = list(en = "Original", pt = "Original"),
    source = list(
      en = "Source: IBGE/PNAD Contínua, mensalized by PNADCperiods",
      pt = "Fonte: IBGE/PNAD Contínua, mensalizado pelo PNADCperiods"
    ),
    axis_date = list(en = "Date", pt = "Data"),
    axis_value = list(en = "Value", pt = "Valor"),
    # Deflation reference note
    deflation_ref = list(
      en = "Real values (deflated to %s)",
      pt = "Valores reais (deflacionados para %s)"
    ),
    # Plot line labels (legend names)
    monthly_original = list(en = "Monthly Original", pt = "Mensal Original"),
    monthly_adjusted_x13 = list(en = "Monthly Adjusted (X-13 ARIMA)", pt = "Mensal Ajustado (X-13 ARIMA)"),
    monthly_adjusted_stl = list(en = "Monthly Adjusted (STL)", pt = "Mensal Ajustado (STL)"),
    monthly = list(en = "Monthly", pt = "Mensal"),
    quarterly = list(en = "Quarterly", pt = "Trimestral"),
    monthly_minus_quarterly = list(en = "Monthly - Quarterly", pt = "Mensal - Trimestral"),
    seasonal_x13 = list(en = "Seasonal (X-13)", pt = "Sazonal (X-13)"),
    seasonal_stl = list(en = "Seasonal (STL)", pt = "Sazonal (STL)"),
    x13_arima = list(en = "X-13 ARIMA", pt = "X-13 ARIMA"),
    stl = list(en = "STL", pt = "STL")
  ),

  # ============================================================================
  # Messages & Notifications
  # ============================================================================
  messages = list(
    loading = list(en = "Loading...", pt = "Carregando..."),
    fetching = list(en = "Fetching from SIDRA API...", pt = "Buscando da API SIDRA..."),
    data_updated = list(en = "Data updated successfully!", pt = "Dados atualizados com sucesso!"),
    no_new_data = list(en = "No new data available", pt = "Não há dados novos disponíveis"),
    error_fetch = list(en = "Error fetching data", pt = "Erro ao buscar dados"),
    last_updated = list(en = "Last updated", pt = "Última atualização"),
    select_series = list(en = "Please select a series", pt = "Por favor, selecione uma série"),
    no_data = list(en = "No data available", pt = "Dados não disponíveis"),
    processing = list(en = "Processing...", pt = "Processando..."),
    deseasonalizing = list(en = "Applying seasonal adjustment...", pt = "Aplicando ajuste sazonal...")
  ),

  # ============================================================================
  # Themes (for metadata - top level)
  # ============================================================================
  themes = list(
    labor_market = list(en = "Labor Market", pt = "Mercado de Trabalho"),
    earnings = list(en = "Earnings", pt = "Renda do Trabalho"),
    demographics = list(en = "Population", pt = "População"),
    social_protection = list(en = "Social Protection", pt = "Proteção Social"),
    prices = list(en = "Prices & Deflators", pt = "Preços e Deflatores")
  ),

  # ============================================================================
  # Theme Categories (middle level)
  # ============================================================================
  theme_categories = list(
    # Labor market
    participation = list(en = "Labor Force Participation", pt = "Participação na Força de Trabalho"),
    unemployment = list(en = "Unemployment", pt = "Desemprego"),
    underutilization = list(en = "Underutilization", pt = "Subutilização"),
    employment_type = list(en = "Employment Type", pt = "Tipo de Ocupação"),
    economic_sector = list(en = "Economic Sector", pt = "Setor Econômico"),
    # Earnings (restructured: average + wage_mass with usual/effective subcategories)
    average = list(en = "Average Earnings", pt = "Rendimento Médio"),
    average_usual = list(en = "Average Usual Earnings", pt = "Rendimento Médio Habitual"),
    average_effective = list(en = "Average Effective Earnings", pt = "Rendimento Médio Efetivo"),
    wage_mass = list(en = "Wage Mass", pt = "Massa Salarial"),
    # Demographics
    total = list(en = "Total Population", pt = "População Total"),
    working_age = list(en = "Working Age Population", pt = "População em Idade de Trabalhar"),
    # Social protection
    social_security = list(en = "Social Security", pt = "Previdência Social"),
    # Prices
    deflators = list(en = "Price Indices", pt = "Índices de Preços")
  ),

  # ============================================================================
  # Subcategories (bottom level)
  # ============================================================================
  subcategories = list(
    # Participation/unemployment/underutilization
    rates = list(en = "Rates", pt = "Taxas"),
    levels = list(en = "Levels (thousands)", pt = "Níveis (milhares)"),
    # Employment type subcategories
    employees = list(en = "Employees", pt = "Empregados"),
    domestic = list(en = "Domestic Workers", pt = "Trabalhadores Domésticos"),
    employers = list(en = "Employers", pt = "Empregadores"),
    self_employed = list(en = "Self-Employed", pt = "Conta Própria"),
    family_workers = list(en = "Family Workers", pt = "Trabalhadores Familiares"),
    # Earnings subcategories
    all_jobs = list(en = "All Jobs", pt = "Todos os Trabalhos"),
    by_employment_type = list(en = "By Employment Type", pt = "Por Tipo de Ocupação"),
    by_economic_sector = list(en = "By Economic Sector", pt = "Por Setor Econômico"),
    # Wage mass period
    usual = list(en = "Usual", pt = "Habitual"),
    effective = list(en = "Effective", pt = "Efetivo")
  ),

  # ============================================================================
  # Units
  # ============================================================================
  units = list(
    percent = list(en = "%", pt = "%"),
    millions = list(en = "millions", pt = "milhões"),
    thousands = list(en = "thousands", pt = "milhares"),
    millions_display = list(en = "million", pt = "mi"),
    currency = list(en = "R$", pt = "R$"),
    currency_millions = list(en = "BRL millions", pt = "R$ milhões"),
    index = list(en = "index", pt = "índice"),
    people = list(en = "people", pt = "pessoas")
  ),

  # ============================================================================
  # About Page
  # ============================================================================
  about = list(
    title = list(en = "About PNADCperiods", pt = "Sobre o PNADCperiods"),
    what_is = list(en = "What is mensalization?", pt = "O que é mensalização?"),
    what_is_text = list(
      en = "Mensalization is a methodology that converts Brazil's quarterly PNADC survey data into monthly time series. This enables higher-frequency analysis of labor market indicators.",
      pt = "Mensalização é uma metodologia que converte os dados trimestrais da PNADC em séries temporais mensais. Isso permite análise de indicadores do mercado de trabalho em frequência mais alta."
    ),
    methodology = list(en = "Methodology", pt = "Metodologia"),
    data_sources = list(en = "Data Sources", pt = "Fontes de Dados"),
    how_to_cite = list(en = "How to Cite", pt = "Como Citar"),
    authors = list(en = "Authors", pt = "Autores"),
    links = list(en = "Links", pt = "Links"),
    github = list(en = "GitHub Repository", pt = "Repositório GitHub"),
    documentation = list(en = "Documentation", pt = "Documentação"),
    ibge = list(en = "IBGE Official Sources", pt = "Fontes Oficiais do IBGE")
  ),

  # ============================================================================
  # Geographic Tab
  # ============================================================================
  geographic = list(
    title = list(en = "Geographic Analysis", pt = "Análise Geográfica"),
    select_indicator = list(en = "Select Indicator", pt = "Selecione o Indicador"),
    select_period = list(en = "Select Period", pt = "Selecione o Período"),
    state = list(en = "State", pt = "Estado"),
    region = list(en = "Region", pt = "Região"),
    brazil = list(en = "Brazil", pt = "Brasil"),
    animate = list(en = "Animate", pt = "Animar"),
    play = list(en = "Play", pt = "Reproduzir"),
    pause = list(en = "Pause", pt = "Pausar")
  ),

  # ============================================================================
  # Inequality Tab
  # ============================================================================
  inequality = list(
    title = list(en = "Inequality Analysis", pt = "Análise de Desigualdade"),
    average_income = list(en = "Average Income", pt = "Renda Média"),
    gini = list(en = "Gini Coefficient", pt = "Coeficiente de Gini"),
    lorenz = list(en = "Lorenz Curve", pt = "Curva de Lorenz"),
    gic = list(en = "Growth Incidence Curve", pt = "Curva de Incidência do Crescimento"),
    mean_income = list(en = "Mean Income", pt = "Renda Média"),
    median_income = list(en = "Median Income", pt = "Renda Mediana"),
    real_income = list(en = "Real Income", pt = "Renda Real"),
    national = list(en = "National", pt = "Nacional"),
    by_state = list(en = "By State", pt = "Por Estado"),
    compare_periods = list(en = "Compare Periods", pt = "Comparar Períodos"),
    period_before = list(en = "Before", pt = "Antes"),
    period_after = list(en = "After", pt = "Depois")
  ),

  # ============================================================================
  # Poverty Tab
  # ============================================================================
  poverty = list(
    title = list(en = "Poverty Analysis", pt = "Análise de Pobreza"),
    poverty_line = list(en = "Poverty Line", pt = "Linha de Pobreza"),
    custom_line = list(en = "Custom Line (R$/month)", pt = "Linha Personalizada (R$/mês)"),
    headcount = list(en = "Headcount Ratio (FGT-0)", pt = "Proporção de Pobres (FGT-0)"),
    poverty_gap = list(en = "Poverty Gap (FGT-1)", pt = "Hiato de Pobreza (FGT-1)"),
    severity = list(en = "Poverty Severity (FGT-2)", pt = "Severidade da Pobreza (FGT-2)"),
    extreme_poverty = list(en = "Extreme Poverty (USD 2.15/day)", pt = "Pobreza Extrema (USD 2,15/dia)"),
    lower_middle = list(en = "Lower-Middle Income (USD 3.65/day)", pt = "Renda Média-Baixa (USD 3,65/dia)"),
    upper_middle = list(en = "Upper-Middle Income (USD 6.85/day)", pt = "Renda Média-Alta (USD 6,85/dia)"),
    high_income = list(en = "High Income (USD 8.30/day)", pt = "Renda Alta (USD 8,30/dia)"),
    brazil_official = list(en = "Brazil Official (1/4 MW)", pt = "Brasil Oficial (1/4 SM)"),
    smoothing = list(en = "3-month rolling average", pt = "Média móvel de 3 meses")
  )
)


#' Get translated string
#'
#' Retrieves a translated string from the translations dictionary.
#'
#' @param key Dot-separated path to the translation key (e.g., "nav.series_explorer", "controls.theme")
#' @param lang Language code: "en" for English or "pt" for Portuguese (default: "pt")
#'
#' @return Translated string. Returns the key itself if translation not found.
#'
#' @examples
#' i18n("nav.series_explorer", "en")  # "Series Explorer"
#' i18n("nav.series_explorer", "pt")  # "Explorador de Séries"
#' i18n("controls.theme", "en")       # "Theme"
#'
#' @export
i18n <- function(key, lang = "pt") {
  parts <- strsplit(key, ".", fixed = TRUE)[[1]]
  result <- translations

  for (part in parts) {
    if (is.list(result) && part %in% names(result)) {
      result <- result[[part]]
    } else {
      return(key)  # Return key if path not found
    }
  }

  if (is.list(result) && lang %in% names(result)) {
    return(result[[lang]])
  }

  return(key)
}


#' Get theme label in specified language
#'
#' @param theme_code Theme code (e.g., "labor_market", "earnings")
#' @param lang Language code: "en" or "pt"
#' @return Translated theme label
#'
#' @export
get_theme_label <- function(theme_code, lang = "pt") {
  i18n(paste0("themes.", theme_code), lang)
}


#' Get theme category label in specified language
#'
#' @param category_code Category code (e.g., "participation", "unemployment")
#' @param lang Language code: "en" or "pt"
#' @return Translated category label
#'
#' @export
get_theme_category_label <- function(category_code, lang = "pt") {
  i18n(paste0("theme_categories.", category_code), lang)
}


#' Get subcategory label in specified language
#'
#' @param subcategory_code Subcategory code (e.g., "rates", "levels")
#' @param lang Language code: "en" or "pt"
#' @return Translated subcategory label
#'
#' @export
get_subcategory_label <- function(subcategory_code, lang = "pt") {
  i18n(paste0("subcategories.", subcategory_code), lang)
}


#' Get series description in specified language
#'
#' Returns the appropriate description column based on language.
#'
#' @param metadata Series metadata data.table from get_sidra_series_metadata()
#' @param series_id Name/ID of the series (renamed from series_name to avoid
#'   data.table column name scoping conflict)
#' @param lang Language code: "en" or "pt"
#' @return Description string, or series_id if not found
#'
#' @export
get_series_description <- function(metadata, series_id, lang = "pt") {
  if (is.null(metadata)) return(series_id)

  # Use explicit variable to avoid data.table column name scoping conflict
  target_series <- series_id
  row <- metadata[metadata[["series_name"]] == target_series, ]
  if (nrow(row) == 0) return(series_id)

  col <- if (lang == "en") "description_en" else "description_pt"
  if (col %in% names(row) && !is.na(row[[col]][1])) {
    return(row[[col]][1])
  }

  # Fallback to Portuguese if English not available
  if ("description_pt" %in% names(row) && !is.na(row$description_pt[1])) {
    return(row$description_pt[1])
  }

  return(series_id)
}


#' Create theme choices for dropdown
#'
#' Returns a named vector of theme codes with translated labels.
#'
#' @param metadata Series metadata data.table
#' @param lang Language code: "en" or "pt"
#' @return Named character vector suitable for selectInput choices
#'
#' @export
get_theme_choices <- function(metadata, lang = "pt") {
  if (is.null(metadata)) return(character(0))

  # Check if new hierarchical structure exists, otherwise fall back to old category
  if ("theme" %in% names(metadata)) {
    themes <- unique(metadata$theme)
    themes <- themes[!is.na(themes)]
    if (length(themes) == 0) return(character(0))

    # Create named vector with translated labels
    labels <- sapply(themes, function(t) get_theme_label(t, lang))
    return(setNames(themes, labels))
  } else if ("category" %in% names(metadata)) {
    # Fallback to old category column (for backwards compatibility)
    cats <- unique(metadata$category)
    cats <- cats[!is.na(cats)]
    if (length(cats) == 0) return(character(0))

    # Map old categories to display labels
    old_labels <- c(
      "rate" = if (lang == "en") "Rates" else "Taxas",
      "population" = if (lang == "en") "Population" else "População",
      "employment" = if (lang == "en") "Employment" else "Emprego",
      "sector" = if (lang == "en") "Economic Sectors" else "Setores Econômicos",
      "income_nominal" = if (lang == "en") "Income (Nominal)" else "Renda (Nominal)",
      "income_real" = if (lang == "en") "Income (Real)" else "Renda (Real)",
      "underutilization" = if (lang == "en") "Underutilization" else "Subutilização",
      "price_index" = if (lang == "en") "Price Indices" else "Índices de Preços"
    )
    labels <- ifelse(cats %in% names(old_labels), old_labels[cats], cats)
    return(setNames(cats, labels))
  }

  return(character(0))
}


#' Create theme category choices for dropdown
#'
#' Returns a named vector of category codes with translated labels,
#' filtered by the selected theme.
#'
#' @param metadata Series metadata data.table
#' @param selected_theme Currently selected theme code
#' @param lang Language code: "en" or "pt"
#' @return Named character vector suitable for selectInput choices
#'
#' @export
get_theme_category_choices <- function(metadata, selected_theme, lang = "pt") {
  if (is.null(metadata) || is.null(selected_theme) || selected_theme == "") {
    return(character(0))
  }

  # Check if new hierarchical structure exists
  if ("theme" %in% names(metadata) && "theme_category" %in% names(metadata)) {
    # Filter by theme
    filtered <- metadata[metadata$theme == selected_theme, ]
    if (nrow(filtered) == 0) return(character(0))

    categories <- unique(filtered$theme_category)
    categories <- categories[!is.na(categories)]
    if (length(categories) == 0) return(character(0))

    # Create named vector with translated labels
    labels <- sapply(categories, function(c) get_theme_category_label(c, lang))
    return(setNames(categories, labels))
  } else if ("category" %in% names(metadata)) {
    # Fallback for old structure: no sub-categories, return empty
    # (old structure uses category directly for series selection)
    return(character(0))
  }

  return(character(0))
}


#' Create subcategory choices for dropdown
#'
#' Returns a named vector of subcategory codes with translated labels,
#' filtered by the selected theme and category.
#'
#' @param metadata Series metadata data.table
#' @param selected_theme Currently selected theme code
#' @param selected_category Currently selected category code
#' @param lang Language code: "en" or "pt"
#' @param include_all If TRUE, includes "All" option at the beginning
#' @return Named character vector suitable for selectInput choices
#'
#' @export
get_subcategory_choices <- function(metadata, selected_theme, selected_category,
                                    lang = "pt", include_all = TRUE) {
  if (is.null(metadata) || is.null(selected_theme) || selected_theme == "" ||
      is.null(selected_category) || selected_category == "") {
    return(character(0))
  }

  # Check if new hierarchical structure exists
  if (!("theme" %in% names(metadata) && "subcategory" %in% names(metadata))) {
    # Old structure has no subcategories
    return(character(0))
  }

  # Filter by theme and category
  filtered <- metadata[metadata$theme == selected_theme &
                         metadata$theme_category == selected_category, ]
  if (nrow(filtered) == 0) return(character(0))

  subcategories <- unique(filtered$subcategory)
  subcategories <- subcategories[!is.na(subcategories) & subcategories != ""]

  # If no subcategories, return empty
  if (length(subcategories) == 0) return(character(0))

  # Create named vector with translated labels
  labels <- sapply(subcategories, function(s) get_subcategory_label(s, lang))
  choices <- setNames(subcategories, labels)

  # Add "All" option if requested
  if (include_all) {
    all_label <- i18n("controls.all", lang)
    choices <- c(setNames("", all_label), choices)
  }

  choices
}


#' Create series choices for dropdown
#'
#' Returns a named vector of series names with translated descriptions,
#' filtered by the selected hierarchy levels.
#'
#' @param metadata Series metadata data.table
#' @param selected_theme Currently selected theme code (or old category code)
#' @param selected_category Currently selected category code (optional)
#' @param selected_subcategory Currently selected subcategory code (optional, "" means all)
#' @param lang Language code: "en" or "pt"
#' @return Named character vector suitable for selectInput choices
#'
#' @export
get_series_choices <- function(metadata, selected_theme,
                               selected_category = NULL,
                               selected_subcategory = NULL,
                               lang = "pt") {
  if (is.null(metadata) || is.null(selected_theme) || selected_theme == "") {
    return(character(0))
  }

  # Check if new hierarchical structure exists
  if ("theme" %in% names(metadata)) {
    # New structure: theme -> theme_category -> subcategory
    filtered <- metadata[metadata$theme == selected_theme, ]

    # Apply category filter if provided
    if (!is.null(selected_category) && selected_category != "") {
      filtered <- filtered[filtered$theme_category == selected_category, ]
    }

    # Apply subcategory filter if provided and not "All" (empty string)
    if (!is.null(selected_subcategory) && selected_subcategory != "") {
      filtered <- filtered[filtered$subcategory == selected_subcategory, ]
    }
  } else if ("category" %in% names(metadata)) {
    # Old structure: category only (selected_theme is actually old category)
    filtered <- metadata[metadata$category == selected_theme, ]
  } else {
    return(character(0))
  }

  if (nrow(filtered) == 0) return(character(0))

  # Get appropriate description column
  desc_col <- if (lang == "en") "description_en" else "description_pt"

  # Fallback to description_pt if description_en not available
  if (!desc_col %in% names(filtered)) {
    desc_col <- "description_pt"
  }

  # Final fallback if no description column
  if (!desc_col %in% names(filtered)) {
    desc_col <- "series_name"
  }

  # Create named vector
  choices <- setNames(filtered$series_name, filtered[[desc_col]])
  choices
}


#' Format date according to language
#'
#' @param date Date object or YYYYMM integer
#' @param lang Language code: "en" or "pt"
#' @param format Format type: "short" (Jan 2024), "long" (January 2024), "numeric" (01/2024)
#' @return Formatted date string
#'
#' @export
format_date_i18n <- function(date, lang = "pt", format = "short") {
  # Convert YYYYMM to Date if needed
  if (is.numeric(date)) {
    date <- as.Date(paste0(substr(date, 1, 4), "-", substr(date, 5, 6), "-15"))
  }

  if (lang == "en") {
    if (format == "short") {
      return(format(date, "%b %Y"))
    } else if (format == "long") {
      return(format(date, "%B %Y"))
    } else {
      return(format(date, "%m/%Y"))
    }
  } else {
    # Portuguese month abbreviations
    months_pt_short <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                         "Jul", "Ago", "Set", "Out", "Nov", "Dez")
    months_pt_long <- c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho",
                        "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")

    month_num <- as.integer(format(date, "%m"))
    year <- format(date, "%Y")

    if (format == "short") {
      return(paste(months_pt_short[month_num], year))
    } else if (format == "long") {
      return(paste(months_pt_long[month_num], "de", year))
    } else {
      return(format(date, "%m/%Y"))
    }
  }
}


#' Format number according to language conventions
#'
#' @param x Numeric value
#' @param digits Number of decimal places
#' @param lang Language code: "en" or "pt"
#' @return Formatted number string
#'
#' @export
format_number_i18n <- function(x, digits = 1, lang = "pt") {
  # Vectorized version
  if (length(x) == 0) return(character(0))

  if (lang == "pt") {
    # Brazilian format: 1.234,5
    result <- formatC(x, format = "f", digits = digits, big.mark = ".", decimal.mark = ",")
  } else {
    # English format: 1,234.5
    result <- formatC(x, format = "f", digits = digits, big.mark = ",", decimal.mark = ".")
  }

  # Handle NAs
  result[is.na(x)] <- NA_character_
  result
}


#' Format series value based on unit type and language
#'
#' Formats values appropriately based on series unit type:
#' - percent: 1 decimal, % suffix
#' - thousands: data in thousands, displayed in millions (divided by 1000)
#' - millions_display: data already in millions, 1 decimal with mi/M suffix
#' - currency: R$ prefix, 0 decimals
#' - currency_millions: R$ prefix, 0 decimals
#' - index: 2 decimals
#'
#' @param x Numeric value
#' @param unit Unit type: "percent", "thousands", "millions_display", "currency",
#'   "currency_millions", "index", "millions", "people"
#' @param lang Language code: "en" or "pt"
#' @param include_unit Whether to include unit suffix/prefix
#' @return Formatted string
#'
#' @export
format_series_value <- function(x, unit = "thousands", lang = "pt",
                                include_unit = TRUE) {
  if (is.na(x)) return(NA_character_)

  # Determine formatting based on unit type
  if (unit == "percent") {
    # Rates: 1 decimal place
    formatted <- format_number_i18n(x, digits = 1, lang = lang)
    if (include_unit) {
      formatted <- paste0(formatted, "%")
    }
  } else if (unit == "thousands") {
    # Data is in thousands; display in millions (divide by 1000)
    val_millions <- x / 1000
    formatted <- format_number_i18n(val_millions, digits = 1, lang = lang)
    if (include_unit) {
      unit_label <- if (lang == "en") " million" else " mi"
      formatted <- paste0(formatted, unit_label)
    }
  } else if (unit == "millions_display") {
    # Data already converted to millions; display with 1 decimal
    formatted <- format_number_i18n(x, digits = 1, lang = lang)
    if (include_unit) {
      unit_label <- if (lang == "en") " million" else " mi"
      formatted <- paste0(formatted, unit_label)
    }
  } else if (unit %in% c("millions", "people")) {
    # Population/levels: no decimals, thousands separator
    formatted <- format_number_i18n(x, digits = 0, lang = lang)
    if (include_unit && unit == "millions") {
      unit_label <- if (lang == "en") " million" else " milhões"
      formatted <- paste0(formatted, unit_label)
    }
  } else if (unit %in% c("currency", "currency_millions")) {
    # Currency: R$ prefix, no decimals for display
    formatted <- format_number_i18n(x, digits = 0, lang = lang)
    if (include_unit) {
      formatted <- paste0("R$ ", formatted)
    }
  } else if (unit == "index") {
    # Index: 2 decimals
    formatted <- format_number_i18n(x, digits = 2, lang = lang)
  } else {
    # Default: 1 decimal
    formatted <- format_number_i18n(x, digits = 1, lang = lang)
  }

  formatted
}


#' Get unit type for a series from metadata
#'
#' @param metadata Series metadata data.table
#' @param series_name Name of the series
#' @return Unit type string (default: "thousands")
#'
#' @export
get_series_unit <- function(metadata, series_name) {
  if (is.null(metadata)) return("thousands")

  row <- metadata[metadata$series_name == series_name, ]
  if (nrow(row) == 0) return("thousands")

  if ("unit" %in% names(row) && !is.na(row$unit[1])) {
    return(row$unit[1])
  }

  # Fallback: infer from series name (more specific patterns)
  if (grepl("^taxa|^perc|^nivelocup$|^niveldesocup$", series_name, ignore.case = TRUE)) {
    return("percent")
  } else if (grepl("^massa", series_name, ignore.case = TRUE)) {
    return("currency_millions")
  } else if (grepl("^rend|^rhr", series_name, ignore.case = TRUE)) {
    return("currency")
  } else if (grepl("^ipca|^inpc", series_name, ignore.case = TRUE)) {
    return("index")
  }

  return("thousands")
}


#' Get plotly tickformat for a series unit type
#'
#' Returns D3 format string for plotly tick labels.
#' Note: Use with separators parameter in layout for proper locale.
#'
#' @param unit Unit type
#' @param lang Language code (not used for format, but kept for API consistency)
#' @return D3 format string
#'
#' @export
get_plotly_tickformat <- function(unit = "thousands", lang = "pt") {

  # D3 format strings - separators are handled by layout(separators=...)
  if (unit == "percent") {
    ",.1f"  # 1 decimal for percentages
  } else if (unit == "thousands") {
    ",.0f"  # No decimals for thousands (raw SIDRA data)
  } else if (unit == "millions_display") {
    ",.1f"  # 1 decimal for converted-to-millions values (e.g. 89.3)
  } else if (unit %in% c("millions", "people")) {
    ",.0f"  # No decimals for population/levels
  } else if (unit %in% c("currency", "currency_millions")) {
    ",.0f"  # No decimals for currency (R$ values are typically shown as integers)
  } else if (unit == "index") {
    ",.2f"  # 2 decimals for indices
  } else {
    ",.1f"  # Default: 1 decimal
  }
}


#' Get plotly separators string for locale
#'
#' Returns the separators parameter for plotly layout.
#' Format: "decimal_sep thousands_sep" (2 characters)
#'
#' @param lang Language code: "en" or "pt"
#' @return Separators string for plotly layout
#'
#' @export
get_plotly_separators <- function(lang = "pt") {
  if (lang == "pt") {
    ",."  # Portuguese: comma for decimal, dot for thousands
  } else {
    ".,"  # English: period for decimal, comma for thousands
  }
}


#' Get plotly hoverformat for a series unit type
#'
#' Returns D3 format string for plotly hover labels
#'
#' @param unit Unit type
#' @param lang Language code
#' @return D3 format string
#'
#' @export
get_plotly_hoverformat <- function(unit = "thousands", lang = "pt") {
  # Same as tickformat
  get_plotly_tickformat(unit, lang)
}
