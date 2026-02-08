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
    home = list(en = "Home", pt = "Inicio"),
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
    select_series = list(en = "Select series...", pt = "Selecione a série..."),
    display = list(en = "Display", pt = "Exibição"),
    export = list(en = "Export", pt = "Exportar")
  ),

  # ============================================================================
  # Summary Statistics
  # ============================================================================
  stats = list(
    title = list(en = "Summary Statistics", pt = "Estatísticas Resumidas"),
    latest = list(en = "Latest", pt = "Último"),
    yoy_change = list(en = "YoY Change", pt = "Var. Anual"),
    min = list(en = "Minimum", pt = "Mínimo"),
    max = list(en = "Maximum", pt = "Máximo"),
    mean = list(en = "Average", pt = "Média"),
    period = list(en = "Period", pt = "Período"),
    observations = list(en = "Observations", pt = "Observações"),
    na = list(en = "N/A", pt = "N/D")
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
    download_csv = list(en = "CSV", pt = "CSV"),
    download_png = list(en = "PNG", pt = "PNG"),
    download_excel = list(en = "Excel", pt = "Excel"),
    refresh = list(en = "Refresh", pt = "Atualizar"),
    refresh_tooltip = list(en = "Refresh from SIDRA API", pt = "Atualizar da API SIDRA"),
    apply = list(en = "Apply", pt = "Aplicar"),
    reset = list(en = "Reset", pt = "Limpar"),
    show_data = list(en = "Show Data", pt = "Ver Dados"),
    hide_data = list(en = "Hide Data", pt = "Ocultar Dados"),
    show = list(en = "Show", pt = "Ver")
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
    series_info = list(en = "Series Info", pt = "Informação da Série"),
    no_description = list(en = "No description available", pt = "Descrição não disponível"),
    # DT table localization
    search = list(en = "Filter:", pt = "Filtrar:"),
    length_menu = list(en = "Show _MENU_ entries", pt = "Mostrar _MENU_ registros"),
    info = list(en = "Showing _START_ to _END_ of _TOTAL_ entries", pt = "Mostrando _START_ a _END_ de _TOTAL_ registros"),
    info_empty = list(en = "Showing 0 to 0 of 0 entries", pt = "Mostrando 0 a 0 de 0 registros"),
    info_filtered = list(en = "(filtered from _MAX_ total entries)", pt = "(filtrado de _MAX_ registros totais)"),
    zero_records = list(en = "No matching records found", pt = "Nenhum registro encontrado"),
    paginate_first = list(en = "First", pt = "Primeira"),
    paginate_last = list(en = "Last", pt = "Última"),
    paginate_next = list(en = "Next", pt = "Próxima"),
    paginate_previous = list(en = "Previous", pt = "Anterior")
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
    caption = list(
      en = "Source: PNADCperiods | Generated: %s",
      pt = "Fonte: PNADCperiods | Gerado: %s"
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
    stl = list(en = "STL", pt = "STL"),
    # Difference plot titles
    diff_monthly_quarterly = list(en = "Difference: Monthly - Quarterly", pt = "Diferença: Mensal - Trimestral"),
    diff_seasonal_x13 = list(en = "Seasonal Component (X-13 ARIMA)", pt = "Componente Sazonal (X-13 ARIMA)"),
    diff_seasonal_stl = list(en = "Seasonal Component (STL)", pt = "Componente Sazonal (STL)"),
    diff_seasonal_comparison = list(en = "Seasonal Components Comparison", pt = "Comparação de Componentes Sazonais"),
    diff_generic = list(en = "Difference", pt = "Diferença")
  ),

  # ============================================================================
  # Messages & Notifications
  # ============================================================================
  messages = list(
    loading = list(en = "Loading...", pt = "Carregando..."),
    loading_data = list(en = "Loading data...", pt = "Carregando dados..."),
    fetching = list(en = "Fetching from SIDRA API...", pt = "Buscando da API SIDRA..."),
    data_updated = list(en = "Data updated successfully!", pt = "Dados atualizados com sucesso!"),
    data_updated_detail = list(
      en = "Data updated successfully! Added %d new periods.",
      pt = "Dados atualizados com sucesso! Adicionados %d novos períodos."
    ),
    no_new_data = list(en = "No new data available", pt = "Não há dados novos disponíveis"),
    no_new_data_detail = list(
      en = "No new data available. Latest data point: %s",
      pt = "Não há dados novos disponíveis. Último dado: %s"
    ),
    error_fetch = list(en = "Error fetching data", pt = "Erro ao buscar dados"),
    error_generic = list(en = "An error occurred. Please try again.", pt = "Ocorreu um erro. Por favor, tente novamente."),
    data_dir_not_found = list(en = "Data directory not found. Changes saved to memory only.", pt = "Diretório de dados não encontrado. Alterações salvas apenas na memória."),
    excel_fallback_csv = list(en = "Excel package not available. Downloaded as CSV instead.", pt = "Pacote Excel não disponível. Baixado como CSV."),
    last_updated = list(en = "Last updated", pt = "Última atualização"),
    select_series = list(en = "Please select a series", pt = "Por favor, selecione uma série"),
    no_data = list(en = "No data available", pt = "Dados não disponíveis"),
    no_data_range = list(
      en = "No data available for the selected series and date range",
      pt = "Não há dados disponíveis para a série e período selecionados"
    ),
    not_available = list(en = "Not available", pt = "Não disponível"),
    processing = list(en = "Processing...", pt = "Processando..."),
    deseasonalizing = list(en = "Applying seasonal adjustment...", pt = "Aplicando ajuste sazonal..."),
    quarterly_not_available = list(
      en = "Quarterly data not available for this series",
      pt = "Dados trimestrais não disponíveis para esta série"
    ),
    no_difference_data = list(
      en = "No difference data to display",
      pt = "Não há dados de diferença para exibir"
    ),
    no_difference_range = list(
      en = "No difference data in selected date range",
      pt = "Não há dados de diferença no período selecionado"
    ),
    seasonal_unavailable = list(
      en = "Seasonal adjustment unavailable. Install package(s): %s",
      pt = "Ajuste sazonal indisponível. Instale o(s) pacote(s): %s"
    ),
    refresh_cooldown = list(
      en = "Data was recently refreshed. Please wait %d seconds.",
      pt = "Dados atualizados recentemente. Aguarde %d segundos."
    ),
    package_unavailable = list(
      en = "SIDRA refresh requires %s package (run app locally with devtools::install_github('antrologos/PNADCperiods@feature/sidra-mensalization'))",
      pt = "Atualização SIDRA requer pacote %s (execute o app localmente com devtools::install_github('antrologos/PNADCperiods@feature/sidra-mensalization'))"
    ),
    checking_new_data = list(en = "Checking for new data...", pt = "Verificando novos dados..."),
    querying_api = list(en = "Querying SIDRA API...", pt = "Consultando API SIDRA..."),
    found_new_periods = list(
      en = "Found %d new periods. Mensalizing...",
      pt = "Encontrados %d novos períodos. Mensalizando..."
    ),
    mensalizing = list(en = "Mensalizing new data...", pt = "Mensalizando novos dados..."),
    computing_deseason = list(
      en = "Computing de-seasonalization for top series...",
      pt = "Calculando dessazonalização para as principais séries..."
    ),
    saving_data = list(en = "Saving updated data...", pt = "Salvando dados atualizados..."),
    x13_not_available = list(
      en = "X-13 adjustment not available",
      pt = "Ajuste X-13 não disponível"
    ),
    stl_not_available = list(
      en = "STL adjustment not available",
      pt = "Ajuste STL não disponível"
    ),
    no_x13_data = list(
      en = "No X-13 adjusted data available",
      pt = "Não há dados ajustados por X-13 disponíveis"
    ),
    no_stl_data = list(
      en = "No STL adjusted data available",
      pt = "Não há dados ajustados por STL disponíveis"
    ),
    no_seasonal_range = list(
      en = "No seasonal data in selected date range",
      pt = "Não há dados sazonais no período selecionado"
    ),
    updating_seasonal = list(
      en = "Updating seasonal adjustments...",
      pt = "Atualizando ajustes sazonais..."
    )
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
    title = list(
      en = "About PNADCperiods Dashboard",
      pt = "Sobre o Painel PNADCperiods"
    ),
    what_is = list(en = "What is Mensalization?", pt = "O que e Mensalizacao?"),
    what_is_text = list(
      en = "Mensalization is a statistical technique that converts Brazil's quarterly PNADC survey data into monthly estimates. The official IBGE releases report 'rolling quarter' estimates (e.g., Jan-Feb-Mar average reported for March), which smooth out short-term dynamics. This dashboard presents the monthly estimates recovered using the PNADCperiods R package (Barbosa & Hecksher, 2026), which implements the mensalization methodology developed by Marcos Hecksher (Hecksher, 2020 --- IPEA Nota Tecnica Disoc n. 62 and n. 87; Carta de Conjuntura v. 47).",
      pt = "Mensalizacao e uma tecnica estatistica que converte os dados trimestrais da PNADC em estimativas mensais. As divulgacoes oficiais do IBGE reportam estimativas de 'trimestre movel' (ex: media Jan-Fev-Mar reportada para Marco), que suavizam dinamicas de curto prazo. Este painel apresenta as estimativas mensais recuperadas usando o pacote R PNADCperiods (Barbosa & Hecksher, 2026), que implementa a metodologia de mensalizacao desenvolvida por Marcos Hecksher (Hecksher, 2020 --- IPEA Nota Tecnica Disoc n. 62 e n. 87; Carta de Conjuntura v. 47)."
    ),
    methodology = list(en = "Methodology", pt = "Metodologia"),
    methodology_text = list(
      en = "The mensalization algorithm uses household panel information and birthday constraints from the PNADC questionnaire to identify which specific month each interview refers to. The process involves:",
      pt = "O algoritmo de mensalizacao usa informacoes do painel domiciliar e restricoes de aniversario do questionario da PNADC para identificar a qual mes especifico cada entrevista se refere. O processo envolve:"
    ),
    methodology_step1 = list(
      en = "Building a crosswalk that maps observations to reference months",
      pt = "Construir uma tabela de correspondencia que mapeia observacoes aos meses de referencia"
    ),
    methodology_step2 = list(
      en = "Calibrating survey weights to match official population totals",
      pt = "Calibrar os pesos amostrais para corresponder aos totais populacionais oficiais"
    ),
    methodology_step3 = list(
      en = "Computing monthly aggregates using the calibrated weights",
      pt = "Calcular agregados mensais usando os pesos calibrados"
    ),
    data_sources = list(en = "Data Sources", pt = "Fontes de Dados"),
    data_source_sidra = list(
      en = "Official IBGE statistics (86 series)",
      pt = "Estatisticas oficiais do IBGE (86 series)"
    ),
    data_source_microdata = list(
      en = "Quarterly and annual survey data",
      pt = "Dados de pesquisa trimestrais e anuais"
    ),
    data_freshness = list(en = "Data Freshness", pt = "Atualizacao dos Dados"),
    sidra_data = list(en = "SIDRA data:", pt = "Dados SIDRA:"),
    not_available_refresh = list(
      en = "Not available - click 'Refresh' in Series Explorer",
      pt = "Nao disponivel - clique em 'Atualizar' no Explorador de Series"
    ),
    how_to_cite = list(en = "How to Cite", pt = "Como Citar"),
    cite_note = list(
      en = "Please cite the R package (Barbosa & Hecksher, 2026) and the methodology papers: Hecksher (2020, IPEA Nota Tecnica Disoc n. 62 and n. 87; Carta de Conjuntura v. 47).",
      pt = "Por favor, cite o pacote R (Barbosa & Hecksher, 2026) e os artigos metodologicos: Hecksher (2020, IPEA Nota Tecnica Disoc n. 62 e n. 87; Carta de Conjuntura v. 47)."
    ),
    methodology_refs_header = list(
      en = "Methodology References",
      pt = "Referencias Metodologicas"
    ),
    links = list(en = "Links", pt = "Links"),
    github = list(en = "GitHub Repository", pt = "Repositorio GitHub"),
    documentation = list(en = "Documentation", pt = "Documentacao"),
    ibge_sidra = list(en = "IBGE SIDRA", pt = "SIDRA IBGE"),
    disclaimer = list(en = "Disclaimer", pt = "Aviso Legal"),
    disclaimer_text = list(
      en = "This dashboard presents estimates derived from official IBGE survey data. The mensalization methodology is an academic contribution and the monthly estimates are not official IBGE statistics. Users should consider the uncertainty inherent in survey-based estimates, especially for sub-national or sub-group analyses where sample sizes may be small.",
      pt = "Este painel apresenta estimativas derivadas de dados oficiais de pesquisas do IBGE. A metodologia de mensalizacao e uma contribuicao academica e as estimativas mensais nao sao estatisticas oficiais do IBGE. Os usuarios devem considerar a incerteza inerente a estimativas baseadas em pesquisas, especialmente para analises subnacionais ou de subgrupos onde os tamanhos amostrais podem ser pequenos."
    ),
    disclaimer_official = list(
      en = "For official statistics, please consult the IBGE website directly.",
      pt = "Para estatisticas oficiais, consulte diretamente o site do IBGE."
    )
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
    brazil = list(en = "State Avg.", pt = "Média Estados"),
    animate = list(en = "Animate", pt = "Animar"),
    play = list(en = "Play", pt = "Reproduzir"),
    pause = list(en = "Pause", pt = "Pausar"),
    # Display options
    options = list(en = "OPTIONS", pt = "OPCOES"),
    show_labels = list(en = "Show state labels", pt = "Mostrar siglas dos estados"),
    show_table = list(en = "Show state table", pt = "Mostrar tabela por estado"),
    summary = list(en = "Regional Summary", pt = "Resumo Regional"),
    table_header = list(en = "State Data", pt = "Dados por Estado"),
    lowest = list(en = "Lowest", pt = "Menor"),
    highest = list(en = "Highest", pt = "Maior"),
    dispersion = list(en = "Dispersion (CV)", pt = "Dispersao (CV)"),
    export = list(en = "EXPORT", pt = "EXPORTAR"),
    # Animation speed
    speed_slow = list(en = "Slow", pt = "Lento"),
    speed_normal = list(en = "Normal", pt = "Normal"),
    speed_fast = list(en = "Fast", pt = "Rapido"),
    # View type
    view_map = list(en = "Map", pt = "Mapa"),
    view_bar = list(en = "Bar Chart", pt = "Grafico de Barras")
  ),

  # ============================================================================
  # Inequality Tab
  # ============================================================================
  inequality = list(
    title = list(en = "Inequality Analysis", pt = "Análise de Desigualdade"),
    info_banner = list(
      en = "Monthly inequality estimates from mensalized annual PNADC microdata (2015-2024). All income values deflated to December 2025 BRL.",
      pt = "Estimativas mensais de desigualdade a partir de microdados mensalizados da PNADC anual (2015-2024). Todos os valores de renda deflacionados para dezembro de 2025."
    ),
    # Themes
    theme = list(en = "Theme", pt = "Tema"),
    theme_income_level = list(en = "Income Level", pt = "Nível de Renda"),
    theme_distribution = list(en = "Distribution", pt = "Distribuição"),
    theme_shares = list(en = "Income Shares", pt = "Parcelas de Renda"),
    theme_lorenz = list(en = "Lorenz Curve", pt = "Curva de Lorenz"),
    theme_decomposition = list(en = "Decomposition", pt = "Decomposição"),
    # Measures
    measure = list(en = "Measure", pt = "Medida"),
    gini = list(en = "Gini Coefficient", pt = "Coeficiente de Gini"),
    palma = list(en = "Palma Ratio", pt = "Razão de Palma"),
    p90p10 = list(en = "P90/P10 Ratio", pt = "Razão P90/P10"),
    p90p50 = list(en = "P90/P50 Ratio", pt = "Razão P90/P50"),
    p50p10 = list(en = "P50/P10 Ratio", pt = "Razão P50/P10"),
    top1_share = list(en = "Top 1% Share", pt = "Parcela do Top 1%"),
    top5_share = list(en = "Top 5% Share", pt = "Parcela do Top 5%"),
    top10_share = list(en = "Top 10% Share", pt = "Parcela do Top 10%"),
    bottom50_share = list(en = "Bottom 50% Share", pt = "Parcela dos 50% Inferiores"),
    mean_income = list(en = "Mean Income", pt = "Renda Média"),
    median_income = list(en = "Median Income", pt = "Renda Mediana"),
    # Shares
    quintile_shares = list(en = "Quintile Shares", pt = "Parcelas por Quintil"),
    decile_shares = list(en = "Decile Shares", pt = "Parcelas por Decil"),
    # Lorenz
    lorenz = list(en = "Lorenz Curve", pt = "Curva de Lorenz"),
    equality_line = list(en = "Perfect Equality", pt = "Igualdade Perfeita"),
    cumulative_pop = list(en = "Cumulative Population Share", pt = "Parcela Acumulada da População"),
    cumulative_income = list(en = "Cumulative Income Share", pt = "Parcela Acumulada da Renda"),
    # Decomposition
    gini_by_source = list(en = "Gini by Income Source", pt = "Gini por Fonte de Renda"),
    income_shares_by_source = list(en = "Income Shares by Source", pt = "Parcelas por Fonte de Renda"),
    gic = list(en = "Growth Incidence Curve", pt = "Curva de Incidência do Crescimento"),
    # Income sources
    source_labor = list(en = "Labor Income", pt = "Renda do Trabalho"),
    source_pension = list(en = "Pensions", pt = "Previdência"),
    source_bpc = list(en = "BPC-LOAS", pt = "BPC-LOAS"),
    source_bolsa_familia = list(en = "Bolsa Família", pt = "Bolsa Família"),
    source_other_programs = list(en = "Other Social Programs", pt = "Outros Programas Sociais"),
    source_unemployment_insurance = list(en = "Unemployment Insurance", pt = "Seguro Desemprego"),
    source_rental = list(en = "Rental Income", pt = "Aluguéis"),
    source_other = list(en = "Other Income", pt = "Outras Rendas"),
    concentration = list(en = "Concentration Coeff.", pt = "Coef. de Concentração"),
    income_share = list(en = "Income Share", pt = "Parcela da Renda"),
    contribution = list(en = "Contribution to Gini", pt = "Contribuição ao Gini"),
    # Period comparison
    compare_periods = list(en = "Compare Periods", pt = "Comparar Períodos"),
    period_before = list(en = "Period 1", pt = "Período 1"),
    period_after = list(en = "Period 2", pt = "Período 2"),
    select_period = list(en = "Select period...", pt = "Selecione o período..."),
    # Display
    smoothing = list(en = "Smoothing", pt = "Suavização"),
    smoothing_raw = list(en = "Raw monthly", pt = "Mensal bruto"),
    smoothing_3mo = list(en = "3-month average", pt = "Média 3 meses"),
    show_table = list(en = "Show data table", pt = "Mostrar tabela"),
    deflation_ref = list(en = "Dec 2025 BRL", pt = "R$ Dez 2025"),
    real_income = list(en = "Real Income", pt = "Renda Real")
  ),

  # ============================================================================
  # Poverty Tab
  # ============================================================================
  poverty = list(
    title = list(en = "Poverty Analysis", pt = "Análise de Pobreza"),
    info_banner = list(
      en = "Monthly poverty estimates from mensalized annual PNADC microdata (2015-2024). World Bank poverty lines (June 2025 update, 2021 PPP). All values in December 2025 BRL.",
      pt = "Estimativas mensais de pobreza a partir de microdados mensalizados da PNADC anual (2015-2024). Linhas de pobreza do Banco Mundial (atualização junho 2025, PPP 2021). Todos os valores em R$ de dezembro de 2025."
    ),
    # Poverty lines
    poverty_line = list(en = "Poverty Line", pt = "Linha de Pobreza"),
    line_300 = list(en = "Extreme ($3.00/day PPP)", pt = "Extrema ($3,00/dia PPP)"),
    line_420 = list(en = "Lower-Middle ($4.20/day PPP)", pt = "Média-Baixa ($4,20/dia PPP)"),
    line_830 = list(en = "Upper-Middle ($8.30/day PPP)", pt = "Média-Alta ($8,30/dia PPP)"),
    line_quarter_mw = list(en = "1/4 Minimum Wage", pt = "1/4 Salário Mínimo"),
    line_half_mw = list(en = "1/2 Minimum Wage", pt = "1/2 Salário Mínimo"),
    custom_line = list(en = "Custom (R$/month)", pt = "Personalizada (R$/mês)"),
    # Measures
    measure = list(en = "Measure", pt = "Medida"),
    headcount = list(en = "Headcount Ratio (FGT-0)", pt = "Proporção de Pobres (FGT-0)"),
    poverty_gap = list(en = "Poverty Gap (FGT-1)", pt = "Hiato de Pobreza (FGT-1)"),
    severity = list(en = "Poverty Severity (FGT-2)", pt = "Severidade da Pobreza (FGT-2)"),
    show_all_fgt = list(en = "Show All Three", pt = "Mostrar as Três"),
    # Stats
    n_poor = list(en = "Number of Poor", pt = "Número de Pobres"),
    population = list(en = "Population", pt = "População"),
    mean_income_poor = list(en = "Mean Income of Poor", pt = "Renda Média dos Pobres"),
    # Display
    smoothing = list(en = "Smoothing", pt = "Suavização"),
    smoothing_raw = list(en = "Raw monthly", pt = "Mensal bruto"),
    smoothing_3mo = list(en = "3-month average", pt = "Média 3 meses"),
    show_table = list(en = "Show data table", pt = "Mostrar tabela"),
    deflation_ref = list(en = "Dec 2025 BRL", pt = "R$ Dez 2025")
  ),

  # ============================================================================
  # Demographic Breakdowns (shared by Inequality & Poverty)
  # ============================================================================
  demographics = list(
    breakdown = list(en = "Breakdown", pt = "Desagregação"),
    overall = list(en = "Overall (National)", pt = "Total (Nacional)"),
    by_sex = list(en = "By Sex", pt = "Por Sexo"),
    by_race = list(en = "By Race/Color", pt = "Por Raça/Cor"),
    by_education = list(en = "By Education", pt = "Por Escolaridade"),
    by_region = list(en = "By Region", pt = "Por Região"),
    by_uf = list(en = "By State", pt = "Por UF"),
    by_urban_rural = list(en = "By Urban/Rural", pt = "Por Situação Censitária"),
    by_age_group = list(en = "By Age Group", pt = "Por Faixa Etária"),
    # Values
    male = list(en = "Men", pt = "Homens"),
    female = list(en = "Women", pt = "Mulheres"),
    white = list(en = "White", pt = "Branca"),
    black = list(en = "Black", pt = "Preta"),
    brown = list(en = "Brown", pt = "Parda"),
    asian = list(en = "Asian", pt = "Amarela"),
    indigenous = list(en = "Indigenous", pt = "Indígena"),
    no_education = list(en = "No Education", pt = "Sem Instrução"),
    primary = list(en = "Primary", pt = "Fundamental"),
    secondary = list(en = "Secondary", pt = "Médio"),
    tertiary = list(en = "Tertiary", pt = "Superior"),
    north = list(en = "North", pt = "Norte"),
    northeast = list(en = "Northeast", pt = "Nordeste"),
    southeast = list(en = "Southeast", pt = "Sudeste"),
    south = list(en = "South", pt = "Sul"),
    center_west = list(en = "Center-West", pt = "Centro-Oeste"),
    urban = list(en = "Urban", pt = "Urbano"),
    rural = list(en = "Rural", pt = "Rural"),
    age_0_17 = list(en = "0-17 years", pt = "0-17 anos"),
    age_18_29 = list(en = "18-29 years", pt = "18-29 anos"),
    age_30_59 = list(en = "30-59 years", pt = "30-59 anos"),
    age_60plus = list(en = "60+ years", pt = "60+ anos"),
    nacional = list(en = "National", pt = "Nacional"),
    select_groups = list(en = "Select groups", pt = "Selecionar grupos")
  ),

  # ============================================================================
  # Controls (shared)
  # ============================================================================
  controls_shared = list(
    select_period = list(en = "Select period", pt = "Selecionar periodo")
  ),

  # ============================================================================
  # Home / Landing Page
  # ============================================================================
  home = list(
    hero_title = list(
      en = "PNADCperiods Dashboard",
      pt = "Painel PNADCperiods"
    ),
    hero_subtitle = list(
      en = "Sub-quarterly period identification and monthly estimates from Brazil's PNADC survey",
      pt = "Identificacao de periodos sub-trimestrais e estimativas mensais a partir da PNADC"
    ),
    what_is = list(
      en = "What is Mensalization?",
      pt = "O que e Mensalizacao?"
    ),
    what_is_text = list(
      en = "The PNADCperiods R package (Barbosa & Hecksher, 2026) identifies sub-quarterly time periods in PNADC microdata --- determining the exact month, and experimentally the fortnight and week, of each interview. This enables two main functionalities: (1) direct analysis of microdata at monthly frequency with calibrated weights, and (2) mensalization of SIDRA rolling quarter series (e.g., recovering the monthly unemployment rate from the Jan-Feb-Mar average reported for March). Both approaches reveal short-term dynamics hidden by quarterly smoothing. The methodology was developed by Marcos Hecksher (Hecksher, 2020 --- IPEA Nota Tecnica Disoc n. 62 and n. 87; Carta de Conjuntura v. 47).",
      pt = "O pacote R PNADCperiods (Barbosa & Hecksher, 2026) identifica periodos sub-trimestrais nos microdados da PNADC --- determinando o mes exato, e experimentalmente a quinzena e a semana, de cada entrevista. Isso possibilita duas funcionalidades principais: (1) analise direta dos microdados em frequencia mensal com pesos calibrados, e (2) mensalizacao das series de trimestres moveis do SIDRA (ex: recuperar a taxa de desemprego mensal a partir da media Jan-Fev-Mar reportada para Marco). Ambas as abordagens revelam dinamicas de curto prazo ocultas pela suavizacao trimestral. A metodologia foi desenvolvida por Marcos Hecksher (Hecksher, 2020 --- IPEA Nota Tecnica Disoc n. 62 e n. 87; Carta de Conjuntura v. 47)."
    ),
    feature_series_title = list(
      en = "SIDRA Series Mensalization",
      pt = "Mensalizacao de Series SIDRA"
    ),
    feature_series_desc = list(
      en = "Explore 86+ mensalized time series recovered from SIDRA rolling quarter aggregates, with seasonal adjustment and interactive visualization.",
      pt = "Explore mais de 86 series temporais mensalizadas recuperadas dos agregados trimestrais moveis do SIDRA, com ajuste sazonal e visualizacao interativa."
    ),
    feature_inequality_title = list(
      en = "Inequality Analysis",
      pt = "Analise de Desigualdade"
    ),
    feature_inequality_desc = list(
      en = "Monthly Gini, Palma ratio, income shares, and Lorenz curves from mensalized PNADC microdata (2012-2024), with demographic breakdowns.",
      pt = "Gini mensal, razao de Palma, participacao na renda e curvas de Lorenz a partir de microdados mensalizados da PNADC (2012-2024), com desagregacoes demograficas."
    ),
    feature_poverty_title = list(
      en = "Poverty Analysis",
      pt = "Analise de Pobreza"
    ),
    feature_poverty_desc = list(
      en = "Monthly poverty rates (FGT family) using World Bank and Brazilian poverty lines, from mensalized PNADC microdata with demographic breakdowns.",
      pt = "Taxas de pobreza mensais (familia FGT) usando linhas do Banco Mundial e brasileiras, a partir de microdados mensalizados da PNADC com desagregacoes demograficas."
    ),
    feature_geo_title = list(
      en = "Geographic Analysis",
      pt = "Analise Geografica"
    ),
    feature_geo_desc = list(
      en = "State-level labor market indicators with interactive choropleth maps and bar charts.",
      pt = "Indicadores do mercado de trabalho por estado com mapas coropleticos interativos e graficos de barras."
    ),
    feature_package_title = list(
      en = "Open Source Package",
      pt = "Pacote Open Source"
    ),
    feature_package_desc = list(
      en = "Built on the PNADCperiods R package. Full methodology and documentation available on GitHub.",
      pt = "Construido sobre o pacote R PNADCperiods. Metodologia completa e documentacao disponivel no GitHub."
    ),
    btn_methodology = list(
      en = "Methodology",
      pt = "Metodologia"
    ),
    btn_documentation = list(
      en = "Documentation",
      pt = "Documentacao"
    ),
    footer_package_label = list(
      en = "[Package]",
      pt = "[Pacote]"
    ),
    footer_methodology_label = list(
      en = "[Original Methodology]",
      pt = "[Metodologia Original]"
    ),
    footer_disclaimer = list(
      en = "Monthly estimates are not official IBGE statistics. For official data, visit sidra.ibge.gov.br.",
      pt = "As estimativas mensais nao sao estatisticas oficiais do IBGE. Para dados oficiais, visite sidra.ibge.gov.br."
    )
  ),

  # ============================================================================
  # Methodology Info Banners
  # ============================================================================
  banners = list(
    series_text = list(
      en = "Estimates based on the SIDRA series mensalization module of PNADCperiods. Monthly values are recovered from rolling quarter aggregates published by IBGE.",
      pt = "Estimativas baseadas no modulo de mensalizacao de series SIDRA do PNADCperiods. Os valores mensais sao recuperados dos agregados trimestrais moveis publicados pelo IBGE."
    ),
    series_link = list(
      en = "Learn more",
      pt = "Saiba mais"
    ),
    geographic_text = list(
      en = "State-level estimates based on mensalized PNADC microdata with calibrated survey weights.",
      pt = "Estimativas por estado baseadas em microdados mensalizados da PNADC com pesos amostrais calibrados."
    ),
    geographic_link = list(
      en = "Learn more",
      pt = "Saiba mais"
    )
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
    # BUT only if the data has non-NA subcategories - this prevents stale
    # subcategory values from filtering out categories that don't use subcategories
    if (!is.null(selected_subcategory) && selected_subcategory != "") {
      # Check if any rows have non-NA subcategories
      has_subcategories <- any(!is.na(filtered$subcategory))
      if (has_subcategories) {
        filtered <- filtered[!is.na(filtered$subcategory) &
                             filtered$subcategory == selected_subcategory, ]
      }
      # If no rows have subcategories, skip the filter entirely
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
    # Handle negative values correctly: -R$ 1.234 instead of R$ -1.234
    is_negative <- x < 0
    formatted <- format_number_i18n(abs(x), digits = 0, lang = lang)
    if (include_unit) {
      prefix <- if (is_negative) "-R$ " else "R$ "
      formatted <- paste0(prefix, formatted)
    } else if (is_negative) {
      formatted <- paste0("-", formatted)
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
#' @param series_id Series identifier (series_name column value)
#' @return Unit type string (default: "thousands")
#'
#' @export
get_series_unit <- function(metadata, series_id) {
  if (is.null(metadata)) return("thousands")

  # Use which() to avoid data.table scoping issues
  # (variable named 'series_name' would conflict with column name)
  idx <- which(metadata$series_name == series_id)
  if (length(idx) == 0) return("thousands")

  row <- metadata[idx, ]

  if ("unit" %in% names(row) && !is.na(row$unit[1])) {
    return(row$unit[1])
  }

  # Fallback: infer from series name (more specific patterns)
  if (grepl("^taxa|^perc|^nivelocup$|^niveldesocup$", series_id, ignore.case = TRUE)) {
    return("percent")
  } else if (grepl("^massa", series_id, ignore.case = TRUE)) {
    return("currency_millions")
  } else if (grepl("^rend|^rhr", series_id, ignore.case = TRUE)) {
    return("currency")
  } else if (grepl("^ipca|^inpc", series_id, ignore.case = TRUE)) {
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
