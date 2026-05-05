# ==============================================================================
# PNADCperiods Dashboard - Main Application
# ==============================================================================

# Source global configuration
source("global.R")

# Source modules
source("R/mod_home.R")
source("R/mod_series_explorer.R")
source("R/mod_geographic.R")
source("R/mod_inequality.R")
source("R/mod_poverty.R")
source("R/mod_about.R")

# ==============================================================================
# UI
# ==============================================================================

ui <- page_navbar(
  title = tags$span(
    tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px;",
             alt = "PNADCperiods logo",
             onerror = "this.style.display='none'"),
    "PNADCperiods"
  ),
  id = "navbar",
  theme = app_theme,
  fillable = TRUE,

  # Enable shinyjs and custom CSS/JS
  header = tagList(
    useShinyjs(),
    tags$head(
      # Prevent stale caching by mobile carriers/proxies
      tags$meta(`http-equiv` = "Cache-Control", content = "no-cache, no-store, must-revalidate"),
      tags$meta(`http-equiv` = "Pragma", content = "no-cache"),
      tags$meta(`http-equiv` = "Expires", content = "0"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      # JavaScript for language toggle and slider formatting
      tags$script(HTML("
        // Update language button states
        Shiny.addCustomMessageHandler('update_lang_buttons', function(lang) {
          document.querySelectorAll('.lang-toggle .btn').forEach(function(btn) {
            btn.classList.remove('active');
          });
          var activeBtn = document.getElementById('lang_' + lang);
          if (activeBtn) activeBtn.classList.add('active');
        });

        // Restore language preference on load
        document.addEventListener('DOMContentLoaded', function() {
          var savedLang = localStorage.getItem('pnadc_lang');
          if (savedLang && Shiny.shinyapp) {
            Shiny.setInputValue('selected_lang', savedLang, {priority: 'event'});
          }
        });

        // Save language preference
        function setLanguage(lang) {
          localStorage.setItem('pnadc_lang', lang);
          Shiny.setInputValue('selected_lang', lang, {priority: 'event'});
        }

        // Navigate to a tab by its data-value attribute (used by home page cards)
        function goToTab(tabName) {
          var el = document.querySelector('[data-value=\"' + tabName + '\"]');
          if (el) el.click();
        }

        // Auto-reconnect on disconnect (reload after 3 seconds)
        $(document).on('shiny:disconnected', function(event) {
          setTimeout(function() { location.reload(); }, 3000);
        });

        // Format date slider tooltips to show only month and year
        $(document).on('shiny:inputchanged', function(event) {
          if (event.name && event.name.indexOf('date_slider') !== -1) {
            formatDateSliderTooltips();
          }
        });

        // Also format on slider creation
        $(document).on('shiny:value', function(event) {
          setTimeout(formatDateSliderTooltips, 100);
        });

        function formatDateSliderTooltips() {
          // Find all ion-range-slider instances and update their prettify function
          $('.js-range-slider').each(function() {
            var slider = $(this).data('ionRangeSlider');
            if (slider) {
              slider.update({
                prettify: function(num) {
                  // num is milliseconds since epoch for date sliders
                  var date = new Date(num);
                  var months = ['Jan', 'Fev', 'Mar', 'Abr', 'Mai', 'Jun',
                                'Jul', 'Ago', 'Set', 'Out', 'Nov', 'Dez'];
                  var lang = localStorage.getItem('pnadc_lang') || 'pt';
                  if (lang === 'en') {
                    months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                              'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
                  }
                  return months[date.getMonth()] + ' ' + date.getFullYear();
                }
              });
            }
          });
        }
      "))
    )
  ),

  # Tab 0: Home / Landing Page
  nav_panel(
    title = textOutput("nav_home", inline = TRUE),
    value = "home",
    icon = bs_icon("house"),
    homeUI("home")
  ),

  # Tab 1: Series Explorer
  nav_panel(
    title = textOutput("nav_series_explorer", inline = TRUE),
    value = "explorer",
    icon = bs_icon("graph-up"),
    seriesExplorerUI("explorer")
  ),

 # Tab 2: Geographic Analysis
  nav_panel(
    title = textOutput("nav_geographic", inline = TRUE),
    value = "geographic",
    icon = bs_icon("map"),
    geographicUI("geographic")
  ),

  # Tab 3: Inequality
  nav_panel(
    title = textOutput("nav_inequality", inline = TRUE),
    value = "inequality",
    icon = bs_icon("bar-chart-line"),
    inequalityUI("inequality")
  ),

  # Tab 4: Poverty
  nav_panel(
    title = textOutput("nav_poverty", inline = TRUE),
    value = "poverty",
    icon = bs_icon("currency-dollar"),
    povertyUI("poverty")
  ),

  # Tab 5: About
  nav_panel(
    title = textOutput("nav_about", inline = TRUE),
    value = "about",
    icon = bs_icon("info-circle"),
    aboutUI("about")
  ),

  # Spacer, language toggle, and dark mode toggle
  nav_spacer(),
  nav_item(
    tags$div(
      class = "d-flex align-items-center gap-3",
      # Language toggle with keyboard support and ARIA labels
      tags$div(
        class = "lang-toggle btn-group btn-group-sm",
        role = "group",
        `aria-label` = "Language selection",
        tags$button(
          id = "lang_en",
          type = "button",
          class = "btn btn-outline-light",
          onclick = "setLanguage('en')",
          onkeypress = "if(event.key==='Enter'||event.key===' '){event.preventDefault();setLanguage('en');}",
          `aria-label` = "Switch to English",
          "EN"
        ),
        tags$button(
          id = "lang_pt",
          type = "button",
          class = "btn btn-outline-light active",
          onclick = "setLanguage('pt')",
          onkeypress = "if(event.key==='Enter'||event.key===' '){event.preventDefault();setLanguage('pt');}",
          `aria-label` = "Mudar para Portugues",
          "PT"
        )
      ),
      # Dark mode toggle
      input_dark_mode(id = "dark_mode", mode = "light")
    )
  )
)

# ==============================================================================
# Server
# ==============================================================================

server <- function(input, output, session) {

  # --------------------------------------------------------------------------
  # Language State Management
  # --------------------------------------------------------------------------

  # Reactive language state (default: Portuguese)
  current_lang <- reactiveVal(default_lang)

  # Update language when toggle clicked
  observeEvent(input$selected_lang, {
    current_lang(input$selected_lang)
    # Update button states via JavaScript
    session$sendCustomMessage("update_lang_buttons", input$selected_lang)
  }, ignoreInit = TRUE)

  # --------------------------------------------------------------------------
  # Navigation Labels (i18n)
  # --------------------------------------------------------------------------

  output$nav_home <- renderText({ i18n("nav.home", current_lang()) })
  output$nav_series_explorer <- renderText({ i18n("nav.series_explorer", current_lang()) })
  output$nav_geographic <- renderText({ i18n("nav.geographic", current_lang()) })
  output$nav_inequality <- renderText({ i18n("nav.inequality", current_lang()) })
  output$nav_poverty <- renderText({ i18n("nav.poverty", current_lang()) })
  output$nav_about <- renderText({ i18n("nav.about", current_lang()) })


  # --------------------------------------------------------------------------
  # Shared Data
  # --------------------------------------------------------------------------

  # Initialize reactive values for shared data
  # Small/always-needed data stored directly; large tab-specific data uses lazy loaders
  shared_data <- reactiveValues(
    # Eager data (Home / Series Explorer)
    monthly_sidra = app_data$monthly_sidra,
    rolling_quarters = app_data$rolling_quarters,
    series_metadata = app_data$series_metadata,
    deseasonalized_cache = app_data$deseasonalized_cache,
    last_updated = app_data$last_updated,
    # SIDRA release provenance (Phase 3 / Deploy C)
    sidra_source = app_data$sidra_source,
    sidra_fetched_at = app_data$sidra_fetched_at,
    sidra_latest_ref_month = app_data$sidra_latest_ref_month,
    sidra_log = app_data$sidra_log,
    # Lazy loaders (load on first tab visit, then cached)
    geo_last_updated = app_data$geo_last_updated,
    ineq_last_updated = app_data$ineq_last_updated,
    pov_last_updated = app_data$pov_last_updated,
    get_geographic_data = app_data$get_geographic_data,
    get_brazil_states_sf = app_data$get_brazil_states_sf,
    get_inequality_data = app_data$get_inequality_data,
    get_income_shares_data = app_data$get_income_shares_data,
    get_lorenz_data = app_data$get_lorenz_data,
    get_income_decomposition_data = app_data$get_income_decomposition_data,
    get_quarterly_income_data = app_data$get_quarterly_income_data,
    get_poverty_data = app_data$get_poverty_data
  )

  # --------------------------------------------------------------------------
  # Periodic SIDRA refresh
  # --------------------------------------------------------------------------
  # Workers no shinyapps.io ficam aquecidos por horas; load_app_data() roda
  # uma vez por processo R. Sem o bloco abaixo, o dashboard serve dados do
  # boot do worker indefinidamente, mesmo após o pipeline diário do Actions
  # publicar uma release nova. A cada 30 min, este observer:
  #   1. Lê sidra_log.json da release (~1 KB).
  #   2. Se fetched_at é mais novo que o atual, re-puxa os 4 .qs2.
  #   3. Atualiza shared_data in-place — dispara re-render dos gráficos.
  # Falhas de rede são silenciosas; próximo tick tenta de novo.
  sidra_refresh_timer <- reactiveTimer(30 * 60 * 1000)

  observe({
    sidra_refresh_timer()

    log <- fetch_sidra_log_from_release()
    if (is.null(log) || is.null(log[["fetched_at"]])) return()

    new_at <- tryCatch(
      as.POSIXct(log[["fetched_at"]],
                 format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      error = function(e) NULL
    )
    if (is.null(new_at) || is.na(new_at)) return()

    cur_at <- isolate(shared_data$sidra_fetched_at)
    if (!is.null(cur_at) && !is.na(cur_at) && new_at <= cur_at) return()

    message("[sidra-refresh] release newer (", new_at, " > ",
            cur_at %||% "NA", "); re-fetching qs2 ...")

    staged <- list()
    for (slot in names(RELEASE_QS2_FILES)) {
      res <- fetch_sidra_qs_from_release(RELEASE_QS2_FILES[[slot]],
                                         fallback_path = NULL)
      if (is.null(res$data) || res$source != "release") {
        message("[sidra-refresh] aborted: failed on ", slot)
        return()
      }
      if (slot == "series_metadata") {
        res$data <- apply_dashboard_taxonomy(res$data)
      }
      staged[[slot]] <- res$data
    }

    for (slot in names(staged)) shared_data[[slot]] <- staged[[slot]]
    shared_data$sidra_log              <- log
    shared_data$sidra_fetched_at       <- new_at
    shared_data$sidra_latest_ref_month <- log[["latest_ref_month"]]
    shared_data$sidra_source           <- "release"
    shared_data$last_updated           <- new_at

    message("[sidra-refresh] OK; new fetched_at = ", new_at)
  })

  # --------------------------------------------------------------------------
  # Module Servers (pass language reactive)
  # --------------------------------------------------------------------------

  # Home / Landing Page module
  homeServer("home", shared_data, lang = current_lang)

  # Series Explorer module
  seriesExplorerServer("explorer", shared_data, lang = current_lang)

  # Geographic Analysis module (Phase 3)
  geographicServer("geographic", shared_data, lang = current_lang)

  # Inequality module
  inequalityServer("inequality", shared_data, lang = current_lang)

  # Poverty module
  povertyServer("poverty", shared_data, lang = current_lang)

  # About module
  aboutServer("about", shared_data, lang = current_lang)
}

# ==============================================================================
# Run App
# ==============================================================================

shinyApp(ui = ui, server = server)
