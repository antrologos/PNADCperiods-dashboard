# ==============================================================================
# PNADCperiods Dashboard - Main Application
# ==============================================================================

# Source global configuration
source("global.R")

# Source modules
source("R/mod_series_explorer.R")
source("R/mod_about.R")

# ==============================================================================
# UI
# ==============================================================================

ui <- page_navbar(
  title = tags$span(
    tags$img(src = "logo.png", height = "30px", style = "margin-right: 10px;",
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
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      # JavaScript for language toggle
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
      "))
    )
  ),

  # Tab 1: Series Explorer
  nav_panel(
    title = textOutput("nav_series_explorer", inline = TRUE),
    value = "explorer",
    icon = bs_icon("graph-up"),
    seriesExplorerUI("explorer")
  ),

 # Tab 2: Geographic (placeholder)
  nav_panel(
    title = textOutput("nav_geographic", inline = TRUE),
    value = "geographic",
    icon = bs_icon("map"),
    card(
      card_header(textOutput("header_geographic", inline = TRUE)),
      card_body(
        textOutput("placeholder_geographic")
      )
    )
  ),

  # Tab 3: Inequality (placeholder)
  nav_panel(
    title = textOutput("nav_inequality", inline = TRUE),
    value = "inequality",
    icon = bs_icon("bar-chart-line"),
    card(
      card_header(textOutput("header_inequality", inline = TRUE)),
      card_body(
        textOutput("placeholder_inequality")
      )
    )
  ),

  # Tab 4: Poverty (placeholder)
  nav_panel(
    title = textOutput("nav_poverty", inline = TRUE),
    value = "poverty",
    icon = bs_icon("currency-dollar"),
    card(
      card_header(textOutput("header_poverty", inline = TRUE)),
      card_body(
        textOutput("placeholder_poverty")
      )
    )
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
      # Language toggle
      tags$div(
        class = "lang-toggle btn-group btn-group-sm",
        role = "group",
        tags$button(
          id = "lang_en",
          type = "button",
          class = "btn btn-outline-light",
          onclick = "setLanguage('en')",
          "EN"
        ),
        tags$button(
          id = "lang_pt",
          type = "button",
          class = "btn btn-outline-light active",
          onclick = "setLanguage('pt')",
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

  output$nav_series_explorer <- renderText({ i18n("nav.series_explorer", current_lang()) })
  output$nav_geographic <- renderText({ i18n("nav.geographic", current_lang()) })
  output$nav_inequality <- renderText({ i18n("nav.inequality", current_lang()) })
  output$nav_poverty <- renderText({ i18n("nav.poverty", current_lang()) })
  output$nav_about <- renderText({ i18n("nav.about", current_lang()) })

  # Placeholder tab headers
  output$header_geographic <- renderText({ i18n("geographic.title", current_lang()) })
  output$header_inequality <- renderText({ i18n("inequality.title", current_lang()) })
  output$header_poverty <- renderText({ i18n("poverty.title", current_lang()) })

  # Placeholder tab content
  output$placeholder_geographic <- renderText({
    if (current_lang() == "en") {
      "Geographic visualization will be implemented in Phase 3. This tab will show all labor market indicators by state (UF)."
    } else {
      "A visualização geográfica será implementada na Fase 3. Esta aba mostrará todos os indicadores do mercado de trabalho por estado (UF)."
    }
  })

  output$placeholder_inequality <- renderText({
    if (current_lang() == "en") {
      "Inequality analysis will be implemented in Phase 4. This tab will show Gini, Lorenz curves, and Growth Incidence Curves."
    } else {
      "A análise de desigualdade será implementada na Fase 4. Esta aba mostrará coeficiente de Gini, curvas de Lorenz e Curvas de Incidência do Crescimento."
    }
  })

  output$placeholder_poverty <- renderText({
    if (current_lang() == "en") {
      "Poverty analysis will be implemented in Phase 5. This tab will show FGT poverty indices with World Bank poverty lines."
    } else {
      "A análise de pobreza será implementada na Fase 5. Esta aba mostrará índices FGT de pobreza com linhas de pobreza do Banco Mundial."
    }
  })

  # --------------------------------------------------------------------------
  # Shared Data
  # --------------------------------------------------------------------------

  # Initialize reactive values for shared data
  shared_data <- reactiveValues(
    monthly_sidra = app_data$monthly_sidra,
    rolling_quarters = app_data$rolling_quarters,
    series_metadata = app_data$series_metadata,
    deseasonalized_cache = app_data$deseasonalized_cache,
    last_updated = app_data$last_updated
  )

  # --------------------------------------------------------------------------
  # Module Servers (pass language reactive)
  # --------------------------------------------------------------------------

  # Series Explorer module
  seriesExplorerServer("explorer", shared_data, lang = current_lang)

  # About module
  aboutServer("about", shared_data)
}

# ==============================================================================
# Run App
# ==============================================================================

shinyApp(ui = ui, server = server)
