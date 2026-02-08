# ==============================================================================
# Home / Landing Page Module
# ==============================================================================

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

homeUI <- function(id) {
  ns <- NS(id)

  div(
    class = "container-fluid home-page",
    style = "max-width: 1100px; margin: 0 auto;",

    # Hero Section
    div(
      class = "home-hero text-center",
      tags$h1(class = "home-title",
              textOutput(ns("hero_title"), inline = TRUE)),
      tags$p(class = "home-subtitle",
             textOutput(ns("hero_subtitle"), inline = TRUE))
    ),

    # "What is Mensalization?" Card
    div(
      class = "card mb-4 home-what-is",
      div(
        class = "card-header",
        tags$span(style = "font-weight: 600;",
                  bs_icon("lightbulb"), " ",
                  textOutput(ns("what_is_header"), inline = TRUE))
      ),
      div(
        class = "card-body",
        tags$p(
          style = "font-size: 0.9rem; line-height: 1.6; margin-bottom: 0;",
          textOutput(ns("what_is_text"), inline = TRUE)
        )
      )
    ),

    # Feature Cards Row
    div(
      class = "row g-4 mb-4",

      # Feature 1: Series Explorer
      div(
        class = "col-md-4",
        div(
          class = "card h-100 home-feature-card",
          div(
            class = "card-body text-center",
            div(
              class = "home-feature-icon",
              bs_icon("graph-up")
            ),
            tags$h5(class = "card-title mt-3",
                    textOutput(ns("feature_series_title"), inline = TRUE)),
            tags$p(class = "card-text text-muted",
                   style = "font-size: 0.85rem;",
                   textOutput(ns("feature_series_desc"), inline = TRUE))
          )
        )
      ),

      # Feature 2: Geographic Analysis
      div(
        class = "col-md-4",
        div(
          class = "card h-100 home-feature-card",
          div(
            class = "card-body text-center",
            div(
              class = "home-feature-icon",
              bs_icon("map")
            ),
            tags$h5(class = "card-title mt-3",
                    textOutput(ns("feature_geo_title"), inline = TRUE)),
            tags$p(class = "card-text text-muted",
                   style = "font-size: 0.85rem;",
                   textOutput(ns("feature_geo_desc"), inline = TRUE))
          )
        )
      ),

      # Feature 3: Open Source Package
      div(
        class = "col-md-4",
        div(
          class = "card h-100 home-feature-card",
          div(
            class = "card-body text-center",
            div(
              class = "home-feature-icon",
              bs_icon("book")
            ),
            tags$h5(class = "card-title mt-3",
                    textOutput(ns("feature_package_title"), inline = TRUE)),
            tags$p(class = "card-text text-muted",
                   style = "font-size: 0.85rem;",
                   textOutput(ns("feature_package_desc"), inline = TRUE))
          )
        )
      )
    ),

    # Links Row
    div(
      class = "d-flex justify-content-center gap-3 mb-4 flex-wrap",
      tags$a(
        href = "https://github.com/antrologos/PNADCperiods",
        target = "_blank",
        class = "btn btn-outline-primary",
        bs_icon("github"), " GitHub"
      ),
      tags$a(
        href = "https://antrologos.github.io/PNADCperiods/",
        target = "_blank",
        class = "btn btn-outline-primary",
        bs_icon("book"), " ",
        textOutput(ns("btn_documentation"), inline = TRUE)
      ),
      tags$a(
        href = "https://antrologos.github.io/PNADCperiods/articles/sidra-mensalization.html",
        target = "_blank",
        class = "btn btn-primary",
        bs_icon("info-circle"), " ",
        textOutput(ns("btn_methodology"), inline = TRUE)
      )
    ),

    # Footer
    div(
      class = "home-footer text-center text-muted",
      tags$p(
        class = "mb-1",
        style = "font-size: 0.8rem; font-style: italic;",
        textOutput(ns("footer_citation"), inline = TRUE)
      ),
      tags$p(
        class = "mb-1",
        style = "font-size: 0.8rem;",
        textOutput(ns("footer_institutions"), inline = TRUE)
      ),
      tags$p(
        class = "mb-0",
        style = "font-size: 0.75rem;",
        textOutput(ns("footer_disclaimer"), inline = TRUE)
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

homeServer <- function(id, shared_data, lang = reactive("pt")) {
  moduleServer(id, function(input, output, session) {

    get_lang <- lang

    # Hero
    output$hero_title <- renderText({ i18n("home.hero_title", get_lang()) })
    output$hero_subtitle <- renderText({ i18n("home.hero_subtitle", get_lang()) })

    # What is Mensalization
    output$what_is_header <- renderText({ i18n("home.what_is", get_lang()) })
    output$what_is_text <- renderText({ i18n("home.what_is_text", get_lang()) })

    # Feature cards
    output$feature_series_title <- renderText({ i18n("home.feature_series_title", get_lang()) })
    output$feature_series_desc <- renderText({ i18n("home.feature_series_desc", get_lang()) })
    output$feature_geo_title <- renderText({ i18n("home.feature_geo_title", get_lang()) })
    output$feature_geo_desc <- renderText({ i18n("home.feature_geo_desc", get_lang()) })
    output$feature_package_title <- renderText({ i18n("home.feature_package_title", get_lang()) })
    output$feature_package_desc <- renderText({ i18n("home.feature_package_desc", get_lang()) })

    # Buttons
    output$btn_documentation <- renderText({ i18n("home.btn_documentation", get_lang()) })
    output$btn_methodology <- renderText({ i18n("home.btn_methodology", get_lang()) })

    # Footer
    output$footer_citation <- renderText({ i18n("home.footer_citation", get_lang()) })
    output$footer_institutions <- renderText({ i18n("home.footer_institutions", get_lang()) })
    output$footer_disclaimer <- renderText({ i18n("home.footer_disclaimer", get_lang()) })
  })
}
