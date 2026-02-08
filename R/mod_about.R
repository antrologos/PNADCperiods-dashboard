# ==============================================================================
# About Module
# ==============================================================================

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------

aboutUI <- function(id) {
  ns <- NS(id)

  # Use plain HTML divs to avoid bslib fill/flex behavior
  div(
    class = "container-fluid",
    style = "padding: 1.5rem; max-width: 1000px; margin: 0 auto;",

    # Main about card
    div(
      class = "card mb-4",
      div(
        class = "card-header bg-primary text-white",
        tags$h4(class = "mb-0", style = "font-size: 1.25rem;",
                textOutput(ns("title"), inline = TRUE))
      ),
      div(
        class = "card-body",
        tags$h5(textOutput(ns("what_is_header"), inline = TRUE)),
        tags$p(textOutput(ns("what_is_text"), inline = TRUE)),

        tags$h5(textOutput(ns("methodology_header"), inline = TRUE)),
        tags$p(textOutput(ns("methodology_text"), inline = TRUE)),
        tags$ol(
          tags$li(textOutput(ns("methodology_step1"), inline = TRUE)),
          tags$li(textOutput(ns("methodology_step2"), inline = TRUE)),
          tags$li(textOutput(ns("methodology_step3"), inline = TRUE))
        ),

        tags$h5(textOutput(ns("data_sources_header"), inline = TRUE)),
        tags$ul(
          tags$li(
            tags$strong("SIDRA API:"), " ",
            textOutput(ns("data_source_sidra"), inline = TRUE)
          ),
          tags$li(
            tags$strong("PNADC Microdata:"), " ",
            textOutput(ns("data_source_microdata"), inline = TRUE)
          )
        ),

        tags$h5(textOutput(ns("data_freshness_header"), inline = TRUE)),
        tags$p(
          textOutput(ns("sidra_data_label"), inline = TRUE), " ",
          textOutput(ns("sidra_freshness"), inline = TRUE)
        )
      )
    ),

    # Citation card
    div(
      class = "card mb-4",
      div(
        class = "card-header",
        tags$span(style = "font-weight: 600;",
                  textOutput(ns("how_to_cite_header"), inline = TRUE))
      ),
      div(
        class = "card-body",
        tags$pre(
          class = "bg-light p-3",
          style = "white-space: pre-wrap; font-size: 0.8rem; border-radius: 6px;",
"@software{PNADCperiods,
  author = {Barbosa, Rogerio J. and Hecksher, Marcos},
  title = {PNADCperiods: Identify Reference Periods in Brazil's PNADC Survey Data},
  year = {2026},
  note = {R package version 0.1.0. R. Barbosa (Ceres-IESP/UERJ): package, dashboard, and website. M. Hecksher (Ipea): mensalization methodology.},
  url = {https://github.com/antrologos/PNADCperiods}
}"
        ),
        tags$h6(class = "mt-3 mb-2", style = "font-weight: 600;",
                textOutput(ns("methodology_refs_header"), inline = TRUE)),
        tags$ul(
          style = "font-size: 0.8rem; padding-left: 1.2rem;",
          tags$li(
            'HECKSHER, Marcos. "Valor Impreciso por M\u00eas Exato: Microdados e Indicadores Mensais Baseados na Pnad Cont\u00ednua". IPEA - Nota T\u00e9cnica Disoc, n. 62. Bras\u00edlia, DF: IPEA, 2020. ',
            tags$a(href = "https://portalantigo.ipea.gov.br/portal/index.php?option=com_content&view=article&id=35453", target = "_blank", "[Link]")
          ),
          tags$li('HECKSHER, M. "Cinco meses de perdas de empregos e simula\u00e7\u00e3o de um incentivo a contrata\u00e7\u00f5es". IPEA - Nota T\u00e9cnica Disoc, n. 87. Bras\u00edlia, DF: IPEA, 2020.'),
          tags$li('HECKSHER, Marcos. "Mercado de trabalho: A queda da segunda quinzena de mar\u00e7o, aprofundada em abril". IPEA - Carta de Conjuntura, v. 47, p. 1-6, 2020.')
        ),
        tags$p(
          class = "text-muted mb-0",
          style = "font-size: 0.85rem;",
          textOutput(ns("cite_note"), inline = TRUE)
        )
      )
    ),

    # Links card
    div(
      class = "card mb-4",
      div(
        class = "card-header",
        tags$span(style = "font-weight: 600;",
                  textOutput(ns("links_header"), inline = TRUE))
      ),
      div(
        class = "card-body",
        div(
          class = "row g-3",
          # GitHub link
          div(
            class = "col-md-4",
            div(
              class = "text-center p-3 bg-light rounded",
              icon("github", class = "fa-2x mb-2 text-muted"),
              tags$br(),
              tags$a(
                href = "https://github.com/antrologos/PNADCperiods",
                target = "_blank",
                textOutput(ns("github_link"), inline = TRUE)
              )
            )
          ),
          # Documentation link
          div(
            class = "col-md-4",
            div(
              class = "text-center p-3 bg-light rounded",
              icon("book", class = "fa-2x mb-2 text-muted"),
              tags$br(),
              tags$a(
                href = "https://antrologos.github.io/PNADCperiods/",
                target = "_blank",
                textOutput(ns("documentation_link"), inline = TRUE)
              )
            )
          ),
          # SIDRA link
          div(
            class = "col-md-4",
            div(
              class = "text-center p-3 bg-light rounded",
              icon("building", class = "fa-2x mb-2 text-muted"),
              tags$br(),
              tags$a(
                href = "https://sidra.ibge.gov.br/",
                target = "_blank",
                textOutput(ns("ibge_sidra_link"), inline = TRUE)
              )
            )
          )
        )
      )
    ),

    # Disclaimer card
    div(
      class = "card mb-4",
      div(
        class = "card-header",
        tags$span(style = "font-weight: 600;",
                  textOutput(ns("disclaimer_header"), inline = TRUE))
      ),
      div(
        class = "card-body text-muted",
        tags$p(textOutput(ns("disclaimer_text"), inline = TRUE)),
        tags$p(
          class = "mb-0",
          textOutput(ns("disclaimer_official"), inline = TRUE)
        )
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

aboutServer <- function(id, shared_data, lang = reactive("pt")) {
  moduleServer(id, function(input, output, session) {

    # Helper to get current language
    get_lang <- lang

    # --------------------------------------------------------------------------
    # Text outputs for i18n
    # --------------------------------------------------------------------------

    output$title <- renderText({ i18n("about.title", get_lang()) })
    output$what_is_header <- renderText({ i18n("about.what_is", get_lang()) })
    output$what_is_text <- renderText({ i18n("about.what_is_text", get_lang()) })
    output$methodology_header <- renderText({ i18n("about.methodology", get_lang()) })
    output$methodology_text <- renderText({ i18n("about.methodology_text", get_lang()) })
    output$methodology_step1 <- renderText({ i18n("about.methodology_step1", get_lang()) })
    output$methodology_step2 <- renderText({ i18n("about.methodology_step2", get_lang()) })
    output$methodology_step3 <- renderText({ i18n("about.methodology_step3", get_lang()) })
    output$data_sources_header <- renderText({ i18n("about.data_sources", get_lang()) })
    output$data_source_sidra <- renderText({ i18n("about.data_source_sidra", get_lang()) })
    output$data_source_microdata <- renderText({ i18n("about.data_source_microdata", get_lang()) })
    output$data_freshness_header <- renderText({ i18n("about.data_freshness", get_lang()) })
    output$sidra_data_label <- renderText({ i18n("about.sidra_data", get_lang()) })
    output$how_to_cite_header <- renderText({ i18n("about.how_to_cite", get_lang()) })
    output$cite_note <- renderText({ i18n("about.cite_note", get_lang()) })
    output$links_header <- renderText({ i18n("about.links", get_lang()) })
    output$github_link <- renderText({ i18n("about.github", get_lang()) })
    output$documentation_link <- renderText({ i18n("about.documentation", get_lang()) })
    output$ibge_sidra_link <- renderText({ i18n("about.ibge_sidra", get_lang()) })
    output$disclaimer_header <- renderText({ i18n("about.disclaimer", get_lang()) })
    output$methodology_refs_header <- renderText({ i18n("about.methodology_refs_header", get_lang()) })
    output$disclaimer_text <- renderText({ i18n("about.disclaimer_text", get_lang()) })
    output$disclaimer_official <- renderText({ i18n("about.disclaimer_official", get_lang()) })

    # --------------------------------------------------------------------------
    # Data freshness output
    # --------------------------------------------------------------------------

    output$sidra_freshness <- renderText({
      if (!is.null(shared_data$last_updated)) {
        format(shared_data$last_updated, "%Y-%m-%d %H:%M")
      } else {
        i18n("about.not_available_refresh", get_lang())
      }
    })

  })
}
