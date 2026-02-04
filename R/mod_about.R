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
                "About PNADCperiods Dashboard")
      ),
      div(
        class = "card-body",
        tags$h5("What is Mensalization?"),
        tags$p(
          "Mensalization is a statistical technique that converts Brazil's ",
          "quarterly PNADC survey data into monthly estimates. The official ",
          "IBGE releases report 'rolling quarter' estimates (e.g., Jan-Feb-Mar ",
          "average reported for March), which smooth out short-term dynamics. ",
          "This dashboard presents the monthly estimates recovered using the ",
          "methodology developed by Marcos Hecksher."
        ),

        tags$h5("Methodology"),
        tags$p(
          "The mensalization algorithm uses household panel information and ",
          "birthday constraints from the PNADC questionnaire to identify which ",
          "specific month each interview refers to. The process involves:"
        ),
        tags$ol(
          tags$li("Building a crosswalk that maps observations to reference months"),
          tags$li("Calibrating survey weights to match official population totals"),
          tags$li("Computing monthly aggregates using the calibrated weights")
        ),

        tags$h5("Data Sources"),
        tags$ul(
          tags$li(tags$strong("SIDRA API:"), " Official IBGE statistics (86 series)"),
          tags$li(tags$strong("PNADC Microdata:"), " Quarterly and annual survey data")
        ),

        tags$h5("Data Freshness"),
        tags$p(
          "SIDRA data: ", textOutput(ns("sidra_freshness"), inline = TRUE)
        )
      )
    ),

    # Citation card
    div(
      class = "card mb-4",
      div(
        class = "card-header",
        tags$span(style = "font-weight: 600;", "How to Cite")
      ),
      div(
        class = "card-body",
        tags$pre(
          class = "bg-light p-3",
          style = "white-space: pre-wrap; font-size: 0.8rem; border-radius: 6px;",
"@software{PNADCperiods,
  author = {Barbosa, Rogerio and Hecksher, Marcos},
  title = {PNADCperiods: Monthly Labor Market Estimates from Brazilian PNADC},
  year = {2024},
  url = {https://github.com/antrologos/PNADCperiods}
}"
        ),
        tags$p(
          class = "text-muted mb-0",
          style = "font-size: 0.85rem;",
          "Please cite both the package and the original methodology paper."
        )
      )
    ),

    # Links card
    div(
      class = "card mb-4",
      div(
        class = "card-header",
        tags$span(style = "font-weight: 600;", "Links")
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
                "GitHub Repository"
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
                "Documentation"
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
                "IBGE SIDRA"
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
        tags$span(style = "font-weight: 600;", "Disclaimer")
      ),
      div(
        class = "card-body text-muted",
        tags$p(
          "This dashboard presents estimates derived from official IBGE survey ",
          "data. The mensalization methodology is an academic contribution and ",
          "the monthly estimates are not official IBGE statistics. Users should ",
          "consider the uncertainty inherent in survey-based estimates, especially ",
          "for sub-national or sub-group analyses where sample sizes may be small."
        ),
        tags$p(
          class = "mb-0",
          "For official statistics, please consult the IBGE website directly."
        )
      )
    )
  )
}

# ------------------------------------------------------------------------------
# Server
# ------------------------------------------------------------------------------

aboutServer <- function(id, shared_data) {
  moduleServer(id, function(input, output, session) {

    output$sidra_freshness <- renderText({
      if (!is.null(shared_data$last_updated)) {
        format(shared_data$last_updated, "%Y-%m-%d %H:%M")
      } else {
        "Not available - click 'Refresh' in Series Explorer"
      }
    })

  })
}
