#' Barplot UI2
#'
#' @param id Module ID
#' @param ... Arguments passed to drilldown_scatterplot_ui
#'
#' @export
barplot_ui2 <- function(id, ...){

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      plotBox(
        width = 12,
        "barplot" %>%
          ns() %>%
          plotly::plotlyOutput(.) %>%
          shinycssloaders::withSpinner(.),
        plotly_ui(ns("barplot"))
      )
    ),
    shiny::conditionalPanel(
      condition = "output.display_drilldown_ui",
      ns = ns,
      drilldown_scatterplot_ui(ns("scatterplot"), ...)
    )
  )
}

