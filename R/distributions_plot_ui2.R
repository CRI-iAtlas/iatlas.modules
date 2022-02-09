
#' Distributions Plot UI2
#'
#' @param id Module ID
#' @export
distributions_plot_ui2 <- function(id){

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      plotBox(
        width = 12,
        "distplot" %>%
          ns() %>%
          plotly::plotlyOutput(.) %>%
          shinycssloaders::withSpinner(.),
        plotly_ui(ns("distplot"))
      )
    ),
    shiny::conditionalPanel(
      condition = "output.display_drilldown_ui",
      ns = ns,
      drilldown_histogram_ui(ns("histogram"))
    )
  )
}
