
#' Heatmap UI2
#'
#' @param id Module ID
#' @param ... Arguments passed to drilldown_scatterplot_ui
#'
#' @export
heatmap_ui2 <- function(id, ...){

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      plotBox(
        width = 12,
        "heatmap" %>%
          ns() %>%
          plotly::plotlyOutput(.) %>%
          shinycssloaders::withSpinner(.),
        plotly_ui(ns("heatmap"))
      )
    ),
    shiny::conditionalPanel(
      condition = "output.display_drilldown_ui",
      ns = ns,
      drilldown_scatterplot_ui(ns("scatterplot"), ...)
    )
  )
}
