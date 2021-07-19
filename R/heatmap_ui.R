
#' Heatmap UI
#'
#' @param id Module ID
#' @param ... Arguments passed to drilldown_scatterplot_ui
#'
#' @export
heatmap_ui <- function(id, ...){

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      iatlas.modules::optionsBox(
        width = 12,
        shiny::column(
          width = 6,
          shiny::uiOutput(ns("class_selection_ui"))
        ),
        shiny::column(
          width = 3,
          shiny::uiOutput(ns("response_selection_ui"))
        ),
        shiny::conditionalPanel(
          condition = "output.display_summarise_function_ui",
          ns = ns,
          shiny::column(
            width = 3,
            shiny::uiOutput(ns("summarise_function_ui"))
          )
        )
      )
    ),
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
