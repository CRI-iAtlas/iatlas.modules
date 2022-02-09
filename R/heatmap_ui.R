
#' Heatmap UI
#'
#' @param id Module ID
#' @param ... Arguments passed to drilldown_scatterplot_ui
#' @param title A string
#' @param html A string that is HTML
#'
#' @export
heatmap_ui <- function(
  id,
  title = "",
  html = htmltools::includeMarkdown(get_markdown_path("distplot1")),
  ...
){

  ns <- shiny::NS(id)

  shiny::tagList(
    titleBox(title),
    messageBox(
      width = 12,
      html
    ),
    shiny::fluidRow(
      optionsBox(
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
    heatmap_ui2(ns("heatmap"), ...)
  )
}
