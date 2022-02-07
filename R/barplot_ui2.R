
#' Barplot2 UI
#'
#' @param id Module ID
#' @param title A string
#' @param barplot_html A string that is HTML
#' @param ... Arguments passed to barplot_ui
#'
#' @export
barplot_ui2 <- function(
  id,
  title = "",
  barplot_html = htmltools::includeMarkdown(get_markdown_path("barchart1")),
  ...
  ){

  ns <- shiny::NS(id)

  shiny::tagList(
    titleBox(title),
    messageBox(
      width = 12,
      barplot_html
    ),
    shiny::conditionalPanel(
      condition = "output.display_feature_class_selection_ui",
      ns = ns,
      shiny::fluidRow(
        optionsBox(
          width = 12,
          shiny::column(
            width = 12,
            shiny::uiOutput(ns("feature_class_selection_ui"))
          )
        )
      )
    ),
    barplot_ui(ns("barplot"), ...)
  )
}
