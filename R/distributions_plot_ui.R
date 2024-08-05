
#' Distributions Plot UI
#'
#' @param id Module ID
#' @param title A string
#' @param html A string that is HTML
#' @export
distributions_plot_ui <- function(
  id,
  title = "",
  html = htmltools::includeMarkdown(get_markdown_path("distplot1"))
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
        shiny::conditionalPanel(
          condition = "output.display_feature_class_selection_ui",
          ns = ns,
          shiny::column(
            width = 3,
            shiny::uiOutput(ns("feature_class_selection_ui"))
          )
        ),
        shiny::conditionalPanel(
          condition = "output.display_feature_selection_ui",
          ns = ns,
          shiny::column(
            width = 3,
            shiny::uiOutput(ns("feature_selection_ui"))
          )
        ),
        shiny::column(
          width = 3,
          shiny::selectInput(
            ns("plot_type_choice"),
            "Select or Search for Plot Type",
            choices = c("Box", "Violin")
          )
        ),
        shiny::column(
          width = 3,
          shiny::uiOutput(ns("scale_method_selection_ui"))
        ),
        shiny::column(
          width = 3,
          shiny::selectInput(
            ns("reorder_method_choice"),
            "Reorder Function",
            choices = c("None", "Median", "Mean", "Max", "Min"),
            selected = "None"
          )
        )
      )
    ),
    distributions_plot_ui2(ns("distplot"))
  )
}
