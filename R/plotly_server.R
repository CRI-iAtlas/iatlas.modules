#' Plotly Server
#'
#' @param id Module ID
#' @param plot_data A shiny::reactive that returns a dataframe
#' @param group_data A shiny::reactive that returns NULL or a dataframe. It
#' must have the columns "group_name", "group_display" and "group_description"
#' @param eventdata A shiny::reactive that returns NULL or a dataframe. It
#' must have the column "x"
#'
#' @export
plotly_server <- function(
  id,
  plot_data,
  group_data  = shiny::reactive(NULL),
  eventdata   = shiny::reactive(NULL)
){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      show_group_text <- shiny::reactive(!is.null(group_data()))
      output$show_group_text <- show_group_text
      shiny::outputOptions(output, "show_group_text", suspendWhenHidden = FALSE)

      output$plot_group_text <- shiny::renderText({
        shiny::req(show_group_text())
        shiny::validate(shiny::need(
          eventdata(),
          "Click plot to see group information."
        ))
        create_group_text_from_eventdata(eventdata(), group_data())
      })

      output$download_tbl <- shiny::downloadHandler(
        filename = function() stringr::str_c("data-", Sys.Date(), ".csv"),
        content = function(con) readr::write_csv(plot_data(), con)
      )
    }
  )
}
