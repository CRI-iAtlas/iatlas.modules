
#' Drilldown Histogram Server
#'
#' @param id Module ID
#' @param plot_data A shiny::reactive that returns a dataframe with columns
#' "group", "feature_value"
#' @param eventdata A shiny::reactive that returns a dataframe with column
#' "key"
#' @param ... arguments sents to plotly_histogram
#'
#' @export
drilldown_histogram_server <- function(
  id,
  plot_data,
  eventdata,
  ...
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      selected_group <- shiny::reactive({
        shiny::req(eventdata())
        eventdata()$key[[1]]
      })

      histogram_data <- shiny::reactive({
        shiny::req(
          plot_data(),
          selected_group(),
          selected_group() %in% plot_data()$group
        )
        plot_data() %>%
          dplyr::filter(.data$group == selected_group()) %>%
          dplyr::select("feature_value")
      })

      output$histogram <- plotly::renderPlotly({
        plotly_histogram(
          histogram_data(),
          x_col = "feature_value",
          title = selected_group(),
          ...
        )
      })

      plotly_server(
        "histogram",
        plot_data = histogram_data()
      )

      return(histogram_data)

    }
  )
}
