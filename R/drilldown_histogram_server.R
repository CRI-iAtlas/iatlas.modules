
#' Drilldown Histogram Server
#'
#' @param id Module ID
#' @param plot_data A shiny::reactive that returns a dataframe with columns
#' "group_display", "dataset_display", "feature_value"
#' @param eventdata A shiny::reactive that returns a dataframe with column
#' "key"
#' @param ... arguments sents to plotly_histogram
#'
#' @export
drilldown_histogram_server <- function(
  id,
  plot_data,
  eventdata,
  eventdata_group_column   = shiny::reactive("x"),
  eventdata_dataset_column = shiny::reactive("key"),
  ...
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      selected_group <- shiny::reactive({
        shiny::req(eventdata(), eventdata_group_column())
        eventdata()[[eventdata_group_column()]][[1]]
      })

      selected_dataset <- shiny::reactive({
        shiny::req(eventdata(), eventdata_dataset_column())
        eventdata()[[eventdata_dataset_column()]][[1]]
      })

      histogram_data <- shiny::reactive({
        shiny::req(
          plot_data(),
          selected_group(),
          selected_group() %in% plot_data()$group_display,
          selected_dataset(),
          selected_dataset() %in% plot_data()$dataset_display
        )
        plot_data() %>%
          dplyr::filter(
            .data$group_display == selected_group(),
            .data$dataset_display == selected_dataset()
          ) %>%
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
