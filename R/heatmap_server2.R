
#' Heatmap Server
#'
#' @param id Module ID
#' @param heatmap_data A shiny::reactive that returns a dataframe with a columns
#' named "sample_name", "group_name", "dataset_name", "feature_value",
#' "feature_display", "response_display", "response_value", and optionally
#' "feature_order".
#' @param group_data A shiny::reactive that returns a dataframe with columns
#' "group_display", and optionally "group_description" and
#' "group_order". Each value in the "group_display" column should only appear
#' once.
#' @param summarise_function A shiny::reactive that returns a function. The
#' function must take two vectors. The first one will be
#' the "feature_value" column of heatmap_data, and the second will be
#' the "response_value" of heatmap_data. The function must return one
#' numeric value.
#' @param drilldown A shiny::reactive that returns True or False
#' @param mock_event_data A shiny::reactive that returns a dataframe. For
#' testing purposes only. Must have columns "curveNumber", "pointNumber", "x",
#' "y", and "z". The "x" column corresponds to the group selected, and the
#' "y" column corresponds to feature selected.
#' @param ... arguments sent to plotly_scatter
#'
#'
#' @export
heatmap_server2 <- function(
  id,
  heatmap_data,
  group_data = shiny::reactive(NULL),
  summarise_function = shiny::reactive(stats::cor),
  drilldown = shiny::reactive(F),
  mock_event_data = shiny::reactive(NULL),
  ...
){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      validated_group_data <- shiny::reactive({
        if(is.null(group_data())){
          shiny::req(validated_heatmap_data())
          return(create_heatmap_group_data(validated_heatmap_data()))
        } else {
          return(validate_group_data(group_data()))
        }
      })

      validated_heatmap_data <- shiny::reactive({
        shiny::req(heatmap_data())
        validate_heatmap_data(heatmap_data())
      })

      merged_heatmap_data <- shiny::reactive({
        shiny::req(validated_heatmap_data(), validated_group_data())
        merge_heatmap_data(validated_heatmap_data(), validated_group_data())
      })

      validated_mock_event_data <- shiny::reactive({
        if(is.null(mock_event_data())) return(NULL)
        validate_data_columns(
          mock_event_data(),
          c("curveNumber", "pointNumber", "x", "y", "z"),
          "mock_event_data"
        )

        selected_group <- mock_event_data()$x[[1]]
        if(!selected_group %in% merged_heatmap_data()$group_display){
          msg <- stringr::str_c(
            "mock_event_data column x value: ",
            selected_group,
            " not in merged_heatmap_data column group_display"
          )
          stop(msg)
        }

        selected_feature <- mock_event_data()$y[[1]]
        if(!selected_feature %in% merged_heatmap_data()$feature_display){
          msg <- stringr::str_c(
            "mock_event_data column y value: ",
            selected_feature,
            " not in merged_heatmap_data column feature_display"
          )
          stop(msg)
        }

        return(mock_event_data())
      })

      summarized_heatmap_data <- shiny::reactive({
        shiny::req(merged_heatmap_data(), summarise_function())
        summarize_heatmap_data(merged_heatmap_data(), summarise_function())
      })

      heatmap_matrix <- shiny::reactive({
        shiny::req(summarized_heatmap_data())
        summarized_heatmap_data() %>%
          tibble::column_to_rownames("feature_display") %>%
          as.matrix()
      })

      heatmap_source_name <- shiny::reactive(ns("heatmap"))

      heatmap_plot <- shiny::reactive({
        shiny::req(heatmap_matrix(), heatmap_source_name())
        plotly_heatmap(
          heatmap_matrix(),
          source_name = heatmap_source_name(),
          scale_colors = T
        )
      })

      output$heatmap <- plotly::renderPlotly({
        shiny::req(heatmap_plot())
        heatmap_plot()
      })

      heatmap_eventdata <- shiny::reactive({
        shiny::req(heatmap_source_name(), heatmap_plot())
        if(!is.null(validated_mock_event_data())){
          eventdata <- validated_mock_event_data()
        } else {
          eventdata <- plotly::event_data("plotly_click", heatmap_source_name())
        }
        shiny::validate(shiny::need(eventdata, "Click on above heatmap."))
        return(eventdata)
      })

      group_text <- plotly_server(
        "heatmap",
        plot_data = summarized_heatmap_data,
        group_data = group_data,
        eventdata = heatmap_eventdata
      )

      selected_feature <- shiny::reactive({
        shiny::req(heatmap_eventdata())
        heatmap_eventdata()$y[[1]]
      })

      selected_group <- shiny::reactive({
        shiny::req(heatmap_eventdata())
        heatmap_eventdata()$x[[1]]
      })

      response_feature <- shiny::reactive({
        shiny::req(merged_heatmap_data())
        merged_heatmap_data() %>%
          dplyr::pull("response_display") %>%
          unique()
      })

      scatterplot_data <- shiny::reactive({
        shiny::req(
          merged_heatmap_data(),
          selected_feature(),
          selected_group(),
          response_feature()
        )
        create_scatterplot_data(
          merged_heatmap_data(),
          selected_feature(),
          response_feature(),
          selected_group()
        )
      })

      formatted_scatterplot_data <- drilldown_scatterplot_server(
        "scatterplot",
        scatterplot_data,
        selected_group = selected_group,
        ...
      )

      output$display_drilldown_ui <- shiny::reactive({
        drilldown()
      })

      shiny::outputOptions(
        output,
        "display_drilldown_ui",
        suspendWhenHidden = FALSE
      )

      module_result <- shiny::reactive({
        shiny::req(formatted_scatterplot_data(), summarized_heatmap_data())
        list(
          "scatterplot_data" = formatted_scatterplot_data(),
          "heatmap_data" = summarized_heatmap_data()
        )
      })

      return(module_result)
    }
  )
}
