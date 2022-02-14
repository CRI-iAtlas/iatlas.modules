#' Barplot Server2
#'
#' @param id Module ID
#' @param barplot_data A shiny::reactive that returns a dataframe with
#' columns "sample_name", "group_name", feature_display", and "feature_value".
#' @param group_data A shiny::reactive that returns a dataframe with columns
#' group_display", and optionally, "group_description". Each value in the
#' "group_name" column should only appear once.
#' @param barplot_xlab A shiny::reactive that returns a string
#' @param barplot_ylab A shiny::reactive that returns a string
#' @param barplot_title A shiny::reactive that returns a string
#' @param barplot_label A shiny::reactive that returns a string
#' @param drilldown A shiny::reactive that returns True or False
#' @param mock_event_data A shiny::reactive that returns a dataframe. For
#' testing purposes only. Must have columns "curveNumber", "pointNumber", "x",
#' "y", and "key". The "x" column corresponds to the group selected.
#' @param ... arguments sent to plotly_scatter
#'
#' @export
barplot_server2 <- function(
  id,
  barplot_data,
  group_data      = shiny::reactive(NULL),
  barplot_xlab    = shiny::reactive(""),
  barplot_ylab    = shiny::reactive(""),
  barplot_title   = shiny::reactive(""),
  barplot_label   = shiny::reactive("Feature"),
  drilldown       = shiny::reactive(F),
  mock_event_data = shiny::reactive(NULL),
  ...
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      validated_barplot_data <- shiny::reactive({
        shiny::req(barplot_data())
        validate_data(
          barplot_data(),
          required_columns = c(
            "sample_name", "group_name", "feature_display",  "feature_value"
          ),
          table_name = "barplot data"
        )
      })

      validated_group_data <- shiny::reactive({
        if(is.null(group_data())){
          shiny::req(validated_barplot_data())
          return(create_barplot_group_data(validated_barplot_data()))
        }
        validate_group_data(group_data())
      })


      merged_barplot_data <- shiny::reactive({
        shiny::req(validated_barplot_data())
        result <- validated_barplot_data() %>%
          dplyr::inner_join(validated_group_data(), by = "group_name") %>%
          dplyr::select(
            "sample_name",
            "group_display",
            "feature_display",
            "feature_value"
          )

        if(nrow(result) == 0) {
          stop("No matches in group_name between barplot_data and group_data.")
        }

        return(result)
      })

      validated_mock_event_data <- shiny::reactive({
        if(is.null(mock_event_data())) return(NULL)
        validate_data_columns(
          mock_event_data(),
          c("curveNumber", "pointNumber", "x", "y", "key"),
          "mock_event_data"
        )

        selected_group <- mock_event_data()$x[[1]]
        if(!selected_group %in% merged_barplot_data()$group_display){
          msg <- stringr::str_c(
            "mock_event_data column x value: ",
            selected_group,
            " not in merged_barplot_data column group_display"
          )
          stop(msg)
        }
        return(mock_event_data())
      })

      summarized_barplot_data <- shiny::reactive({
        shiny::req(merged_barplot_data(), barplot_label())
        summarise_barplot_se(merged_barplot_data(), barplot_label())
      })

      barplot_source_name <- shiny::reactive(ns("barplot"))

      output$barplot <- plotly::renderPlotly({
        shiny::req(summarized_barplot_data(), barplot_source_name())
        plotly_bar(
          summarized_barplot_data(),
          source_name = barplot_source_name(),
          x_col = "group_display",
          y_col = "MEAN",
          color_col = "feature_display",
          error_col = "SE",
          text_col = "text",
          xlab = barplot_xlab(),
          ylab = barplot_ylab(),
          title = barplot_title(),
        )
      })

      group_text <- plotly_server(
        "barplot",
        plot_data  = summarized_barplot_data,
        group_data = validated_group_data,
        eventdata  = barplot_event_data
      )

      barplot_event_data <- shiny::reactive({
        shiny::req(barplot_source_name(), summarized_barplot_data())

        if(!is.null(validated_mock_event_data())){
          barplot_event_data <- validated_mock_event_data()
        } else {
          barplot_event_data <- plotly::event_data("plotly_click", barplot_source_name())
        }

        shiny::validate(shiny::need(barplot_event_data, "Click on above barplot."))
        return(barplot_event_data)
      })

      selected_group <- shiny::reactive({
        shiny::req(barplot_event_data())
        barplot_event_data()$key[[1]]
      })

      scatterplot_data <- shiny::reactive({
        shiny::req(merged_barplot_data(), selected_group())

        data <- merged_barplot_data() %>%
          dplyr::filter(.data$group_display == selected_group())

        if(nrow(data) == 0) {
          stop("selected_group not in validated scatterplot data.")
        }

        data %>%
          dplyr::select(
            "sample_name", "group_display", "feature_value", "feature_display"
          ) %>%
          tidyr::pivot_wider(
            values_from = "feature_value", names_from = "feature_display"
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
        shiny::req(formatted_scatterplot_data(), summarized_barplot_data())
        list(
          "scatterplot_data" = formatted_scatterplot_data(),
          "barplot_data" = summarized_barplot_data()
        )
      })

      return(module_result)
    }
  )
}

