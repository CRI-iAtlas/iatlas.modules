#' Barplot Server2
#'
#' @param id Module ID
#' @param barplot_data A shiny::reactive that returns a dataframe with
#' columns "sample_name", "group_name", feature_display", and "feature_value".
#' @param group_data A shiny::reactive that returns a dataframe with columns
#' group_display", and optionally, "group_description" and "group_color". Each
#' value in the "group_name" column should only appear once.
#' @param barplot_xlab A shiny::reactive that returns a string
#' @param barplot_ylab A shiny::reactive that returns a string
#' @param barplot_title A shiny::reactive that returns a string
#' @param barplot_label A shiny::reactive that returns a string
#' @param drilldown A shiny::reactive that returns True or False
#' @param ... shiny::reactives passed to drilldown_scatterplot_server
#'
#' @export
barplot_server2 <- function(
  id,
  barplot_data,
  group_data    = shiny::reactive(NULL),
  barplot_xlab  = shiny::reactive(""),
  barplot_ylab  = shiny::reactive(""),
  barplot_title = shiny::reactive(""),
  barplot_label = shiny::reactive("Feature"),
  drilldown     = shiny::reactive(F),
  ...
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      validated_group_data <- shiny::reactive({
        if(is.null(group_data())) return(NULL)
        validate_group_data(group_data())
      })

      validated_data <- shiny::reactive({
        shiny::req(barplot_data())
        validate_data(
          barplot_data(),
          required_columns = c(
            "sample_name", "group_name", "feature_display",  "feature_value"
          ),
          table_name = "barplot data"
        )
      })

      merged_barplot_data <- shiny::reactive({
        shiny::req(validated_data())
        if(is.null(validated_group_data())){
          result <- validated_data() %>%
            dplyr::mutate(
              "group_display" = .data$group_name,
              "group_color" = NA_character_
            )
        } else {
          result <- dplyr::inner_join(
            validated_data(), validated_group_data(), by = "group_name"
          )
        }
        dplyr::select(
          result,
          "sample_name",
          "group_display",
          "group_color",
          "feature_display",
          "feature_value"
        )
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
        eventdata  = barplot_eventdata
      )

      barplot_eventdata <- shiny::reactive({
        shiny::req(barplot_source_name(), summarized_barplot_data())
        eventdata <- plotly::event_data("plotly_click", barplot_source_name())
        if(is.null(eventdata) & !is.null(input$mock_event_data)){
          eventdata <- input$mock_event_data
        }
        shiny::validate(shiny::need(eventdata, "Click on above barplot."))
        return(eventdata)
      })

      selected_group <- shiny::reactive({
        shiny::req(barplot_eventdata())
        barplot_eventdata()$key[[1]]
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

