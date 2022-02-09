
#' Heatmap Server
#'
#' @param id Module ID
#' @param heatmap_data A shiny::reactive that returns a dataframe with a columns
#' named "sample_name", "group_name", "feature_value", "feature_display",
#' "response_display", "response_value", and optionally "feature_order".
#' @param group_data A shiny::reactive that returns a dataframe with columns
#' "group_display", and optionally "group_description" and
#' "group_order". Each value in the "group_display" column should only appear
#' once.
#' @param summarize_function A shiny::reactive that returns a function. The function must take vectors.
#' The first one will be the "feature_value" column of heatmap_data , and the second will be
#' the "response_value" of heatmap_data, by group. The function must return one
#' numeric value.
#' @param drilldown A shiny::reactive that returns True or False
#' @param ... shiny::reactives passed to drilldown_scatterplot_server
#'
#' @export
heatmap_server2 <- function(
  id,
  heatmap_data,
  group_data = shiny::reactive(NULL),
  summarise_function = shiny::reactive(stats::cor),
  drilldown = shiny::reactive(F),
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

        validate_feature_data(
          heatmap_data(),
          required_columns = c(
            "sample_name",
            "group_name",
            "feature_value",
            "feature_display",
            "response_display",
            "response_value"
          ),
          table_name = "heatmap_data",
          table_key = NULL,
          optional_columns = c("feature_order")
        )
      })

      joined_heatmap_data <- shiny::reactive({
        shiny::req(validated_heatmap_data(), validated_group_data())
        validated_heatmap_data() %>%
          dplyr::inner_join(validated_group_data(), by = "group_name") %>%
          dplyr::select(
            "sample_name",
            "feature_display",
            "feature_order",
            "group_display",
            "feature_value",
            "response_display",
            "response_value"
          )
      })

      summarized_heatmap_data <- shiny::reactive({
        shiny::req(joined_heatmap_data(), summarise_function())
        summarize_heatmap_data(joined_heatmap_data(), summarise_function())
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
        eventdata <- plotly::event_data("plotly_click", heatmap_source_name())
        if(is.null(eventdata) & !is.null(input$mock_event_data)){
          eventdata <- input$mock_event_data
        }
        shiny::validate(shiny::need(eventdata, "Click on above heatmap."))
        return(eventdata)
      })

      plotly_server(
        "heatmap",
        plot_data = heatmap_tibble,
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
        shiny::req(joined_heatmap_data())
        joined_heatmap_data() %>%
          dplyr::pull("response_display") %>%
          unique()
      })

      scatterplot_data <- shiny::reactive({
        shiny::req(
          joined_heatmap_data(),
          selected_feature(),
          selected_group(),
          response_feature()
        )

        shiny::validate(shiny::need(
          all(
            selected_group() %in% joined_heatmap_data()$group_display,
            selected_feature() %in% joined_heatmap_data()$feature_display
          ),
          "Plot has been updated, please click on plot."
        ))

        shiny::validate(shiny::need(
          selected_feature() != response_feature(),
          "Selected features to compare are the same, please select new features."
        ))

        joined_heatmap_data() %>%
          dplyr::filter(
            .data$feature_display == selected_feature(),
            .data$group_display == selected_group()
          ) %>%
          dplyr::select("sample_name", "group_display", "feature_value", "response_value") %>%
          dplyr::rename(
            !!selected_feature() := .data$feature_value,
            !!response_feature() := .data$response_value
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

      return(list(
        "scatterplot_data" = formatted_scatterplot_data,
        "heatmap_data" = summarized_heatmap_data
      ))
    }
  )
}
