
#' Distributions Plot Server
#'
#' @param id Module ID
#' @param sample_data_function A shiny::reactive that returns a dataframe with
#' columns "sample_name", "group_name", feature_display", and "feature_value".
#' @param group_data A shiny::reactive that returns a dataframe with columns
#' "group_name", "group_display", and optionally "group_description" and
#' "group_color". Each value in the "group_name"column should only appear once.
#' @param distplot_xlab A shiny::reactive that returns a string
#' @param distplot_title A shiny::reactive that returns a string
#' @param drilldown A shiny::reactive that returns True or False
#' @param ... shiny::reactives passed to drilldown_histogram_server
#'
#' @export
distributions_plot_server2 <- function(
  id,
  distplot_data,
  group_data     = shiny::reactive(NULL),
  plot_type      = shiny::reactive("Violin"),
  distplot_xlab  = shiny::reactive(""),
  distplot_title = shiny::reactive(NULL),
  drilldown      = shiny::reactive(F),
  ...
  ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      validated_distplot_data <- shiny::reactive({
        shiny::req(distplot_data())
        validate_data(
          distplot_data(),
          required_columns = c(
            "sample_name", "group_name", "feature_display",  "feature_value"
          ),
          table_name = "distplot_data"
        )
      })

      validated_group_data <- shiny::reactive({
        if(is.null(group_data())){
          shiny::req(validated_distplot_data())
          data <- validated_distplot_data() %>%
            dplyr::select("group_name") %>%
            dplyr::distinct() %>%
            dplyr::mutate(
              "group_display" = .data$group_name,
              "group_color" = NA_character_,
              "group_description" = ""
            )
        } else {
          data <- validate_group_data(group_data())
        }
        return(data)
      })

      merged_distplot_data <- shiny::reactive({
        shiny::req(validated_distplot_data(), validated_group_data())
        validated_distplot_data() %>%
          dplyr::inner_join(validated_group_data(), by = "group_name") %>%
          dplyr::select(
            "sample_name",
            "group_display",
            "group_color",
            "group_description",
            "feature_display",
            "feature_value"
          )
      })

      distplot_source_name <- shiny::reactive(ns("distplot"))

      plotly_function <- shiny::reactive({
        shiny::req(plot_type())
        if(plot_type() == "Violin") return(plotly_violin)
        else return(plotly_box)
      })

      plot_fill_colors <- shiny::reactive({

        colors_provided <- !all(is.na(validated_group_data()$group_color))

        if(colors_provided){
          fill_colors <- validated_group_data() %>%
            dplyr::select("group_display", "group_color") %>%
            dplyr::distinct() %>%
            tibble::deframe(.)
        } else {
          fill_colors <- NULL
        }
        return(fill_colors)
      })

      plot_title <- shiny::reactive({
        if(!is.null(distplot_title())) title <- distplot_title()
        else if(is.null(input$feature_choice)) title <- ""
        else{
          title <- validated_feature_data() %>%
            dplyr::filter(.data$feature_name == input$feature_choice) %>%
            dplyr::pull("feature_display") %>%
            unique()
        }
        return(title)
      })

      output$distplot <- plotly::renderPlotly({
        shiny::req(
          merged_distplot_data(),
          distplot_source_name(),
          plotly_function()
        )
        merged_distplot_data() %>%
          dplyr::select("group_display", "feature_value") %>%
          plotly_function()(
            source_name = distplot_source_name(),
            x_col = "group_display",
            y_col = "feature_value",
            fill_colors = plot_fill_colors(),
            title = plot_title(),
            xlab = distplot_xlab(),
            ylab = plot_title()
          )
      })

      distplot_eventdata <- shiny::reactive({
        shiny::req(
          merged_distplot_data(),
          distplot_source_name(),
          plotly_function()
        )
        if(!is.null(input$mock_event_data)){
          eventdata <- input$mock_event_data
        } else {
          eventdata <- plotly::event_data("plotly_click", distplot_source_name())
        }
        shiny::validate(shiny::need(eventdata, "Click on above barplot."))
        return(eventdata)
      })

      group_text <- plotly_server(
        "distplot",
        plot_data = merged_distplot_data,
        group_data = validated_group_data,
        eventdata = distplot_eventdata
      )

      histogram_data <- drilldown_histogram_server(
        "histogram",
        plot_data = merged_distplot_data,
        eventdata = distplot_eventdata,
        xlab = plot_title(),
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

        shiny::req(
          histogram_data(),
          merged_distplot_data(),
          !is.null(group_text())
        )
        list(
          "histogram_data" = histogram_data(),
          "distplot_data" = merged_distplot_data(),
          "group_text" = group_text()
        )
      })

      return(module_result)
    }
  )
}
