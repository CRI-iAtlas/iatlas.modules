
#' Distributions Plot Server
#'
#' @param id Module ID
#' @param distplot_data A shiny::reactive that returns a dataframe with
#' columns "sample_name", "group_name", feature_display", and "feature_value".
#' @param group_data A shiny::reactive that returns a dataframe with columns
#' "group_name", "group_display", and optionally "group_description" and
#' "group_color". Each value in the "group_name"column should only appear once.
#' @param plot_type A shiny::reactive that returns a string, eith "Violin" or
#' "Box"
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
        validate_distplot_data(distplot_data())
      })

      validated_group_data <- shiny::reactive({
        if(is.null(group_data())){
          shiny::req(validated_distplot_data())
          return(create_group_data(validated_distplot_data()))
        } else {
          return(validate_group_data(group_data()))
        }
      })

      merged_distplot_data <- shiny::reactive({
        shiny::req(validated_distplot_data(), validated_group_data())
        merge_distplot_data(validated_distplot_data(), validated_group_data())

      })

      distplot_source_name <- shiny::reactive(ns("distplot"))

      plotly_function <- shiny::reactive({
        shiny::req(plot_type())
        if(plot_type() == "Violin") return(plotly_violin)
        else return(plotly_box)
      })

      plot_fill_colors <- shiny::reactive({
        shiny::req(validated_group_data())
        get_plot_colors(validated_group_data())
      })

      plot_title <- shiny::reactive({
        if(!is.null(distplot_title())) return(distplot_title())
        else return("")
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
