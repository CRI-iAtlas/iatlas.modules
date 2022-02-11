
#' Distributions Plot Server
#'
#' @param id Module ID
#' @param distplot_data A shiny::reactive that returns a dataframe with
#' columns "sample_name", "group_name", feature_display", "feature_value", and
#' "dataset_name". There will be one plot for each value in "dataset_name".
#' @param group_data A shiny::reactive that returns a dataframe with columns
#' "group_name", "group_display", and optionally "group_description" and
#' "group_color". Each value in the "group_name" column should only appear once.
#' @param dataset_data A shiny::reactive that returns a dataframe with columns
#' "dataset_name", and "dataset_display".
#' @param plot_type A shiny::reactive that returns a string, either "Violin" or
#' "Box"
#' @param scale_method A shiny::reactive that returns a string, one of (
#' "None", Log2", "Log2 + 1", "Log10 + 1", "Log10")
#' @param reorder_method A shiny::reactive that returns a string, one of (
#' "None", "Median", "Mean", "Max", "Min")
#' @param distplot_xlab A shiny::reactive that returns a string
#' @param drilldown A shiny::reactive that returns True or False
#' @param ... arguments sent to plotly_histogram
#'
#' @export
distributions_plot_server2 <- function(
  id,
  distplot_data,
  group_data     = shiny::reactive(NULL),
  dataset_data   = shiny::reactive(NULL),
  plot_type      = shiny::reactive("Violin"),
  scale_method   = shiny::reactive("None"),
  reorder_method = shiny::reactive("None"),
  distplot_xlab  = shiny::reactive(""),
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
          return(create_distplot_group_data(validated_distplot_data()))
        } else {
          return(validate_group_data(group_data()))
        }
      })

      validated_dataset_data <- shiny::reactive({
        if(is.null(dataset_data())){
          shiny::req(validated_distplot_data())
          return(create_distplot_dataset_data(validated_distplot_data()))
        } else {
          return(validate_dataset_data(dataset_data()))
        }
      })

      merged_distplot_data <- shiny::reactive({
        shiny::req(
          validated_distplot_data(),
          validated_group_data(),
          validated_dataset_data()
        )
        merge_distplot_data(
          validated_distplot_data(),
          validated_group_data(),
          validated_dataset_data()
        )
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

      feature <- shiny::reactive({
        shiny::req(merged_distplot_data())
        merged_distplot_data()$feature_display[[1]]
      })

      formatted_distplot_data <- shiny::reactive({
        shiny::req(
          merged_distplot_data(),
          reorder_method(),
          scale_method()
        )
        format_distplot_data2(
          merged_distplot_data(),
          reorder_method(),
          scale_method()
        )
      })

      distplots <- shiny::reactive({
        shiny::req(
          formatted_distplot_data(),
          distplot_source_name(),
          plotly_function(),
          feature()
        )
        create_displots(
          formatted_distplot_data(),
          distplot_source_name(),
          plotly_function(),
          feature(),
          plot_fill_colors(),
          distplot_xlab()
        )
      })

      output$distplot <- plotly::renderPlotly({
        shiny::req(distplots())
        plotly::subplot(
          distplots(),
          shareX = TRUE,
          shareY = TRUE,
          titleY = TRUE,
          nrows = 1,
          margin = c(0.01, 0.01, 0.01, 0.7)
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
