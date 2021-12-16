
#' Distributions Plot Server
#'
#' @param id Module ID
#' @param plot_data_function A shiny::reactive that returns a function.
#' The function must take an argument called ".feature" and return a
#' dataframe with columns "sample_name", "feature_name", "feature_value",
#' "group_name",
#' and optionally "group_description", "group_color"
#' @param features A shiny::reactive that returns a dataframe with "feature_name",
#' "feature_display", and any other additional optional columns to group the
#' features by. If not given, the argument to plot_data_function will be Null.
#' @param distplot_xlab A shiny::reactive that returns a string
#' @param scale_method_default A shiny::reactive that returns a string
#' @param feature_default A shiny::reactive that returns a string
#' @param drilldown A shiny::reactive that returns True or False
#' @param ... shiny::reactives passed to drilldown_histogram_server
#'
#' @export
distributions_plot_server <- function(
  id,
  plot_data_function,
  features = shiny::reactive(NULL),
  distplot_xlab = shiny::reactive(""),
  scale_method_default = shiny::reactive("None"),
  feature_default = shiny::reactive(NULL),
  drilldown = shiny::reactive(F),
  ...
  ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      output$scale_method_selection_ui <- shiny::renderUI({
        shiny::req(scale_method_default())
        shiny::selectInput(
          ns("scale_method_choice"),
          "Select or Search for variable scaling",
          selected = scale_method_default(),
          choices = c(
            "None",
            "Log2",
            "Log2 + 1",
            "Log10",
            "Log10 + 1"
          )
        )
      })

      feature_classes <- shiny::reactive({
        get_distributions_feature_classes(features())
      })

      display_feature_class_selection_ui <- shiny::reactive({
        shiny::req(!is.null(feature_classes()))
        length(feature_classes()) > 1
      })

      output$display_feature_class_selection_ui <- shiny::reactive({
        display_feature_class_selection_ui()
      })

      shiny::outputOptions(
        output,
        "display_feature_class_selection_ui",
        suspendWhenHidden = FALSE
      )

      output$feature_class_selection_ui <- shiny::renderUI({
        shiny::req(feature_classes())
        shiny::selectInput(
          inputId  = ns("feature_class_choice"),
          label    = "Select Feature Class",
          choices  = feature_classes()
        )
      })

      display_feature_selection_ui <- shiny::reactive({
        !is.null(features())
      })

      output$display_feature_selection_ui <- shiny::reactive({
        display_feature_selection_ui()
      })

      shiny::outputOptions(
        output,
        "display_feature_selection_ui",
        suspendWhenHidden = FALSE
      )

      feature_list <- shiny::reactive({
        shiny::req(
          features(),
          display_feature_selection_ui(),
          !is.null(display_feature_class_selection_ui())
        )
        if(display_feature_class_selection_ui()){
          shiny::req(input$feature_class_choice)
        }
        get_distributions_feature_list(
          features(),
          input$feature_class_choice,
          display_feature_class_selection_ui()
        )
      })

      output$feature_selection_ui <- shiny::renderUI({
        shiny::req(feature_list())
        shiny::selectInput(
          inputId  = ns("feature_choice"),
          label    = "Select Feature",
          choices  = feature_list(),
          selected = feature_default()
        )
      })

      distplot_data <- shiny::reactive({
        shiny::req(
          plot_data_function(),
          input$scale_method_choice,
          input$reorder_method_choice
        )

        if(display_feature_selection_ui()){
          shiny::req(input$feature_choice)
        }

        create_distplot_data(
          plot_data_function(),
          input$feature_choice,
          input$scale_method_choice,
          input$reorder_method_choice
        )
      })

      distplot_source_name <- shiny::reactive(ns("distplot"))

      plotly_function <- shiny::reactive({
        if(input$plot_type_choice == "Violin") return(plotly_violin)
        else return(plotly_box)
      })

      plot_fill_colors <- shiny::reactive({
        shiny::req(distplot_data())
        if("group_color" %in% colnames(distplot_data())){
          fill_colors <- distplot_data() %>%
            dplyr::select("group_name", "group_color") %>%
            dplyr::distinct() %>%
            tibble::deframe(.)
        } else {
          fill_colors <- NULL
        }
        return(fill_colors)
      })

      plot_title <- shiny::reactive({
        if(display_feature_selection_ui()){
          shiny::req(features(), input$feature_choice)
          title <- features() %>%
            dplyr::filter(.data$feature_name == input$feature_choice) %>%
            dplyr::pull("feature_display") %>%
            unique()
        } else {
          title <- ""
        }
        return(title)
      })

      output$distplot <- plotly::renderPlotly({
        shiny::req(
          distplot_data(),
          distplot_source_name(),
          plotly_function()
        )
        plotly_function()(
          plot_data = distplot_data(),
          source_name = distplot_source_name(),
          x_col = "group_name",
          y_col = "feature_value",
          fill_colors = plot_fill_colors(),
          title = plot_title(),
          xlab = distplot_xlab(),
          ylab = plot_title()
        )
      })

      distplot_eventdata <- shiny::reactive({
        shiny::req(distplot_source_name(), distplot_data(), plotly_function())
        eventdata <- plotly::event_data("plotly_click", distplot_source_name())
        if(is.null(eventdata) & !is.null(input$mock_event_data)){
          eventdata <- input$mock_event_data
        }
        shiny::validate(shiny::need(eventdata, "Click on above barplot."))
        return(eventdata)
      })

      group_data <- shiny::reactive({
        shiny::req("group_description" %in% colnames(distplot_data()))
        distplot_data() %>%
          dplyr::select("group_name", "group_description") %>%
          dplyr::distinct()
      })

      plotly_server(
        "distplot",
        plot_data = distplot_data,
        group_data = group_data,
        eventdata = distplot_eventdata
      )

      histogram_data <- drilldown_histogram_server(
        "histogram",
        plot_data = distplot_data,
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

      return(list(
        "histogram_data" = histogram_data,
        "distplot_data" = distplot_data
      ))
    }
  )
}
