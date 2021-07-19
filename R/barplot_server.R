
#' Barplot Server
#'
#' @param id Module ID
#' @param plot_data_function A shiny::reactive that returns a function
#' The function must take an argument called ".feature_class" and return a
#' dataframe with columns "sample", "feature", "feature_value", "group",
#' and optionally "group_description"
#' @param feature_classes A shiny::reactive that returns a vector of strings.
#' One of these strings are passed to plot_data_function
#' @param barplot_xlab A shiny::reactive that returns a string
#' @param barplot_ylab A shiny::reactive that returns a string
#' @param barplot_title A shiny::reactive that returns a string
#' @param barplot_label A shiny::reactive that returns a string
#' @param drilldown A shiny::reactive that returns True or False
#' @param ... shiny::reactives passed to drilldown_scatterplot_server
#'
#' @export
barplot_server <- function(
  id,
  plot_data_function,
  feature_classes = shiny::reactive(NULL),
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

      display_feature_class_selection_ui <- shiny::reactive({
        !is.null(feature_classes())
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

      barplot_data <- shiny::reactive({
        shiny::req(plot_data_function())
        if(display_feature_class_selection_ui()){
          shiny::req(input$feature_class_choice)
        }
        build_barplot_data(plot_data_function(), input$feature_class_choice)
      })

      summarized_barplot_data <- shiny::reactive({
        shiny::req(barplot_data(), barplot_label())
        summarise_barplot_se(barplot_data(), barplot_label())
      })

      barplot_source_name <- shiny::reactive(ns("barplot"))

      output$barplot <- plotly::renderPlotly({
        shiny::req(summarized_barplot_data(), barplot_source_name())
        plotly_bar(
          summarized_barplot_data(),
          source_name = barplot_source_name(),
          x_col = "group",
          y_col = "MEAN",
          color_col = "feature",
          error_col = "SE",
          text_col = "text",
          xlab = barplot_xlab(),
          ylab = barplot_ylab(),
          title = barplot_title(),
        )
      })

      group_data <- shiny::reactive({
        shiny::req("group_description" %in% colnames(barplot_data()))
        get_barplot_group_data(barplot_data())
      })

      plotly_server(
        "barplot",
        plot_data = summarized_barplot_data,
        group_data = group_data,
        eventdata = barplot_eventdata
      )

      barplot_eventdata <- shiny::reactive({
        shiny::req(barplot_source_name(), summarized_barplot_data())
        eventdata <- plotly::event_data("plotly_click", barplot_source_name())
        if(is.null(eventdata) & !is.null(input$test_event_data)){
          eventdata <- input$test_event_data
        }
        shiny::validate(shiny::need(eventdata, "Click on above barplot."))
        return(eventdata)
      })

      selected_group <- shiny::reactive({
        shiny::req(barplot_eventdata())
        barplot_eventdata()$key[[1]]
      })

      scatterplot_data <- shiny::reactive({
        shiny::req(barplot_data(), selected_group())
        barplot_data() %>%
          dplyr::filter(.data$group == selected_group()) %>%
          dplyr::select("sample", "feature", "feature_value") %>%
          tidyr::pivot_wider(
            ., values_from = "feature_value", names_from = "feature"
          )
      })

      drilldown_scatterplot_server(
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
    }
  )
}
