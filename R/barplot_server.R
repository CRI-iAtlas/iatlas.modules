
#' Barplot Server
#'
#' @param id Module ID
#' @param sample_data_function A shiny::reactive that returns a function.
#' The function must take an argument called ".feature" and return a
#' dataframe with columns "sample_name", "feature_name", "group_name", and
#' "feature_value",
#' @param feature_data A shiny::reactive that returns a dataframe with columns
#' "feature_name","feature_display", and optionally "feature_class. Each value
#'  in the "feature_name" column should only appear once.
#' @param group_data A shiny::reactive that returns a dataframe with columns
#' "group_name", "group_display", and optionally "group_description" and
#' "group_color". Each value in the "group_name" column should only appear once.
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
  sample_data_function,
  feature_data = shiny::reactive(NULL),
  group_data = shiny::reactive(NULL),
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


      validated_feature_data <- shiny::reactive({
        if(is.null(feature_data())) return(NULL)
        validate_feature_data(feature_data())

        dplyr::select(
          feature_data(),
          dplyr::any_of(c( "feature_name", "feature_display", "feature_class"))
        )
      })

      validated_group_data <- shiny::reactive({
        if(is.null(group_data())) return(NULL)
        validate_group_data(group_data())
        return(group_data())
      })

      display_feature_class_selection_ui <- shiny::reactive({
        all(
          !is.null(validated_feature_data()),
          !is.null(validated_feature_data()$feature_class),
          length(unique(validated_feature_data()$feature_class)) > 1
        )
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
        shiny::req(display_feature_class_selection_ui())
        shiny::selectInput(
          inputId  = ns("feature_class_choice"),
          label    = "Select Feature Class",
          choices  = sort(unique(validated_feature_data()$feature_class))
        )
      })

      validated_sample_data <- shiny::reactive({
        shiny::req(sample_data_function())

        if(display_feature_class_selection_ui()){
          shiny::req(input$feature_class_choice)
        }

        needed_col_names <-
          c("sample_name", "feature_name", "group_name", "feature_value")

        sample_data <- dplyr::select(
          sample_data_function()(.feature_class = input$feature_class_choice),
          dplyr::any_of(needed_col_names)
        )

        col_names <- colnames(sample_data)


        if(!all(needed_col_names %in% col_names)) {
          msg <- stringr::str_c(
            "Columns in table from sample_data_function (",
            stringr::str_c(col_names, collapse = ", "),
            ") missing one or more of (",
            stringr::str_c(needed_col_names, collapse = ", "),
            ")."
          )
          stop(msg)
        }

        return(sample_data)
      })

      barplot_data <- shiny::reactive({
        shiny::req(validated_sample_data())
        format_barplot_data(
          validated_sample_data(),
          validated_feature_data(),
          validated_group_data()
        )
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

      plotly_server(
        "barplot",
        plot_data = summarized_barplot_data,
        group_data = group_data,
        eventdata = barplot_eventdata
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
        shiny::req(barplot_data(), selected_group())
        barplot_data() %>%
          dplyr::filter(.data$group_name == selected_group()) %>%
          dplyr::select("sample_name", "group_display", "feature_display", "feature_value") %>%
          tidyr::pivot_wider(
            ., values_from = "feature_value", names_from = "feature_display"
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
        "barplot_data" = summarized_barplot_data
      ))
    }
  )
}
