
#' Distributions Plot Server
#'
#' @param id Module ID
#' @param sample_data_function A shiny::reactive that returns a function.
#' The function must take an argument called ".feature" and return a
#' dataframe with columns "sample_name", "feature_name", "group_name", and
#' "feature_value",
#' @param feature_data A shiny::reactive that returns a dataframe with columns
#' "feature_name", and "feature_display". Any other additional columns will be
#'  names of classes to group the features by. Each value in the "feature_name"
#'  column should only appear once.
#' @param group_data A shiny::reactive that returns a dataframe with columns
#' "group_name", "group_display", and optionally "group_description" and
#' "group_color". Each value in the "group_name"column should only appear once.
#' @param distplot_xlab A shiny::reactive that returns a string
#' @param distplot_title A shiny::reactive that returns a string
#' @param scale_method_default A shiny::reactive that returns a string
#' @param feature_default A shiny::reactive that returns a string
#' @param drilldown A shiny::reactive that returns True or False
#' @param ... shiny::reactives passed to drilldown_histogram_server
#'
#' @export
distributions_plot_server <- function(
  id,
  sample_data_function,
  feature_data = shiny::reactive(NULL),
  group_data = shiny::reactive(NULL),
  distplot_xlab = shiny::reactive(""),
  distplot_title = shiny::reactive(NULL),
  scale_method_default = shiny::reactive("None"),
  feature_default = shiny::reactive(NULL),
  drilldown = shiny::reactive(F),

  ...
  ) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      validated_feature_data <- shiny::reactive({
        if(is.null(feature_data())) return(NULL)

        needed_col_names <-
          c("feature_name", "feature_display")

        col_names <- colnames(feature_data())

        if(!all(needed_col_names %in% col_names)) {
          msg <- stringr::str_c(
            "Columns in fetaure_data (",
            stringr::str_c(col_names, collapse = ", "),
            ") missing one or more of (",
            stringr::str_c(needed_col_names, collapse = ", "),
            ")."
          )
          stop(msg)
        }

        if(nrow(feature_data()) > length(unique(feature_data()$feature_name)))
          stop("Values in feature_data$feature_name are not unique.")

        return(feature_data())
      })

      validated_group_data <- shiny::reactive({

        data <- group_data()

        if(is.null(data)) return(NULL)

        needed_col_names <-
          c("group_name", "group_display")

        col_names <- colnames(data)

        if(!all(needed_col_names %in% col_names)) {
          msg <- stringr::str_c(
            "Columns in fetaure_data (",
            stringr::str_c(col_names, collapse = ", "),
            ") missing one or more of (",
            stringr::str_c(needed_col_names, collapse = ", "),
            ")."
          )
          stop(msg)
        }

        if(nrow(data) > length(unique(data$group_name)))
          stop("Values in group_data$group_name are not unique.")

        if(!"group_color" %in% col_names){
          data <- dplyr::mutate(data, "group_color" = NA)
        }

        if(!"group_description" %in% col_names){
          data <- dplyr::mutate(data, "group_description" = "")
        }

        return(data)
      })

      feature_classes <- shiny::reactive({
        get_distributions_feature_classes(validated_feature_data())
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
        !is.null(validated_feature_data())
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
        shiny::req(validated_feature_data())

        if(is.null(input$feature_class_choice)){
          class_choice <- feature_classes()[[1]]
        } else {
          class_choice <- input$feature_class_choice
        }

        get_distributions_feature_list(validated_feature_data(), class_choice)
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

      validated_sample_data <- shiny::reactive({
        shiny::req(sample_data_function())

        if(display_feature_selection_ui()){
          shiny::req(input$feature_choice)
        }

        needed_col_names <-
          c("sample_name", "feature_name", "group_name", "feature_value")

        sample_data <- dplyr::select(
          sample_data_function()(.feature = input$feature_choice),
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

      distplot_data <- shiny::reactive({

        shiny::req(
          validated_sample_data(),
          input$scale_method_choice,
          input$reorder_method_choice
        )

        format_distplot_data(
          validated_sample_data(),
          input$scale_method_choice,
          input$reorder_method_choice,
          validated_feature_data(),
          validated_group_data()
        )
      })

      distplot_source_name <- shiny::reactive(ns("distplot"))

      plotly_function <- shiny::reactive({
        if(input$plot_type_choice == "Violin") return(plotly_violin)
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
          distplot_data(),
          distplot_source_name(),
          plotly_function()
        )
        distplot_data() %>%
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
        shiny::req(distplot_source_name(), distplot_data(), plotly_function())
        eventdata <- plotly::event_data("plotly_click", distplot_source_name())
        if(is.null(eventdata) & !is.null(input$mock_event_data)){
          eventdata <- input$mock_event_data
        }
        shiny::validate(shiny::need(eventdata, "Click on above barplot."))
        return(eventdata)
      })

      plotly_server(
        "distplot",
        plot_data = distplot_data,
        group_data = validated_group_data,
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
