
#' Barplot Server
#'
#' @param id Module ID
#' @param sample_data_function A shiny::reactive that returns a function.
#' The function must take an argument called ".feature_class" and return a
#' dataframe with columns "sample_name", "feature_name", "group_name", and
#' "feature_value".
#' @param feature_data A shiny::reactive that returns a dataframe with columns
#' "feature_name", and optionally "feature_display" and "feature_class".
#'  Each value in the "feature_name" column should only appear once.
#' @param group_data A shiny::reactive that returns a dataframe with columns
#' "group_name", "group_display", and optionally "group_description". Each
#'  value in the "group_name" column should only appear once.
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
  feature_data  = shiny::reactive(NULL),
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

      validated_feature_data <- shiny::reactive({
        if(is.null(feature_data())) return(NULL)
        validate_feature_data(
          feature_data(),
          optional_columns = c("feature_display", "feature_class")
        )
      })

      display_feature_class_selection_ui <- shiny::reactive({
        col_exists <- all(
          !is.null(validated_feature_data()),
          "feature_class" %in% colnames(validated_feature_data())
        )
        if(!col_exists) return(FALSE)
        else {
          return(length(unique(validated_feature_data()$feature_class)) > 1)
        }
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
          validated_feature_data()
        )
      })

      ploted_data <- barplot_server2(
        "barplot",
        barplot_data,
        group_data,
        barplot_xlab,
        barplot_ylab,
        barplot_title,
        barplot_label,
        drilldown,
        ...
      )

      return(ploted_data)
    }
  )
}
