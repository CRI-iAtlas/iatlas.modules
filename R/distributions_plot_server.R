
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
#' @param dataset_data A shiny::reactive that returns a dataframe with columns
#' "dataset_name", and "dataset_display".
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
  dataset_data   = shiny::reactive(NULL),
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

        optional_columns <- feature_data() %>%
          colnames() %>%
          setdiff("feature_name") %>%
          c("feature_display") %>%
          unique()

        validate_feature_data(
          feature_data(),
          optional_columns = optional_columns
        )
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
        shiny::req(feature_classes(), display_feature_class_selection_ui())
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
        shiny::req(feature_list(), display_feature_selection_ui())
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

        sample_data <-
          sample_data_function()(.feature = input$feature_choice) %>%
          validate_sample_data()
      })

      distplot_data <- shiny::reactive({
        shiny::req(validated_sample_data())
        format_distplot_data(validated_sample_data(), validated_feature_data())
      })

      plot_title <- shiny::reactive({
        if(!is.null(distplot_title())) return(distplot_title())
        else if(is.null(input$feature_choice)) return("")
        else {
          shiny::req(validated_feature_data())
          title <- validated_feature_data() %>%
            dplyr::filter(.data$feature_name == input$feature_choice) %>%
            dplyr::pull("feature_display") %>%
            unique()
        }
      })

      ploted_data <- distributions_plot_server2(
        "distplot",
        distplot_data,
        group_data,
        dataset_data,
        distplot_xlab,
        drilldown      = drilldown,
        plot_type      = shiny::reactive(input$plot_type_choice),
        scale_method   = shiny::reactive(input$scale_method_choice),
        reorder_method = shiny::reactive(input$reorder_method_choice),
        ...
      )

      return(ploted_data)
    }
  )
}
