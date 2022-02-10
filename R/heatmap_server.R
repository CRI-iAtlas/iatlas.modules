
#' Heatmap Server
#'
#' @param id Module ID
#' @param feature_sample_data_function A shiny::reactive that returns a function
#' The function must take an argument called ".feature_class" and return a
#' dataframe with columns "sample_name", "group_name", "dataset_name",
#' "feature_name", and "feature_value"
#' @param response_sample_data_function A shiny::reactive that returns a function
#' The function must take an argument called ".feature" and return a
#' dataframe with columns "sample_name", "feature_name", "feature_value"
#' @param feature_data A shiny::reactive that returns a dataframe with columns
#' "feature_name","feature_display", "feature_class", and "feature_order". Each
#' value in the "feature_name" column should only appear once.
#' @param response_data A shiny::reactive that returns a dataframe with columns
#' "feature_name","feature_display", and optionally "feature_class. Each value
#'  in the "feature_name" column should only appear once.
#' @param group_data A shiny::reactive that returns a dataframe with columns
#' "group_name", "group_display", and optionally "group_description" and
#' "group_color". Each value in the "group_name"column should only appear once.
#' @param summarise_function_list A shiny::reactive that returns a either a function
#' or a named list of functions. If a list is passed, it will be passed to
#' shiny::selectInput. Each function must take vectors. The first one will be
#' the "feature_value" column of feature_data_function, and the second will be
#' the "feature_value" of repsonse_data_function. Each function must return one
#' numeric value.
#' @param drilldown A shiny::reactive that returns True or False
#' @param default_response A shiny::reactive that returns a string that is one of
#' the values in the response_data feature_name column
#' @param default_class A shiny::reactive that returns a string that is one of
#' the values in the feature_data feature_class column
#' @param ... shiny::reactives passed to drilldown_scatterplot_server
#'
#' @export
heatmap_server <- function(
  id,
  feature_sample_data_function,
  response_sample_data_function,
  feature_data,
  response_data,
  group_data,
  summarise_function_list = shiny::reactive(stats::cor),
  drilldown = shiny::reactive(F),
  default_response = shiny::reactive(NULL),
  default_class = shiny::reactive(NULL),
  ...
){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      # validate data ----
      validated_feature_data <- shiny::reactive({
        shiny::req(feature_data())
        validate_feature_data(feature_data())
      })

      validated_response_data <- shiny::reactive({
        shiny::req(response_data())
        validate_feature_data(
          response_data(),
          optional_columns = c("feature_display", "feature_class")
        )
      })

      default_class2 <- shiny::reactive({
        if(is.null(default_class())){
          shiny::req(validated_feature_data())
          return(validated_feature_data()$feature_class[[1]])
        } else{
          return(default_class())
        }
      })

      output$class_selection_ui <- shiny::renderUI({
        shiny::req(feature_data()$feature_class, default_class2())
        shiny::selectInput(
          inputId  = ns("class_choice"),
          label    = "Select or Search for Feature Class",
          choices  = unique(feature_data()$feature_class),
          selected = default_class2()
        )
      })

      default_response2 <- shiny::reactive({
        if(is.null(default_response())){
          shiny::req(validated_response_data())
          return(validated_response_data()$feature_name[[1]])
        } else{
          return(default_response())
        }
      })

      response_choices <- shiny::reactive({
        shiny::req(response_data())
        if("feature_class" %in% colnames(response_data())){
          lst <- response_data() %>%
            dplyr::select("feature_class", "feature_display", "feature_name") %>%
            create_nested_named_list()
        } else {
          lst <- response_data() %>%
            dplyr::select("feature_display", "feature_name") %>%
            tibble::deframe()
        }
        return(lst)
      })

      output$response_selection_ui <- shiny::renderUI({
        shiny::req(response_choices(), default_response2())
        shiny::selectInput(
          inputId  = ns("response_choice"),
          label    = "Select or Search for Response Feature",
          choices  = response_choices(),
          selected = default_response2()
        )
      })

      display_summarise_function_ui <- shiny::reactive({
        shiny::req(summarise_function_list())
        length(summarise_function_list()) > 1
      })

      output$display_summarise_function_ui <- shiny::reactive({
        display_summarise_function_ui()
      })

      shiny::outputOptions(
        output,
        "display_summarise_function_ui",
        suspendWhenHidden = FALSE
      )

      output$summarise_function_ui <- shiny::renderUI({
        shiny::req(summarise_function_list())
        shiny::selectInput(
          inputId  = ns("summarise_function_choice"),
          label    = "Select Summarise Function",
          choices  = names(summarise_function_list())
        )
      })

      feature_sample_data <- shiny::reactive({
        shiny::req(input$class_choice, feature_sample_data_function())

        data <-
          feature_sample_data_function()(.feature_class = input$class_choice)

        shiny::validate(shiny::need(
          nrow(data) > 0,
          "Feature class choice did not produce any data, please select a different one."
        ))

        validate_sample_data(data)
      })

      response_sample_data <- shiny::reactive({
        shiny::req(input$response_choice, response_sample_data_function())

        data <-
          response_sample_data_function()(.feature = input$response_choice)

        shiny::validate(shiny::need(
          nrow(data) > 0,
          "Response feature choice did not produce any data, please select a different one."
        ))

        validate_data(
          data,
          required_columns = c( "sample_name", "feature_name", "feature_value"),
          table_name = "response_data"
        )
      })

      heatmap_data <- shiny::reactive({
        shiny::req(
          feature_sample_data(),
          response_sample_data(),
          validated_feature_data(),
          validated_response_data()
        )
        create_heatmap_data(
          feature_sample_data(),
          response_sample_data(),
          validated_feature_data(),
          validated_response_data()
        )
      })

      summarise_function <- shiny::reactive({
        shiny::req(summarise_function_list())
        if(typeof(summarise_function_list()) == "closure"){
          func = summarise_function_list()
        } else if(length(summarise_function_list()) == 1){
          func <- summarise_function_list()[[1]]
        } else {
          func <- unname(
            summarise_function_list()[[input$summarise_function_choice]]
          )
        }
        return(func)
      })

      ploted_data <- heatmap_server2(
        "heatmap",
        heatmap_data,
        group_data,
        summarise_function,
        drilldown,
        ...
      )

      return(ploted_data)


    }
  )
}
