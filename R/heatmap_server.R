
#' Heatmap Server
#'
#' @param id Module ID
#' @param feature_sample_data_function A shiny::reactive that returns a function
#' The function must take an argument called ".feature_class" and return a
#' dataframe with columns "sample_name", "group_name", "feature_name", and
#' "feature_value"
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
      valid_feature_data <- shiny::reactive({
        shiny::req(feature_data())
        column_names <- c(
          "feature_name", "feature_display", "feature_class", "feature_order"
        )
        if(!all(column_names %in% colnames(feature_data()))){
          stop("feature_data missing required columns.")
        }
        dplyr::select(feature_data(), dplyr::all_of(column_names))
      })

      valid_response_data <- shiny::reactive({
        shiny::req(response_data())
        column_names <- c(
          "feature_name", "feature_display", "feature_class"
        )
        if(!all(column_names %in% colnames(response_data()))){
          stop("response_data missing required columns.")
        }
        dplyr::select(response_data(), dplyr::all_of(column_names))
      })

      valid_group_data <- shiny::reactive({
        shiny::req(group_data())
        column_names <- c(
          "group_name", "group_display", "group_color", "group_description"
        )
        if(!all(column_names %in% colnames(group_data()))){
          stop("group_data missing required columns.")
        }
        dplyr::select(group_data(), dplyr::all_of(column_names))
      })

      default_class2 <- shiny::reactive({
        if(is.null(default_class())){
          shiny::req(valid_feature_data())
          return(valid_feature_data()$feature_class[[1]])
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
          shiny::req(valid_response_data())
          return(valid_response_data()$feature_name[[1]])
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

      feature_sample_tbl <- shiny::reactive({
        shiny::req(input$class_choice, feature_sample_data_function())
        tbl <-
          feature_sample_data_function()(
            .feature_class = input$class_choice
          ) %>%
          dplyr::select(
            "sample_name",
            "feature_name",
            "feature_value",
            "group_name"
          )
        shiny::validate(shiny::need(
          nrow(tbl) > 0,
          "Feature class choice did not produce any data, please select a different one."
        ))
        return(tbl)
      })

      response_sample_tbl <- shiny::reactive({
        shiny::req(input$response_choice, response_sample_data_function())

        tbl <-
          response_sample_data_function()(.feature = input$response_choice) %>%
          dplyr::select(
            "sample_name",
            "feature_name",
            "feature_value"
          )

        shiny::validate(shiny::need(
          nrow(tbl) > 0,
          "Response feature choice did not produce any data, please select a different one."
        ))

        return(tbl)
      })

      joined_feature_tbl <- shiny::reactive({
        shiny::req(
          valid_feature_data(),
          valid_response_data(),
          valid_group_data()
        )

        feature_sample_tbl() %>%
          dplyr::inner_join(valid_feature_data(), by = "feature_name") %>%
          dplyr::inner_join(valid_group_data(), by = "group_name") %>%
          dplyr::select(
            "sample_name",
            "feature_value",
            "feature_display",
            "feature_order",
            "group_display",
            "group_color",
            "group_description"
          )
      })

      joined_response_tbl <- shiny::reactive({
        shiny::req(feature_sample_tbl(), response_sample_tbl())

        response_sample_tbl() %>%
          dplyr::inner_join(valid_response_data(), by = "feature_name") %>%
          dplyr::select(
            "sample_name",
            "response_value" = "feature_value",
            "response_display" = "feature_display"
          )
      })

      joined_tbl <- shiny::reactive({
        shiny::req(joined_feature_tbl(), joined_response_tbl())
        dplyr::inner_join(
          joined_feature_tbl(),
          joined_response_tbl(),
          by = "sample_name"
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

      heatmap_tibble <- shiny::reactive({
        shiny::req(joined_tbl(), summarise_function())
        build_heatmap_tbl(joined_tbl(), summarise_function())
      })

      heatmap_matrix <- shiny::reactive({
        shiny::req(heatmap_tibble())
        heatmap_tibble() %>%
          tibble::column_to_rownames("feature") %>%
          as.matrix()
      })

      heatmap_source_name <- shiny::reactive(ns("heatmap"))

      heatmap <- shiny::reactive({
        shiny::req(heatmap_matrix(), heatmap_source_name())
        plotly_heatmap(
          heatmap_matrix(),
          source_name = heatmap_source_name(),
          scale_colors = T
        )
      })

      output$heatmap <- plotly::renderPlotly({
        shiny::req(heatmap())
        heatmap()
      })

      heatmap_eventdata <- shiny::reactive({
        shiny::req(heatmap_source_name(), heatmap())
        eventdata <- plotly::event_data("plotly_click", heatmap_source_name())
        if(is.null(eventdata) & !is.null(input$mock_event_data)){
          eventdata <- input$mock_event_data
        }
        shiny::validate(shiny::need(eventdata, "Click on above heatmap."))
        return(eventdata)
      })

      plotly_server(
        "heatmap",
        plot_data = heatmap_tibble,
        group_data = group_data,
        eventdata = heatmap_eventdata
      )

      selected_feature <- shiny::reactive({
        shiny::req(heatmap_eventdata())
        heatmap_eventdata()$y[[1]]
      })

      selected_group <- shiny::reactive({
        shiny::req(heatmap_eventdata())
        heatmap_eventdata()$x[[1]]
      })

      response_feature <- shiny::reactive({
        shiny::req(valid_response_data(), input$response_choice)
        valid_response_data() %>%
          dplyr::filter(.data$feature_name == input$response_choice) %>%
          dplyr::pull("feature_display")
      })

      scatterplot_data <- shiny::reactive({
        shiny::req(
          joined_tbl(), selected_feature(), selected_group(), response_feature()
        )

        shiny::validate(shiny::need(
          all(
            selected_group() %in% joined_tbl()$group_display,
            selected_feature() %in% joined_tbl()$feature_display
          ),
          "Plot has been updated, please click on plot."
        ))

        shiny::validate(shiny::need(
          selected_feature() != response_feature(),
          "Selected features to compare are the same, please select new features."
        ))

        joined_tbl() %>%
          dplyr::filter(
            .data$feature_display == selected_feature(),
            .data$group_display == selected_group()
          ) %>%
          dplyr::select("sample_name", "group_display", "feature_value", "response_value") %>%
          dplyr::rename(
            !!selected_feature() := .data$feature_value,
            !!response_feature() := .data$response_value
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
        "heatmap_data" = heatmap_tibble
      ))
    }
  )
}
