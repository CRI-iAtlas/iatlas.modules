
#' Heatmap Server
#'
#' @param id Module ID
#' @param feature_classes A shiny::reactive that returns a list of feature
#' classes. This will be passed to the choices argument of shiny::selectInput.
#' The selected class will be passed to the feature_data_function as .class.
#' @param response_features A shiny::reactive that returns a list of features.
#' This will be passed to the choices argument of shiny::selectInput.
#' The selected feature will be passed to the repsonse_data_function as .feature.
#' @param feature_data_function A shiny::reactive that returns a function
#' The function must take an argument called ".feature_class" and return a
#' dataframe with columns "sample", "group", "feature", "feature_value",
#' "feature_order", "group_description", "color"
#' @param response_data_function A shiny::reactive that returns a function
#' The function must take an argument called ".feature" and return a
#' dataframe with columns "sample", "feature_value"
#' @param summarise_function_list A shiny::reactive that returns a either a function
#' or a named list of functions. If a list is passed, it will be passed to
#' shiny::selectInput. Each function must take vectors. The first one will be
#' the "feature_value" column of feature_data_function, and the second will be
#' the "feature_value" of repsonse_data_function. Each function must return one
#' numeric value.
#' @param drilldown A shiny::reactive that returns True or False
#' @param ... shiny::reactives passed to drilldown_scatterplot_server
#'
#' @export
heatmap_server <- function(
  id,
  feature_classes,
  response_features,
  feature_data_function,
  response_data_function,
  summarise_function_list = shiny::reactive(stats::cor),
  drilldown = shiny::reactive(F),
  ...
){
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      output$class_selection_ui <- shiny::renderUI({
        shiny::req(feature_classes())
        shiny::selectInput(
          inputId  = ns("feature_class_choice"),
          label    = "Select or Search for Feature Class",
          choices  = feature_classes()
        )
      })

      output$response_selection_ui <- shiny::renderUI({
        shiny::req(response_features())
        shiny::selectInput(
          inputId  = ns("response_feature_choice"),
          label    = "Select or Search for Response Feature",
          choices  = response_features()
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

      feature_values_tbl <- shiny::reactive({
        shiny::req(input$feature_class_choice, feature_data_function())
        tbl <- feature_data_function()(.class = input$feature_class_choice) %>%
          dplyr::select(
            "sample",
            "feature",
            "feature_value",
            "feature_order",
            "group",
            "group_description",
            "color"
          )
        shiny::validate(shiny::need(
          nrow(tbl) > 0,
          "Feature class choice did not produce any data, please select a different one."
        ))
        return(tbl)
      })

      response_values_tbl <- shiny::reactive({
        shiny::req(input$response_feature_choice, response_data_function())
        tbl <- response_data_function()(.feature = input$response_feature_choice) %>%
          dplyr::select("sample", "response" = "feature", "response_value" = "feature_value")
        shiny::validate(shiny::need(
          nrow(tbl) > 0,
          "Response feature choice did not produce any data, please select a different one."
        ))
        return(tbl)
      })

      joined_tibble <- shiny::reactive({
        shiny::req(feature_values_tbl(), response_values_tbl())
        dplyr::inner_join(feature_values_tbl(), response_values_tbl(), by = "sample")
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
        shiny::req(joined_tibble(), summarise_function())
        build_heatmap_tbl(joined_tibble(), summarise_function())
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
        if(is.null(eventdata) & !is.null(input$test_event_data)){
          eventdata <- input$test_event_data
        }
        shiny::validate(shiny::need(eventdata, "Click on above heatmap."))
        return(eventdata)
      })

      group_data <- shiny::reactive({
        shiny::req("group_description" %in% colnames(feature_values_tbl()))
        feature_values_tbl() %>%
          dplyr::select("group", "description" = "group_description") %>%
          dplyr::distinct()
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
        shiny::req(response_values_tbl())
        response_values_tbl()$response[[1]]
      })

      scatterplot_data <- shiny::reactive({
        shiny::req(
          joined_tibble(), selected_feature(), selected_group(), response_feature()
        )

        shiny::validate(shiny::need(
          all(
            selected_group() %in% joined_tibble()$group,
            selected_feature() %in% joined_tibble()$feature
          ),
          "Plot has been updated, please click on plot."
        ))

        shiny::validate(shiny::need(
          selected_feature() != response_feature(),
          "Selected features to compare are the same, please select new features."
        ))

        joined_tibble() %>%
          dplyr::filter(
            .data$feature == selected_feature(),
            .data$group == selected_group()
          ) %>%
          dplyr::select("sample", "group", "feature_value", "response_value") %>%
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
