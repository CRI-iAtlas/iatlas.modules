
#' Drilldown Scatterplot Server
#'
#' @param id Module ID
#' @param scatterplot_data A shiny::reactive that returns a dataframe with columns
#' "sample", "group", "feature", "feature_value"
#' @param x_feature_input A shiny::reactive that returns a string
#' @param y_feature_input A shiny::reactive that returns a string
#' @param selected_group A string, this gets added to the sample label
#'
#' @export
drilldown_scatterplot_server <- function(
  id,
  scatterplot_data,
  x_feature_input = NULL,
  y_feature_input = NULL,
  selected_group = shiny::reactive("Clicked Group")
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns

      scatterplot_feature_columns <- shiny::reactive({
        scatterplot_data() %>%
          colnames() %>%
          setdiff(c("sample", "group"))
      })

      display_feature_selection_ui <- shiny::reactive({
        shiny::req(scatterplot_feature_columns())

        all(
          any(is.null(x_feature_input), is.null(y_feature_input)),
          length(scatterplot_feature_columns()) > 2
        )
      })

      output$display_feature_selection_ui <- shiny::reactive({
        display_feature_selection_ui()
      })

      shiny::outputOptions(
        output,
        "display_feature_selection_ui",
        suspendWhenHidden = FALSE
      )

      output$x_feature_selection_ui <- shiny::renderUI({
        shiny::req(display_feature_selection_ui())
        choices <- scatterplot_feature_columns()

        shiny::selectInput(
          inputId  = ns("x_feature_choice"),
          label    = "Select X Feature",
          choices  = choices
        )
      })

      output$y_feature_selection_ui <- shiny::renderUI({
        shiny::req(display_feature_selection_ui(), input$x_feature_choice)
        choices <- scatterplot_feature_columns() %>%
          setdiff(input$x_feature_choice)

        shiny::selectInput(
          inputId  = ns("y_feature_choice"),
          label    = "Select Y Feature",
          choices  = choices
        )
      })

      if(is.null(x_feature_input)){
        x_feature <-
          shiny::reactive(
            get_scatterplot_x_feature(
              input$x_feature_choice,
              scatterplot_feature_columns()
            )
          )
      } else {
        x_feature <- x_feature_input
      }

      if(is.null(y_feature_input)){
        y_feature <-
          shiny::reactive(
            get_scatterplot_y_feature(
              input$y_feature_choice,
              scatterplot_feature_columns()
            )
          )
      } else {
        y_feature <- y_feature_input
      }

      formatted_scatterplot_data <- shiny::reactive({
        shiny::req(
          scatterplot_data(),
          x_feature(),
          y_feature(),
          x_feature() %in% colnames(scatterplot_data()),
          y_feature() %in% colnames(scatterplot_data())
        )

        shiny::validate(shiny::need(
          x_feature() != y_feature(),
          "Selected features to compare are the same, please select new features."
        ))

        format_scatterplot_data(
          scatterplot_data(), x_feature(), y_feature()
        )
      })

      output$scatterplot <- plotly::renderPlotly({
        plotly_scatter(
          formatted_scatterplot_data(),
          text_col = "text",
          xlab = x_feature(),
          ylab = y_feature(),
          title = selected_group(),
          identity_line = TRUE
        )
      })

      plotly_server(
        "scatterplot",
        plot_data = formatted_scatterplot_data
      )

      return(formatted_scatterplot_data)
    }
  )
}
