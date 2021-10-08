ui <- function() {
  shiny::fluidPage(

    shiny::titlePanel("Availible Modules"),
    shiny::mainPanel(
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          "Barplots",
          barplot_ui("barplot1", title = "Example 1"),
          barplot_ui("barplot2", title = "Example 2"),
          barplot_ui("barplot3", title = "Example 3"),
          barplot_ui("barplot4", title = "Example 4")
        ),
        shiny::tabPanel(
          "Distribution Plots",
          distributions_plot_ui("distplot1", title = "Example 1"),
          distributions_plot_ui("distplot2", title = "Example 2"),
          distributions_plot_ui("distplot3", title = "Example 3"),
          distributions_plot_ui("distplot4", title = "Example 4")
        ),
        shiny::tabPanel(
          "Heatmaps",
          heatmap_ui("heatmap1"),
          heatmap_ui("heatmap2")
        )
      )
    )
  )
}
