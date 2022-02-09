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
        ),
        shiny::tabPanel(
          "Barplots2",
          barplot_ui2("barplot4"),
        ),
        shiny::tabPanel(
          "Distribution Plots",
          distributions_plot_ui("distplot1", title = "Example 1"),
          distributions_plot_ui("distplot2", title = "Example 2"),
          distributions_plot_ui("distplot3", title = "Example 3"),
          distributions_plot_ui("distplot4", title = "Example 4")
        ),
        shiny::tabPanel(
          "Distribution Plots2",
          distributions_plot_ui2("distplot5"),
          distributions_plot_ui2("distplot6")
        ),
        shiny::tabPanel(
          "Heatmaps",
          heatmap_ui("heatmap1"),
          heatmap_ui("heatmap2")
        ),
        shiny::tabPanel(
          "Heatmaps2",
          heatmap_ui2("heatmap3"),
          heatmap_ui2("heatmap4")
        )
      )
    )
  )
}
