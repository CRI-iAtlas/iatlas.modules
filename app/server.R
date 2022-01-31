
server <- function(input, output, session) {

  barplot_server(
    "barplot1",
    shiny::reactive(example_iris_data),
    drilldown = shiny::reactive(T),
    barplot_xlab = shiny::reactive("Species"),
    barplot_ylab = shiny::reactive("Height")
  )

  barplot_server(
    "barplot2",
    shiny::reactive(example_iris_data),
    drilldown = shiny::reactive(T),
    feature_data = shiny::reactive(example_iris_data_features_1_class()),
    barplot_xlab = shiny::reactive("Species"),
    barplot_ylab = shiny::reactive("Height")
  )

  distributions_plot_server(
    "distplot1",
    sample_data_function = shiny::reactive(example_iris_data),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Species")
  )

  distributions_plot_server(
    "distplot2",
    sample_data_function = shiny::reactive(example_iris_data),
    feature_data = shiny::reactive(example_iris_data_features_1_class()),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Species")
  )

  distributions_plot_server(
    "distplot3",
    sample_data_function = shiny::reactive(example_iris_data),
    feature_data = shiny::reactive(example_iris_data_feature_data()),
    group_data = shiny::reactive(example_iris_data_groups()),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Species")
  )

  distributions_plot_server(
    "distplot4",
    sample_data_function = shiny::reactive(example_iris_data),
    feature_data = shiny::reactive(example_iris_data_feature_data()),
    group_data = shiny::reactive(example_iris_data_groups2()),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Species")
  )


  heatmap_server(
    "heatmap1",
    feature_sample_data_function = shiny::reactive(example_iris_data),
    response_sample_data_function = shiny::reactive(example_iris_data),
    feature_data = shiny::reactive(example_iris_data_features_1_class()),
    response_data = shiny::reactive(example_iris_data_features_1_class()),
    group_data = shiny::reactive(example_iris_data_groups()),
    summarise_function_list = shiny::reactive(
      purrr::partial(stats::cor, method = "pearson")
    ),
    drilldown = shiny::reactive(T)
  )

  heatmap_server(
    "heatmap2",
    feature_sample_data_function = shiny::reactive(example_iris_data),
    response_sample_data_function = shiny::reactive(example_iris_data),
    feature_data = shiny::reactive(example_iris_data_features_1_class()),
    response_data = shiny::reactive(example_iris_data_features_1_class()),
    group_data = shiny::reactive(example_iris_data_groups()),
    summarise_function_list = shiny::reactive(
      list(
        "Pearson" = purrr::partial(stats::cor, method = "pearson"),
        "Spearman" = purrr::partial(stats::cor, method = "spearman")
        )
    ),
    drilldown = shiny::reactive(T)
  )


}
