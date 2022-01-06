
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
    feature_data = shiny::reactive(example_iris_data_features_1_class2()),
    barplot_xlab = shiny::reactive("Species"),
    barplot_ylab = shiny::reactive("Height")
  )
  #
  # barplot_server(
  #   "barplot2",
  #   shiny::reactive(example_iris_data_func),
  #   feature_classes = shiny::reactive(c("Length", "Width")),
  #   barplot_xlab = shiny::reactive("Species"),
  #   drilldown = shiny::reactive(T)
  # )
  #
  # barplot_server(
  #   "barplot3",
  #   shiny::reactive(example_iris_data_func),
  #   barplot_xlab = shiny::reactive("Species"),
  #   drilldown = shiny::reactive(T)
  # )
  #
  # barplot_server(
  #   "barplot4",
  #   shiny::reactive(example_iris_data_func),
  #   barplot_xlab = shiny::reactive("Species"),
  #   drilldown = shiny::reactive(T),
  #   x_feature_input = shiny::reactive("Petal.Length"),
  #   y_feature_input = shiny::reactive("Petal.Width")
  # )
  #
  # barplot_server(
  #   "barplot5",
  #   shiny::reactive(get_tcga_cell_proportions),
  #   barplot_xlab    = shiny::reactive("Fraction type by group"),
  #   barplot_ylab    = shiny::reactive("Fraction mean"),
  #   barplot_label   = shiny::reactive("Fraction"),
  #   drilldown       = shiny::reactive(T),
  #   y_feature_input = shiny::reactive("Leukocyte Fraction"),
  #   x_feature_input = shiny::reactive("Stromal Fraction")
  # )

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
    feature_data = shiny::reactive(example_iris_data_features_2_classes()),
    group_data = shiny::reactive(example_iris_data_groups()),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Species")
  )

  distributions_plot_server(
    "distplot4",
    sample_data_function = shiny::reactive(example_iris_data),
    feature_data = shiny::reactive(example_iris_data_features_2_classes()),
    group_data = shiny::reactive(example_iris_data_groups2()),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Species")
  )


  # heatmap_server(
  #   "heatmap1",
  #   feature_classes = shiny::reactive(get_pcawg_feature_class_list()),
  #   response_features = shiny::reactive(get_pcawg_feature_list()),
  #   feature_data_function = shiny::reactive(get_pcawg_feature_values_by_class),
  #   response_data_function = shiny::reactive(get_pcawg_feature_values_by_feature),
  #   summarise_function_list = shiny::reactive(
  #     purrr::partial(stats::cor, method = "pearson")
  #   ),
  #   drilldown = shiny::reactive(T)
  # )
  #
  # heatmap_server(
  #   "heatmap2",
  #   feature_classes = shiny::reactive(get_pcawg_feature_class_list()),
  #   response_features = shiny::reactive(get_pcawg_feature_list()),
  #   feature_data_function = shiny::reactive(get_pcawg_feature_values_by_class),
  #   response_data_function = shiny::reactive(get_pcawg_feature_values_by_feature),
  #   summarise_function_list = shiny::reactive(
  #     list(
  #       "Pearson" = purrr::partial(stats::cor, method = "pearson"),
  #       "Spearman" = purrr::partial(stats::cor, method = "spearman")
  #       )
  #   ),
  #   drilldown = shiny::reactive(T)
  # )


}
