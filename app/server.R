
server <- function(input, output, session) {

  barplot_server(
    "barplot1",
    shiny::reactive(example_starwars_data_func),
    barplot_xlab = shiny::reactive("Species"),
    barplot_ylab = shiny::reactive("Height")
  )

  barplot_server(
    "barplot2",
    shiny::reactive(example_iris_data_func),
    feature_classes = shiny::reactive(c("Length", "Width")),
    barplot_xlab = shiny::reactive("Species"),
    drilldown = shiny::reactive(T)
  )

  barplot_server(
    "barplot3",
    shiny::reactive(example_iris_data_func),
    barplot_xlab = shiny::reactive("Species"),
    drilldown = shiny::reactive(T)
  )

  barplot_server(
    "barplot4",
    shiny::reactive(example_iris_data_func),
    barplot_xlab = shiny::reactive("Species"),
    drilldown = shiny::reactive(T),
    x_feature_input = shiny::reactive("Petal.Length"),
    y_feature_input = shiny::reactive("Petal.Width")
  )

  barplot_server(
    "barplot5",
    shiny::reactive(get_tcga_cell_proportions),
    barplot_xlab    = shiny::reactive("Fraction type by group"),
    barplot_ylab    = shiny::reactive("Fraction mean"),
    barplot_label   = shiny::reactive("Fraction"),
    drilldown       = shiny::reactive(T),
    y_feature_input = shiny::reactive("Leukocyte Fraction"),
    x_feature_input = shiny::reactive("Stromal Fraction")
  )

  distributions_plot_server(
    "distplot1",
    plot_data_function = shiny::reactive(example_iris_data_func),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Species")
  )

  distributions_plot_server(
    "distplot2",
    plot_data_function = shiny::reactive(example_iris_data_func),
    features = shiny::reactive(
      example_iris_data() %>%
        dplyr::select(
          "feature_class",
          "feature_name",
          "feature_display"
        ) %>%
        dplyr::distinct()
    ),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Species")
  )

  distributions_plot_server(
    "distplot3",
    plot_data_function = shiny::reactive(example_iris_data_func),
    features = shiny::reactive(
      example_iris_data() %>%
        dplyr::select(
          "Class1" = "feature_class",
          "Class2" = "feature_class2",
          "feature_name",
          "feature_display"
        ) %>%
        dplyr::distinct()
    ),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Species"),
    scale_method_default = shiny::reactive("Log2"),
    feature_default = shiny::reactive("Petal.Length")
  )

  distributions_plot_server(
    "distplot4",
    plot_data_function <- shiny::reactive({
      function(.feature){
        iatlas.modules2::pcawg_immune_subtype_cohort_obj$get_gene_values(entrez = as.integer(.feature)) %>%
          dplyr::select(
            "sample_name",
            "group_name" = "group_short_name",
            "feature_name" = "entrez",
            "feature_display" = "hgnc",
            "feature_value" = "rna_seq_expr",
            "group_description" = "group_characteristics",
            "group_color" = "group_color"
          )
      }
    }),
    features <- shiny::reactive({
      iatlas.api.client::query_immunomodulators() %>%
        dplyr::select(
          "feature_name" = "entrez",
          "feature_display" = "hgnc",
          "Gene Family" = "gene_family",
          "Gene Function" = "gene_function",
          "Immune Checkpoint" = "immune_checkpoint",
          "Super Category" = "super_category"
        )
    }),
    drilldown = shiny::reactive(T),
    distplot_xlab = shiny::reactive("Immune_Subtype"),
    scale_method_default = shiny::reactive("Log10"),
    feature_default = shiny::reactive("Petal.Length")
  )

  heatmap_server(
    "heatmap1",
    feature_classes = shiny::reactive(get_pcawg_feature_class_list()),
    response_features = shiny::reactive(get_pcawg_feature_list()),
    feature_data_function = shiny::reactive(get_pcawg_feature_values_by_class),
    response_data_function = shiny::reactive(get_pcawg_feature_values_by_feature),
    summarise_function_list = shiny::reactive(
      purrr::partial(stats::cor, method = "pearson")
    ),
    drilldown = shiny::reactive(T)
  )

  heatmap_server(
    "heatmap2",
    feature_classes = shiny::reactive(get_pcawg_feature_class_list()),
    response_features = shiny::reactive(get_pcawg_feature_list()),
    feature_data_function = shiny::reactive(get_pcawg_feature_values_by_class),
    response_data_function = shiny::reactive(get_pcawg_feature_values_by_feature),
    summarise_function_list = shiny::reactive(
      list(
        "Pearson" = purrr::partial(stats::cor, method = "pearson"),
        "Spearman" = purrr::partial(stats::cor, method = "spearman")
        )
    ),
    drilldown = shiny::reactive(T)
  )


}
