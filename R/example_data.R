utils::globalVariables("iris")

#' Example Starwars Data
example_starwars_data <- function(){
  dplyr::starwars %>%
    dplyr::select(
      "sample" = "name",
      "group" = "species",
      "height",
      "mass"
    ) %>%
    tidyr::pivot_longer(
      -c("sample", "group"), names_to = "feature", values_to = "feature_value"
    )
}

example_starwars_data_func <- function(.feature_class){
  example_starwars_data()
}

#' Example Iris Data
#' @importFrom magrittr %>%
example_iris_data <- function(){
  iris %>%
    dplyr::as_tibble() %>%
    dplyr::mutate("sample" = as.character(1:dplyr::n())) %>%
    tidyr::pivot_longer(
      !c("Species", "sample"),
      names_to = "feature",
      values_to = "feature_value"
    ) %>%
    dplyr::rename("group" = "Species") %>%
    dplyr::inner_join(
      dplyr::tribble(
        ~group,       ~color,
        "setosa",     "#FF0000",
        "versicolor", "#0000FF",
        "virginica",  "#FFFF00"
      ),
      by = "group"
    ) %>%
    dplyr::mutate(
      "group_description" = stringr::str_c("Iris Species: ", .data$group),
    ) %>%
    dplyr::inner_join(
      dplyr::tribble(
        ~feature_class, ~feature,        ~feature_class2, ~feature_display,
        "Length",       "Sepal.Length",  "Sepal",         "Sepal Length",
        "Width",        "Sepal.Width",   "Sepal",         "Sepal Width",
        "Length",       "Petal.Length",  "Petal",         "Petal Length",
        "Width",        "Petal.Width",   "Petal",         "Petal Width"
      ),
      by = "feature"
    )
}

example_iris_data_func <- function(.feature_class = NULL, .feature = NULL){
  iris_data <- example_iris_data()
  if (!is.null(.feature_class)){
    iris_data <- dplyr::filter(iris_data, .data$feature_class == .feature_class)
  }
  if (!is.null(.feature)){
    iris_data <- dplyr::filter(iris_data, .data$feature == .feature)
  }
  dplyr::select(iris_data, -"feature_class")
}

# heatmap examples ----

get_pcawg_feature_class_list <- function(){
  features <-
    iatlas.api.client::query_features(cohorts = "PCAWG") %>%
    dplyr::select("class") %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$class)
}

get_pcawg_feature_list <- function(){
  features <-
    iatlas.api.client::query_features(cohorts = "PCAWG") %>%
    dplyr::arrange(.data$class) %>%
    create_nested_named_list(
      names_col1 = "class",
      names_col2 = "display",
      values_col = "name"
    )
}

get_pcawg_heatmap_example <- function(){
  feature_data <-
    iatlas.api.client::query_feature_values(
      cohorts = "PCAWG", feature_classes = "Adaptive Receptor - T cell"
    ) %>%
    dplyr::select(
      "sample",
      "feature" = "feature_display",
      "feature_value",
      "order" = "feature_order"
    )

  response_data <-
    iatlas.api.client::query_feature_values(
      cohorts = "PCAWG", features = "age_at_diagnosis"
    ) %>%
    dplyr::select(
      "sample",
      "response" = "feature_display",
      "response_value" = "feature_value"
    )

  group_data <-
    iatlas.api.client::query_tag_samples(
      cohorts = "PCAWG", parent_tags = "Immune_Subtype"
    ) %>%
    dplyr::select(
      "sample" = "sample_name",
      "group" = "tag_short_display",
      "group_description" = "tag_characteristics"
    )

  plot_data <-
    dplyr::inner_join(
      feature_data,
      response_data,
      by = "sample"
    ) %>%
    dplyr::inner_join(
      group_data,
      by = "sample"
    )
}

# scatterplot examples ----

get_pcawg_scatterplot_example <- function(){
  feature_data <-
    iatlas.api.client::query_feature_values(
      cohorts = "PCAWG", feature_classes = "Adaptive Receptor - T cell"
    ) %>%
    dplyr::select("sample","feature_display", "feature_value") %>%
    tidyr::pivot_wider(
      names_from = "feature_display", values_from = "feature_value"
    ) %>%
    tidyr::drop_na()

  group_data <-
    iatlas.api.client::query_tag_samples(
      cohorts = "PCAWG", parent_tags = "Immune_Subtype"
    ) %>%
    dplyr::select("sample" = "sample_name", "group" = "tag_short_display")

  plot_data <-
    dplyr::inner_join(
      feature_data,
      group_data,
      by = "sample"
    )
}

# plot data example functions ----

get_pcawg_feature_values_by_feature <- function(.feature){
  feature_data <-
    iatlas.api.client::query_feature_values(
      cohorts = "PCAWG", features = .feature
    ) %>%
    dplyr::select("sample", "feature" = "feature_display", "feature_value")

  group_data <-
    iatlas.api.client::query_tag_samples(
      cohorts = "PCAWG", parent_tags = "Immune_Subtype"
    ) %>%
    dplyr::select(
      "sample" = "sample_name",
      "group" = "tag_short_display",
      "group_description" = "tag_characteristics",
      "color" = "tag_color"
    )

  dplyr::inner_join(
    feature_data,
    group_data,
    by = "sample"
  )
}

get_pcawg_feature_values_by_class <- function(.class){
  feature_data <-
    iatlas.api.client::query_feature_values(
      cohorts = "PCAWG", feature_classes = .class
    ) %>%
    dplyr::select(
      "sample", "feature" = "feature_name", "feature_value", "feature_order"
    )

  group_data <-
    iatlas.api.client::query_tag_samples(
      cohorts = "PCAWG", parent_tags = "Immune_Subtype"
    ) %>%
    dplyr::select(
      "sample" = "sample_name",
      "group" = "tag_short_display",
      "group_description" = "tag_characteristics",
      "color" = "tag_color"
    )

  dplyr::inner_join(
    feature_data,
    group_data,
    by = "sample"
  )
}

