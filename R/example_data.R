utils::globalVariables("iris")

#' Example Starwars Data
example_starwars_data <- function(){
  dplyr::starwars %>%
    dplyr::select(
      "sample_name" = "name",
      "group_name" = "species",
      "height",
      "mass"
    ) %>%
    tidyr::pivot_longer(
      -c("sample_name", "group_name"), names_to = "feature_name", values_to = "feature_value"
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
    dplyr::mutate("sample_name" = as.character(1:dplyr::n())) %>%
    dplyr::rename("group_name" = "Species") %>%
    tidyr::pivot_longer(
      !c("group_name", "sample_name"),
      names_to = "feature_name",
      values_to = "feature_value"
    ) %>%
    dplyr::inner_join(
      dplyr::tribble(
        ~group_name,  ~group_color, ~group_description,
        "setosa",     "#FF0000",    "Iris Species: Setosa",
        "versicolor", "#0000FF",    "Iris Species: Versicolor",
        "virginica",  "#FFFF00",    "Iris Species: Virginica",
      ),
      by = "group_name"
    ) %>%
    dplyr::inner_join(
      dplyr::tribble(
        ~feature_class, ~feature_name,   ~feature_class2, ~feature_display,
        "Length",       "Sepal.Length",  "Sepal",         "Sepal Length",
        "Width",        "Sepal.Width",   "Sepal",         "Sepal Width",
        "Length",       "Petal.Length",  "Petal",         "Petal Length",
        "Width",        "Petal.Width",   "Petal",         "Petal Width"
      ),
      by = "feature_name"
    )
}

example_iris_data_func <- function(.feature_class = NULL, .feature = NULL){
  iris_data <- example_iris_data()
  if (!is.null(.feature_class)){
    iris_data <- dplyr::filter(iris_data, .data$feature_class == .feature_class)
  }
  if (!is.null(.feature)){
    iris_data <- dplyr::filter(iris_data, .data$feature_name == .feature)
  }
  dplyr::select(iris_data, -"feature_class")
}

# heatmap examples ----

get_pcawg_feature_class_list <- function(){
  features <-
    iatlas.api.client::query_features(cohorts = "PCAWG") %>%
    dplyr::select("class") %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$class) %>%
    dplyr::pull("class")
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
    print()
    dplyr::select(
      "sample_name" = "sample",
      "feature_name",
      "feature_display",
      "feature_value",
      "feature_order"
    )

  response_data <-
    iatlas.api.client::query_feature_values(
      cohorts = "PCAWG", features = "age_at_diagnosis"
    ) %>%
    dplyr::select(
      "sample_name" = "sample",
      "response_name" = "feature_name",
      "response_display" = "feature_display",
      "response_value" = "feature_value"
    )

  group_data <-
    iatlas.api.client::query_tag_samples(
      cohorts = "PCAWG", parent_tags = "Immune_Subtype"
    ) %>%
    dplyr::select(
      "sample_name" = "sample_name",
      "group_name" = "tag_short_display",
      "group_description" = "tag_characteristics"
    )

  plot_data <-
    dplyr::inner_join(
      feature_data,
      response_data,
      by = "sample_name"
    ) %>%
    dplyr::inner_join(
      group_data,
      by = "sample_name"
    )
}

# scatterplot examples ----

get_pcawg_scatterplot_example <- function(){
  feature_data <-
    iatlas.api.client::query_feature_values(
      cohorts = "PCAWG", feature_classes = "Adaptive Receptor - T cell"
    ) %>%
    dplyr::select("sample_name" = "sample", "feature_display", "feature_value") %>%
    tidyr::pivot_wider(
      names_from = "feature_display", values_from = "feature_value"
    ) %>%
    tidyr::drop_na()

  group_data <-
    iatlas.api.client::query_tag_samples(
      cohorts = "PCAWG", parent_tags = "Immune_Subtype"
    ) %>%
    dplyr::select("sample_name", "group_name" = "tag_short_display")

  plot_data <-
    dplyr::inner_join(
      feature_data,
      group_data,
      by = "sample_name"
    )
}

# plot data example functions ----

get_pcawg_feature_values_by_feature <- function(.feature){
  feature_data <-
    iatlas.api.client::query_feature_values(
      cohorts = "PCAWG", features = .feature
    ) %>%
    dplyr::select(
      "sample_name" = "sample",
      "feature_name",
      "feature_display",
      "feature_value"
    )

  group_data <-
    iatlas.api.client::query_tag_samples(
      cohorts = "PCAWG", parent_tags = "Immune_Subtype"
    ) %>%
    dplyr::select(
      "sample_name",
      "group_name" = "tag_short_display",
      "group_description" = "tag_characteristics",
      "group_color" = "tag_color"
    )

  dplyr::inner_join(
    feature_data,
    group_data,
    by = "sample_name"
  )
}

get_pcawg_feature_values_by_class <- function(.feature_class){
  feature_data <-
    iatlas.api.client::query_feature_values(
      cohorts = "PCAWG", feature_classes = .feature_class
    ) %>%
    dplyr::select(
      "sample_name" = "sample",
      "feature_name",
      "feature_display",
      "feature_value",
      "feature_order"
    )

  group_data <-
    iatlas.api.client::query_tag_samples(
      cohorts = "PCAWG", parent_tags = "Immune_Subtype"
    ) %>%
    dplyr::select(
      "sample_name",
      "group_name" = "tag_short_display",
      "group_description" = "tag_characteristics",
      "group_color" = "tag_color"
    )

  dplyr::inner_join(
    feature_data,
    group_data,
    by = "sample_name"
  )
}

get_feature_values_by_class_no_data <- function(.feature_class){
  dplyr::tibble(
    "sample_name" = character(),
    "group_name" = character(),
    "feature_name" = character(),
    "feature_display" = character(),
    "feature_value" = character(),
    "feature_order" = character(),
    "group_description" = character(),
    "group_color" = character()
  )
}

get_feature_values_by_feature_no_data <- function(.feature){
  dplyr::tibble(
    "sample_name" = character(),
    "group_name" = character(),
    "feature_name" = character(),
    "feature_display" = character(),
    "feature_value" = character(),
    "feature_order" = character(),
    "group_description" = character(),
    "group_color" = character()
  )
}
