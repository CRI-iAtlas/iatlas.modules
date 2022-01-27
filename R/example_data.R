utils::globalVariables("iris")

# # Example Starwars Data -------------------------------------------------------
# example_starwars_data <- function(){
#   dplyr::starwars %>%
#     dplyr::select(
#       "sample_name" = "name",
#       "group_name" = "species",
#       "height",
#       "mass"
#     ) %>%
#     tidyr::pivot_longer(
#       -c("sample_name", "group_name"), names_to = "feature_name", values_to = "feature_value"
#     ) %>%
#     dplyr::mutate("feature_display" = .data$feature_name)
# }
#
# example_starwars_data_func <- function(.feature_class){
#   example_starwars_data()
# }

# Example Iris Data ----------------------------------------------------------

example_iris_data <- function(.feature = NULL, .feature_class = NULL){
  tbl <- iris %>%
    dplyr::as_tibble() %>%
    dplyr::rename("group_name" = "Species") %>%
    dplyr::mutate(
      "sample_name" = as.character(1:dplyr::n())
    ) %>%
    tidyr::pivot_longer(
      !c("group_name", "sample_name"),
      names_to = "feature_name",
      values_to = "feature_value"
    ) %>%
    dplyr::inner_join(
      dplyr::tribble(
        ~feature_name,  ~feature_class,
        "Sepal.Length", "Length",
        "Sepal.Width",  "Width",
        "Petal.Length", "Length",
        "Petal.Width",  "Width",
      ),
      by = "feature_name"
    )


  if(!is.null(.feature)){
    tbl <- dplyr::filter(tbl, .data$feature_name == .feature)
  }

  if(!is.null(.feature_class)){
    tbl <- dplyr::filter(tbl, .data$feature_class == .feature_class)
  }

  dplyr::select(
    tbl,
    "sample_name",
    "feature_name",
    "group_name",
    "feature_value"
  )
}


example_iris_data_missing_column <- function(.feature = NULL, .feature_class = NULL){
  dplyr::select(example_iris_data(), "sample_name", "feature_name", "group_name")
}

example_iris_data_features_2_classes <- function(){
  dplyr::tribble(
    ~feature_name,  ~feature_display, ~Class1,  ~Class2,
    "Sepal.Length", "Sepal Length",   "Length", "Sepal",
    "Sepal.Width",  "Sepal Width",    "Width",  "Sepal",
    "Petal.Length", "Petal Length",   "Length", "Petal",
    "Petal.Width",  "Petal Width",    "Width",  "Petal"
  )
}

example_iris_data_features_1_class <- function(){
  dplyr::select(
    example_iris_data_features_2_classes(),
    "feature_name",
    "feature_display",
    "Class1"
  )
}

example_iris_data_features_1_class2 <- function(){
  dplyr::select(
    example_iris_data_features_2_classes(),
    "feature_name",
    "feature_display",
    "feature_class" = "Class1"
  )
}

example_iris_data_features_missing_column <- function(){
  dplyr::select(
    example_iris_data_features_2_classes(),
    "feature_name",
    "Class1"
  )
}

example_iris_data_features_duplicated_feature <- function(){
  tibble::add_row(
    example_iris_data_features_1_class(),
    "feature_name" = "Sepal.Length",
    "feature_display" = "Sepal Width",
    "Class1" = "Length"
  )
}

example_iris_data_groups <- function(){
  dplyr::tribble(
    ~group_name,  ~group_display, ~group_color, ~group_description,
    "setosa",     "Setosa",       "#FF0000",    "Iris Species: Setosa",
    "versicolor", "Versicolor",   "#0000FF",    "Iris Species: Versicolor",
    "virginica",  "Virginica",    "#FFFF00",    "Iris Species: Virginica",
  )
}

example_iris_data_groups2 <- function(){
  dplyr::tribble(
    ~group_name,  ~group_display,
    "setosa",     "Setosa",
    "versicolor", "Versicolor",
    "virginica",  "Virginica",
  )
}

example_iris_data_groups_missing_column <- function(){
  dplyr::tribble(
    ~group_name,  ~group_display, ~group_color, ~group_description,
    "setosa",     "Setosa",       "#FF0000",    "Iris Species: Setosa",
    "versicolor", "Versicolor",   "#0000FF",    "Iris Species: Versicolor",
    "virginica",  "Virginica",    "#FFFF00",    "Iris Species: Virginica",
  )
}

example_scatterplot_iris_data_4_features <- function(){
  tbl <- iris %>%
    dplyr::as_tibble() %>%
    dplyr::mutate("sample_name" = as.character(1:dplyr::n())) %>%
    dplyr::rename("group_display" = "Species")
}

example_scatterplot_iris_data_3_features <- function(){
  tbl <- example_scatterplot_iris_data_4_features() %>%
    dplyr::select("sample_name", "group_display", "Sepal.Length", "Sepal.Width", "Petal.Length")
}

example_scatterplot_iris_data_2_features <- function(){
  tbl <- example_scatterplot_iris_data_4_features() %>%
    dplyr::select("sample_name", "group_display", "Sepal.Length", "Sepal.Width")
}


# example_iris_data <- function(){
#   iris %>%
#     dplyr::as_tibble() %>%
#     dplyr::mutate("sample_name" = as.character(1:dplyr::n())) %>%
#     dplyr::rename("group_name" = "Species") %>%
#     tidyr::pivot_longer(
#       !c("group_name", "sample_name"),
#       names_to = "feature_name",
#       values_to = "feature_value"
#     ) %>%
#     dplyr::inner_join(
#       dplyr::tribble(
#         ~group_name,  ~group_color, ~group_description,
#         "setosa",     "#FF0000",    "Iris Species: Setosa",
#         "versicolor", "#0000FF",    "Iris Species: Versicolor",
#         "virginica",  "#FFFF00",    "Iris Species: Virginica",
#       ),
#       by = "group_name"
#     ) %>%
#     dplyr::inner_join(
#       dplyr::tribble(
#         ~feature_class, ~feature_name,   ~feature_class2, ~feature_display,
#         "Length",       "Sepal.Length",  "Sepal",         "Sepal Length",
#         "Width",        "Sepal.Width",   "Sepal",         "Sepal Width",
#         "Length",       "Petal.Length",  "Petal",         "Petal Length",
#         "Width",        "Petal.Width",   "Petal",         "Petal Width"
#       ),
#       by = "feature_name"
#     )
# }
#
# example_iris_data_func <- function(.feature_class = NULL, .feature = NULL){
#   iris_data <- example_iris_data()
#   if (!is.null(.feature_class)){
#     iris_data <- dplyr::filter(iris_data, .data$feature_class == .feature_class)
#   }
#   if (!is.null(.feature)){
#     iris_data <- dplyr::filter(iris_data, .data$feature_name == .feature)
#   }
#   dplyr::select(iris_data, -"feature_class")
# }

# pcawg examples ---------------------------------------------------------------
#
# get_pcawg_feature_class_list <- function(){
#   features <-
#     iatlas.api.client::query_features(cohorts = "PCAWG") %>%
#     dplyr::select("class") %>%
#     dplyr::distinct() %>%
#     dplyr::arrange(.data$class) %>%
#     dplyr::pull("class")
# }
#
# get_pcawg_feature_list <- function(){
#   features <-
#     iatlas.api.client::query_features(cohorts = "PCAWG") %>%
#     dplyr::arrange(.data$class) %>%
#     create_nested_named_list(
#       names_col1 = "class",
#       names_col2 = "display",
#       values_col = "name"
#     )
# }
#
# get_pcawg_heatmap_example <- function(){
#   feature_data <-
#     iatlas.api.client::query_feature_values(
#       cohorts = "PCAWG", feature_classes = "Adaptive Receptor - T cell"
#     ) %>%
#     dplyr::select(
#       "sample_name" = "sample",
#       "feature_name",
#       "feature_display",
#       "feature_value",
#       "feature_order"
#     )
#
#   response_data <-
#     iatlas.api.client::query_feature_values(
#       cohorts = "PCAWG", features = "age_at_diagnosis"
#     ) %>%
#     dplyr::select(
#       "sample_name" = "sample",
#       "response_name" = "feature_name",
#       "response_display" = "feature_display",
#       "response_value" = "feature_value"
#     )
#
#   group_data <-
#     iatlas.api.client::query_tag_samples(
#       cohorts = "PCAWG", parent_tags = "Immune_Subtype"
#     ) %>%
#     dplyr::select(
#       "sample_name" = "sample_name",
#       "group_name" = "tag_short_display",
#       "group_description" = "tag_characteristics"
#     )
#
#   plot_data <-
#     dplyr::inner_join(
#       feature_data,
#       response_data,
#       by = "sample_name"
#     ) %>%
#     dplyr::inner_join(
#       group_data,
#       by = "sample_name"
#     )
# }
#
# # scatterplot examples ----
#
# get_pcawg_scatterplot_example <- function(){
#   feature_data <-
#     iatlas.api.client::query_feature_values(
#       cohorts = "PCAWG", feature_classes = "Adaptive Receptor - T cell"
#     ) %>%
#     dplyr::select("sample_name" = "sample", "feature_display", "feature_value") %>%
#     tidyr::pivot_wider(
#       names_from = "feature_display", values_from = "feature_value"
#     ) %>%
#     tidyr::drop_na()
#
#   group_data <-
#     iatlas.api.client::query_tag_samples(
#       cohorts = "PCAWG", parent_tags = "Immune_Subtype"
#     ) %>%
#     dplyr::select("sample_name", "group_name" = "tag_short_display")
#
#   plot_data <-
#     dplyr::inner_join(
#       feature_data,
#       group_data,
#       by = "sample_name"
#     )
# }
#
# # plot data example functions ----
#
# get_pcawg_feature_values_by_feature <- function(.feature){
#   feature_data <-
#     iatlas.api.client::query_feature_values(
#       cohorts = "PCAWG", features = .feature
#     ) %>%
#     dplyr::select(
#       "sample_name" = "sample",
#       "feature_name",
#       "feature_display",
#       "feature_value"
#     )
#
#   group_data <-
#     iatlas.api.client::query_tag_samples(
#       cohorts = "PCAWG", parent_tags = "Immune_Subtype"
#     ) %>%
#     dplyr::select(
#       "sample_name",
#       "group_name" = "tag_short_display",
#       "group_description" = "tag_characteristics",
#       "group_color" = "tag_color"
#     )
#
#   dplyr::inner_join(
#     feature_data,
#     group_data,
#     by = "sample_name"
#   )
# }
#
# get_pcawg_feature_values_by_class <- function(.feature_class){
#   feature_data <-
#     iatlas.api.client::query_feature_values(
#       cohorts = "PCAWG", feature_classes = .feature_class
#     ) %>%
#     dplyr::select(
#       "sample_name" = "sample",
#       "feature_name",
#       "feature_display",
#       "feature_value",
#       "feature_order"
#     )
#
#   group_data <-
#     iatlas.api.client::query_tag_samples(
#       cohorts = "PCAWG", parent_tags = "Immune_Subtype"
#     ) %>%
#     dplyr::select(
#       "sample_name",
#       "group_name" = "tag_short_display",
#       "group_description" = "tag_characteristics",
#       "group_color" = "tag_color"
#     )
#
#   dplyr::inner_join(
#     feature_data,
#     group_data,
#     by = "sample_name"
#   )
# }
#
# get_feature_values_by_class_no_data <- function(.feature_class){
#   dplyr::tibble(
#     "sample_name" = character(),
#     "group_name" = character(),
#     "feature_name" = character(),
#     "feature_display" = character(),
#     "feature_value" = character(),
#     "feature_order" = character(),
#     "group_description" = character(),
#     "group_color" = character()
#   )
# }
#
# get_feature_values_by_feature_no_data <- function(.feature){
#   dplyr::tibble(
#     "sample_name" = character(),
#     "group_name" = character(),
#     "feature_name" = character(),
#     "feature_display" = character(),
#     "feature_value" = character(),
#     "feature_order" = character(),
#     "group_description" = character(),
#     "group_color" = character()
#   )
# }
#
# # TCGA
#
# get_tcga_feature_values_by_feature <- function(.feature){
#   feature_data <-
#     iatlas.api.client::query_feature_values(
#       cohorts = "TCGA", features = .feature
#     ) %>%
#     dplyr::select(
#       "sample_name" = "sample",
#       "feature_name",
#       "feature_display",
#       "feature_value"
#     )
#
#   group_data <-
#     iatlas.api.client::query_tag_samples(
#       cohorts = "TCGA", parent_tags = "Immune_Subtype"
#     ) %>%
#     dplyr::select(
#       "sample_name",
#       "group_name" = "tag_short_display",
#       "group_description" = "tag_characteristics",
#       "group_color" = "tag_color"
#     )
#
#   dplyr::inner_join(
#     feature_data,
#     group_data,
#     by = "sample_name"
#   )
# }
#
# get_tcga_cell_proportions <- function(.feature_class){
#   result <-
#     get_tcga_feature_values_by_feature(
#       c("leukocyte_fraction", "Stromal_Fraction", "Tumor_fraction")
#     )  %>%
#     dplyr::select(
#       "sample_name",
#       "group_name",
#       "feature_name",
#       "feature_display",
#       "feature_value",
#       "group_description"
#     )
# }
#
#
#
