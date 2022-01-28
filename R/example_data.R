utils::globalVariables("iris")

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
    ~feature_name,  ~feature_display, ~Class1,  ~Class2, ~feature_order,
    "Sepal.Length", "Sepal Length",   "Length", "Sepal", 1,
    "Sepal.Width",  "Sepal Width",    "Width",  "Sepal", 2,
    "Petal.Length", "Petal Length",   "Length", "Petal", 3,
    "Petal.Width",  "Petal Width",    "Width",  "Petal", 4
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
    "feature_class" = "Class1",
    "feature_order"
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

