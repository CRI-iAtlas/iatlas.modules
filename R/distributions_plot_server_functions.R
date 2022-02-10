get_distributions_feature_classes <- function(features){
  if(is.null(features)){
    return(character(0))
  } else {
    features %>%
      colnames() %>%
      setdiff(c("feature_name", "feature_display")) %>%
      return()
  }
}


get_distributions_feature_list <- function(features, feature_class_choice){
  features %>%
    dplyr::select(dplyr::all_of(c(
      "feature_class" = feature_class_choice,
      "feature_display",
      "feature_name"
    ))) %>%
    create_nested_named_list()
}

format_distplot_data <- function(data, feature_data = NULL){

  distplot_data <- data %>%
    dplyr::select(
      "sample_name",
      "feature_name",
      "group_name",
      "dataset_name",
      "feature_value"
    )

  if(is.null(feature_data)){
    distplot_data <- distplot_data %>%
      dplyr::mutate("feature_display" = .data$feature_name)
  } else {
    distplot_data <- distplot_data %>%
      dplyr::left_join(feature_data, by = "feature_name") %>%
      dplyr::mutate("feature_display" = dplyr::if_else(
        is.na(.data$feature_display),
        .data$feature_name,
        .data$feature_display
      ))
  }

  distplot_data %>%
    dplyr::select(
      "sample_name",
      "feature_name",
      "feature_display",
      "group_name",
      "dataset_name",
      "feature_value"
    )
}
