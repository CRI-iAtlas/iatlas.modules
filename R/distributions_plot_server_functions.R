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


format_distplot_data <- function(
  data,
  scale_method_choice,
  reorder_method_choice,
  feature_data = NULL,
  group_data = NULL
  ){

  distplot_data <- data %>%
    scale_tbl_value_column(scale_method_choice) %>%
    reafctor_by_tbl_value_column(reorder_method_choice) %>%
    dplyr::select("sample_name", "feature_name", "group_name", "feature_value")

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

  if(is.null(group_data)){
    distplot_data <- distplot_data %>%
      dplyr::mutate("group_display" = .data$group_name)
  } else {
    distplot_data <- distplot_data %>%
      dplyr::left_join(group_data, by = "group_name") %>%
      dplyr::mutate("group_display" = dplyr::if_else(
        is.na(.data$group_display),
        .data$group_name,
        .data$group_display
      ))
  }

  distplot_data %>%
    dplyr::select(
      "sample_name",
      "feature_name",
      "feature_display",
      "group_name",
      "group_display",
      "feature_value"
    )
}
