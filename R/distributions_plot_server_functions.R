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


get_distributions_feature_list <- function(
  features,
  feature_class_choice,
  display_feature_class_selection_ui
  ){

  if(display_feature_class_selection_ui){
    tbl <- features %>%
      dplyr::select(dplyr::all_of(c(
        "feature_class" = feature_class_choice,
        "feature_display",
        "feature_name"
      )))
  } else {
    tbl <- features %>%
      dplyr::select(
        "feature_class",
        "feature_display",
        "feature_name"
      )
  }
  create_nested_named_list(tbl)
}

create_distplot_data <- function(
  plot_data_function,
  feature_choice,
  scale_method_choice,
  reorder_method_choice
  ){

  data <-
    plot_data_function(.feature = feature_choice) %>%
    scale_tbl_value_column(scale_method_choice) %>%
    reafctor_by_tbl_value_column(reorder_method_choice) %>%
    dplyr::select(dplyr::any_of(
      c(
        "sample_name", "feature_name", "feature_value", "group_name", "group_description",
        "group_color"
      )
    ))
}
