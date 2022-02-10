format_barplot_data <- function(data, feature_data = NULL){

  data <- data %>%
    dplyr::select("sample_name", "feature_name", "group_name", "feature_value")

  if(is.null(feature_data)){
    data <- data %>%
      dplyr::mutate("feature_display" = .data$feature_name)
  } else {
    data <- data %>%
      dplyr::left_join(feature_data, by = "feature_name") %>%
      dplyr::mutate("feature_display" = dplyr::if_else(
        is.na(.data$feature_display),
        .data$feature_name,
        .data$feature_display
      ))
  }

  data %>%
    dplyr::select(
      "sample_name",
      "group_name",
      "feature_display",
      "feature_value"
    )
}


