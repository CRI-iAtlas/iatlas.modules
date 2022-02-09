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

summarise_barplot_se <- function(data, title){
  data %>%
    dplyr::select("group_display", "feature_display", "feature_value") %>%
    tidyr::drop_na() %>%
    dplyr::group_by_at(dplyr::vars("group_display", "feature_display")) %>%
    dplyr::summarise(
      "MEAN" = mean(.data$feature_value),
      "SE" = .data$MEAN / sqrt(dplyr::n()),
      .groups = "drop"
    ) %>%
    create_plotly_text(
      .data$feature_display,
      .data$group_display,
      c("MEAN", "SE"),
      title
    )
}

get_barplot_group_data <- function(barplot_data){
  barplot_data %>%
    dplyr::select("group_display", "group_description") %>%
    dplyr::distinct()
}
