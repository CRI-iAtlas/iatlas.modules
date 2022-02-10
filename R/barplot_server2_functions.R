create_barplot_group_data <- function(validated_barplot_data){
  data <- validated_barplot_data %>%
    dplyr::select("group_name") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "group_display" = .data$group_name,
      "group_description" = ""
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
