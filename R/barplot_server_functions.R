build_barplot_data <- function(plot_data_function, feature_class_choice){
  data <-
    plot_data_function(.feature_class = feature_class_choice) %>%
    dplyr::select(dplyr::any_of(
      c(
        "sample_name",
        "feature_name",
        "feature_display",
        "feature_value",
        "group_name",
        "group_description"
      )
    ))
}

summarise_barplot_se <- function(data, title){
  data %>%
    dplyr::select("group_name", "feature_display", "feature_value") %>%
    tidyr::drop_na() %>%
    dplyr::group_by_at(dplyr::vars("group_name", "feature_display")) %>%
    dplyr::summarise(
      "MEAN" = mean(.data$feature_value),
      "SE" = .data$MEAN / sqrt(dplyr::n()),
      .groups = "drop"
    ) %>%
    create_plotly_text(
      .data$feature_display,
      .data$group_name,
      c("MEAN", "SE"),
      title
    )
}

get_barplot_group_data <- function(barplot_data){
  barplot_data %>%
    dplyr::select("group_name", "group_description") %>%
    dplyr::distinct()
}
