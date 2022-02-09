summarize_heatmap_data <- function(tbl, func){
  tbl %>%
    dplyr::select(
      "feature_display",
      "feature_order",
      "group_display",
      "feature_value",
      "response_value"
    ) %>%
    dplyr::group_by(
      .data$group_display, .data$feature_display, .data$feature_order
    ) %>%
    dplyr::summarise("value" = func(
      .data$feature_value,
      .data$response_value
    )) %>%
    dplyr::arrange(dplyr::desc(.data$feature_order)) %>%
    dplyr::select(-c("feature_order")) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(
      .,
      names_from = "group_display",
      values_from = "value"
    )
}

create_heatmap_group_data <- function(heatmap_data){
  data <- heatmap_data %>%
    dplyr::select("group_name") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "group_display" = .data$group_name,
      "group_color" = NA_character_,
      "group_description" = ""
    )
}
