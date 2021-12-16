build_heatmap_tbl <- function(tbl, func){
  tbl %>%
    dplyr::select(
      "feature" = "feature_display", "feature_order", "group_name", "feature_value", "response_value"
    ) %>%
    dplyr::group_by(.data$group_name, .data$feature, .data$feature_order) %>%
    dplyr::summarise("value" = func(
      .data$feature_value,
      .data$response_value
    )) %>%
    dplyr::arrange(dplyr::desc(.data$feature_order)) %>%
    dplyr::select(-c("feature_order")) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_wider(
      .,
      names_from = "group_name",
      values_from = "value"
    )
}
