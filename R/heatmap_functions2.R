validate_heatmap_data <- function(heatmap_data){
  validate_feature_data(
    heatmap_data,
    required_columns = c(
      "sample_name",
      "group_name",
      "dataset_name",
      "feature_value",
      "feature_display",
      "response_display",
      "response_value"
    ),
    table_name = "heatmap_data",
    table_key = NULL,
    optional_columns = c("feature_order")
  )
}

combine_heatmap_data <- function(validated_heatmap_data, validated_group_data){
  validated_heatmap_data %>%
    dplyr::inner_join(validated_group_data, by = "group_name") %>%
    dplyr::select(
      "sample_name",
      "feature_display",
      "feature_order",
      "group_display",
      "feature_value",
      "response_display",
      "response_value"
    )
}

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

create_scatterplot_data <- function(
  combined_heatmap_data, selected_feature, response_feature, selected_group
){

  shiny::validate(shiny::need(
    all(
      selected_group %in% combined_heatmap_data$group_display,
      selected_feature %in% combined_heatmap_data$feature_display
    ),
    "Plot has been updated, please click on plot."
  ))

  shiny::validate(shiny::need(
    selected_feature != response_feature,
    "Selected features to compare are the same, please select new features."
  ))

  combined_heatmap_data %>%
    dplyr::filter(
      .data$feature_display == selected_feature,
      .data$group_display == selected_group
    ) %>%
    dplyr::select(
      "sample_name", "group_display", "feature_value", "response_value"
    ) %>%
    dplyr::rename(
      !!selected_feature := .data$feature_value,
      !!response_feature := .data$response_value
    )
}
