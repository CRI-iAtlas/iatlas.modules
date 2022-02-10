
create_heatmap_data <- function(
  feature_sample_tbl,
  response_sample_tbl,
  validated_feature_data,
  validated_response_data
){

  dplyr::inner_join(

    feature_sample_tbl %>%
      dplyr::inner_join(validated_feature_data, by = "feature_name") %>%
      dplyr::select(
        "sample_name",
        "feature_value",
        "feature_display",
        "feature_order",
        "group_name",
        "dataset_name"
      ),

    response_sample_tbl %>%
      dplyr::inner_join(validated_response_data, by = "feature_name") %>%
      dplyr::select(
        "sample_name",
        "response_value" = "feature_value",
        "response_display" = "feature_display"
      ),

    by = "sample_name"
  )

}
