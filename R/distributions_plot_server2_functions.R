validate_distplot_data <- function(distplot_data){
  validate_data(
    distplot_data,
    required_columns = c(
      "sample_name", "group_name", "feature_display",  "feature_value"
    ),
    table_name = "distplot_data"
  )
}

create_group_data <- function(validated_distplot_data){
  data <- validated_distplot_data %>%
    dplyr::select("group_name") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "group_display" = .data$group_name,
      "group_color" = NA_character_,
      "group_description" = ""
    )
}

merge_distplot_data <- function(validated_distplot_data, validated_group_data){
  validated_distplot_data %>%
    dplyr::inner_join(validated_group_data, by = "group_name") %>%
    dplyr::select(
      "sample_name",
      "group_display",
      "group_color",
      "group_description",
      "feature_display",
      "feature_value"
    )
}

get_plot_colors <- function(validated_group_data){
  colors_provided <- !all(is.na(validated_group_data$group_color))

  if(colors_provided){
    fill_colors <- validated_group_data %>%
      dplyr::select("group_display", "group_color") %>%
      dplyr::distinct() %>%
      tibble::deframe(.)
  } else {
    fill_colors <- NULL
  }

  return(fill_colors)
}

