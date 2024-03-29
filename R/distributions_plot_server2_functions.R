validate_distplot_data <- function(distplot_data){
  validate_data(
    distplot_data,
    required_columns = c(
      "sample_name",
      "group_name",
      "dataset_name",
      "feature_display",
      "feature_value"
    ),
    table_name = "distplot_data"
  )
}

create_distplot_group_data <- function(validated_distplot_data){
  data <- validated_distplot_data %>%
    dplyr::select("group_name") %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      "group_display" = .data$group_name,
      "group_color" = NA_character_,
      "group_description" = ""
    )
}

create_distplot_dataset_data <- function(validated_distplot_data){
  data <- validated_distplot_data %>%
    dplyr::select("dataset_name") %>%
    dplyr::distinct() %>%
    dplyr::mutate("dataset_display" = .data$dataset_name)
}

merge_distplot_data <- function(
  validated_distplot_data,
  validated_group_data,
  validated_dataset_data
){
  validated_distplot_data %>%
    dplyr::inner_join(validated_group_data, by = "group_name") %>%
    dplyr::inner_join(validated_dataset_data, by = "dataset_name") %>%
    dplyr::select(
      "sample_name",
      "group_display",
      "group_color",
      "group_description",
      "feature_display",
      "feature_value",
      "dataset_display"
    )
}

format_distplot_data2 <- function(
  merged_distplot_data, reorder_method, scale_method
){
  merged_distplot_data %>%
    dplyr::select("group_display", "dataset_display", "feature_value") %>%
    refactor_by_tbl_value_column(reorder_method) %>%
    scale_tbl_value_column(scale_method) %>%
    dplyr::group_by(.data$dataset_display)
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

create_displots <- function(
  formatted_distplot_data,
  distplot_source_name,
  plotly_function,
  feature,
  plot_fill_colors,
  distplot_xlab
){
  tables <- dplyr::group_split(formatted_distplot_data)
  titles <- dplyr::group_keys(formatted_distplot_data)$dataset_display

  tables %>%
    purrr::map(
      .x = .,
      .f = ~plotly_function(
        plot_data = .x,
        source_name = distplot_source_name,
        x_col = "group_display",
        y_col = "feature_value",
        key_col = "dataset_display",
        fill_colors = plot_fill_colors,
        xlab = distplot_xlab,
        ylab = feature
      )
    ) %>%
    purrr::map2(
      .x = .,
      .y = titles,
      .f = ~plotly::add_annotations(
        p = .x,
        text = .y,
        x = 0.5,
        y = 1,
        yref = "paper",
        xref = "paper",
        xanchor = "center",
        yanchor = "top",
        yshift = 30,
        showarrow = FALSE,
        font = list(family = "Roboto, Open Sans, sans-serif", size = 15))
    )
}

