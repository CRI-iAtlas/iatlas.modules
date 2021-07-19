
get_scatterplot_x_feature <- function(feature_choice, feature_columns){
  if(is.null(feature_choice)) return(feature_columns[[1]])
  else return(feature_choice)
}

get_scatterplot_y_feature <- function(feature_choice, feature_columns){
  if(is.null(feature_choice)) return(feature_columns[[2]])
  else return(feature_choice)
}

format_scatterplot_data <- function(
  plot_data,
  x_feature,
  y_feature
){
  plot_data %>%
    dplyr::select(dplyr::all_of(c(
      "sample", "group", "x" =  x_feature, "y" = y_feature
    ))) %>%
    tidyr::drop_na() %>%
    create_plotly_text(
      .data$sample, .data$group, c("x", "y"), "Sample"
    ) %>%
    dplyr::select("x", "y", "text")
}
