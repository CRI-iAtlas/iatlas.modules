test_that("plotly_bar", {
  p <- example_iris_data() %>%
    dplyr::mutate(
      "group_display" = .data$group_name,
      "feature_display" = .data$feature_name
    ) %>%
    summarise_barplot_se(title = "Sample") %>%
    plotly_bar(
      source_name = "test",
      x_col = "group_display",
      y_col = "MEAN",
      color_col = "feature_display",
      error_col = "SE",
      text_col = "text"
    )
  expect_type(p, "list")
  print(p)
})

