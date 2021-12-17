test_that("plotly_bar_one_group", {
  p <- example_starwars_data() %>%
    summarise_barplot_se(title = "Sample") %>%
    plotly_bar(
      source_name = "test",
      x_col = "group_name",
      y_col = "MEAN",
      color_col = "feature_display",
      error_col = "SE",
      text_col = "text"
    )
  expect_type(p, "list")
  print(p)
})

test_that("plotly_bar_two_groups", {
  p <- example_iris_data() %>%
    summarise_barplot_se(title = "Sample") %>%
    plotly_bar(
      source_name = "test",
      x_col = "group_name",
      y_col = "MEAN",
      color_col = "feature_display",
      text_col = "text"
    )
  expect_type(p, "list")
  print(p)
})
