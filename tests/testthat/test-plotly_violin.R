test_that("violin_plot", {
  p <- example_iris_data() %>%
    tidyr::drop_na() %>%
    plotly_violin(
      x_col = "group_name",
      y_col = "feature_value"
    )
  expect_type(p, "list")
  print(p)
})


