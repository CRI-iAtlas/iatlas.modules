
test_that("format_scatterplot_data", {

  result <- format_scatterplot_data(
    example_scatterplot_iris_data_4_features(),
    "Sepal.Length",
    "Sepal.Width"
  )
  expect_type(result, "list")
  expect_named(result, c("x", "y", "text"))
})
