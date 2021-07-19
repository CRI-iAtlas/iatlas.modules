
test_that("format_scatterplot_data", {

  result <- format_scatterplot_data(
    get_pcawg_scatterplot_example(),
    "TCR Evenness",
    "TCR Richness"
  )
  expect_type(result, "list")
  expect_named(result, c("x", "y", "text"))
})
