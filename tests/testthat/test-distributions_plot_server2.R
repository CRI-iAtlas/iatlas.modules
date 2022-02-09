merged_distplot_names <- c(
  "sample_name",
  "group_display",
  "group_color",
  "group_description",
  "feature_display",
  "feature_value"
)

test_result_object <- function(res){
  expect_named(
    res,
    c("histogram_data", "distplot_data", "group_text")
  )

  expect_type(res$group_text, "character")

  histogram_data <- res$histogram_data
  expect_true(tibble::is_tibble(histogram_data))
  expect_named(histogram_data, "feature_value")
  distplot_data <- res$distplot_data
  expect_type(distplot_data, "list")
  expect_named(
    distplot_data,
    c(
      'sample_name',
      'group_display',
      'group_color',
      'group_description',
      'feature_display',
      'feature_value'
    )
  )
}


test_that("distributions_plot_server2_no_group_data", {

  shiny::testServer(
    distributions_plot_server2,
    args = list(
      "distplot_data" = shiny::reactive(
        dplyr::rename(
          example_iris_data(),
          "feature_display" = "feature_name"
        )
      ),
      "plot_type" = shiny::reactive("Violin"),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species")
    ),
    {
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = c(0,0),
        "pointNumber" = c(0,0),
        "x" = "setosa",
        "y" = c(5.1, 2.1),
        "key" = "setosa"
      ))

      expect_true(tibble::is_tibble(validated_group_data()))
      expect_true(tibble::is_tibble(validated_distplot_data()))

      expect_true(tibble::is_tibble(merged_distplot_data()))
      expect_true(nrow(merged_distplot_data()) > 0)
      expect_named( merged_distplot_data(), merged_distplot_names)

      expect_equal(distplot_source_name(), "proxy1-distplot")
      expect_equal(plotly_function(), plotly_violin)
      expect_null(plot_fill_colors())
      expect_equal(plot_title(), "")

      expect_type(output$distplot, "character")
      test_result_object(session$getReturned()())
    }
  )
})


test_that("distributions_plot_server2_with_group_data", {

  shiny::testServer(
    distributions_plot_server2,
    args = list(
      "distplot_data" = shiny::reactive(
        dplyr::rename(
          example_iris_data(),
          "feature_display" = "feature_name"
        )
      ),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species")
    ),
    {
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = c(0,0),
        "pointNumber" = c(0,0),
        "x" = "Setosa",
        "y" = c(5.1, 2.1),
        "key" = "Setosa"
      ))

      expect_true(tibble::is_tibble(validated_group_data()))
      expect_true(tibble::is_tibble(validated_distplot_data()))

      expect_true(tibble::is_tibble(merged_distplot_data()))
      expect_true(nrow(merged_distplot_data()) > 0)
      expect_named( merged_distplot_data(), merged_distplot_names)

      expect_equal(distplot_source_name(), "proxy1-distplot")
      expect_equal(plotly_function(), plotly_violin)
      expect_type(plot_fill_colors(), "character")
      expect_equal(plot_title(), "")

      expect_type(output$distplot, "character")
      test_result_object(session$getReturned()())
    }
  )
})
