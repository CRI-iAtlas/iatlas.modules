
summarized_barplot_data_names <- c(
  "group_display",
  "feature_display",
  "text",
  "MEAN",
  "SE"
)

barplot_eventdata_names <- c("curveNumber", "pointNumber", "x", "y", "key")

test_result_object <- function(res){
  scatterplot_data <- res$scatterplot_data
  expect_type(scatterplot_data, "list")
  expect_named(scatterplot_data, c("x", "y", "text"))
  barplot_data <- res$barplot_data
  expect_type(barplot_data, "list")
  expect_named(
    barplot_data,
    c('group_display', 'feature_display', 'text', 'MEAN', 'SE')
  )
}

test_that("barplot_server2_iris_no_group_data", {

  shiny::testServer(
    barplot_server2,
    args = list(
      "barplot_data" = shiny::reactive(
        dplyr::rename(
          example_iris_data(),
          "feature_display" = "feature_name"
        )
      ),
      "drilldown" = shiny::reactive(T)
    ),
    {
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = 1,
        "pointNumber" = 2,
        "x" = "virginica",
        "y" = 6.588,
        "key" = "virginica"
      ))

      expect_true(tibble::is_tibble(validated_barplot_data()))
      expect_true(tibble::is_tibble(validated_group_data()))

      expect_true(tibble::is_tibble(merged_barplot_data()))
      expect_true(nrow(merged_barplot_data()) > 0)
      expect_named(
        merged_barplot_data(),
        c(
          "sample_name",
          "group_display",
          "feature_display",
          "feature_value"
        )
      )

      expect_type(summarized_barplot_data(), "list")
      expect_named(summarized_barplot_data(), summarized_barplot_data_names)
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")

      expect_type(barplot_eventdata(), "list")
      expect_named(barplot_eventdata(), barplot_eventdata_names)
      expect_equal(selected_group(), "virginica")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c(
          'sample_name',
          'group_display',
          'Sepal.Length',
          'Sepal.Width',
          'Petal.Length',
          'Petal.Width'
        )
      )

      test_result_object(session$getReturned()())
    }
  )
})


test_that("barplot_server2_iris", {

  shiny::testServer(
    barplot_server2,
    args = list(
      "barplot_data" = shiny::reactive(
        dplyr::rename(
          example_iris_data(),
          "feature_display" = "feature_name"
        )
      ),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "drilldown" = shiny::reactive(T)
    ),
    {
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = 1,
        "pointNumber" = 2,
        "x" = "Virginica",
        "y" = 6.588,
        "key" = "Virginica"
      ))

      expect_true(tibble::is_tibble(validated_barplot_data()))

      expect_true(tibble::is_tibble(merged_barplot_data()))
      expect_true(nrow(merged_barplot_data()) > 0)
      expect_named(
        merged_barplot_data(),
        c(
          "sample_name",
          "group_display",
          "feature_display",
          "feature_value"
        )
      )

      expect_type(summarized_barplot_data(), "list")
      expect_named(summarized_barplot_data(), summarized_barplot_data_names)
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")

      expect_type(barplot_eventdata(), "list")
      expect_named(barplot_eventdata(), barplot_eventdata_names)
      expect_equal(selected_group(), "Virginica")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c(
          'sample_name',
          'group_display',
          'Sepal.Length',
          'Sepal.Width',
          'Petal.Length',
          'Petal.Width'
        )
      )

      test_result_object(session$getReturned()())
    }
  )
})


