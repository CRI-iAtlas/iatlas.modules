sample_data_names <- c(
  "sample_name",
  "feature_name",
  "group_name",
  "feature_value"
)

barplot_data_names <- c(
  "sample_name",
  "group_name",
  "feature_display",
  "feature_value"
)

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


test_that("barplot_server_iris_no_feature_or_group_data", {

  shiny::testServer(
    barplot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data),
      "drilldown" = shiny::reactive(T),
      "mock_event_data" = shiny::reactive(data.frame(
        "curveNumber" = 1,
        "pointNumber" = 2,
        "x" = "setosa",
        "y" = 6.588,
        "key" = "setosa"
      ))
    ),
    {
      session$setInputs("feature_class_choice" = "Length")

      expect_null(feature_data())
      expect_null(validated_feature_data())
      expect_null(group_data())

      expect_false(display_feature_class_selection_ui())

      expect_named(validated_sample_data(), sample_data_names)
      expect_type(barplot_data(), "list")
      expect_named(barplot_data(), barplot_data_names)

      test_result_object(session$getReturned()())
    }
  )
})


test_that("barplot_server_iris_feature_data", {

  shiny::testServer(
    barplot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data),
      "feature_data" = shiny::reactive(example_iris_data_features_1_class()),
      "drilldown" = shiny::reactive(T),
      "mock_event_data" = shiny::reactive(data.frame(
        "curveNumber" = 1,
        "pointNumber" = 2,
        "x" = "setosa",
        "y" = 6.588,
        "key" = "setosa"
      ))
    ),
    {
      session$setInputs("feature_class_choice" = "Length")

      expect_true(tibble::is_tibble(feature_data()))
      expect_true(tibble::is_tibble(validated_feature_data()))
      expect_named(
        validated_feature_data(),
        c('feature_name', 'feature_display', 'feature_class')
      )

      expect_true(display_feature_class_selection_ui())
      expect_type(output$feature_class_selection_ui, "list")
      expect_named(validated_sample_data(), sample_data_names)

      expect_type(barplot_data(), "list")
      expect_named(barplot_data(), barplot_data_names)

      test_result_object(session$getReturned()())
    }
  )
})

test_that("barplot_server_iris_group_data", {

  shiny::testServer(
    barplot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "drilldown" = shiny::reactive(T),
      "mock_event_data" = shiny::reactive(data.frame(
        "curveNumber" = 1,
        "pointNumber" = 2,
        "x" = "Virginica",
        "y" = 6.588,
        "key" = "Virginica"
      ))
    ),
    {
      session$setInputs("feature_class_choice" = "Length")

      expect_null(feature_data())
      expect_null(validated_feature_data())
      expect_type(group_data(), "list")

      expect_false(display_feature_class_selection_ui())

      expect_named(validated_sample_data(), sample_data_names)
      expect_type(barplot_data(), "list")
      expect_named(barplot_data(), barplot_data_names)

      test_result_object(session$getReturned()())
    }
  )
})
