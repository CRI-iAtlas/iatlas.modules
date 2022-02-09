test_result_object <- function(res){
  scatterplot_data <- res$scatterplot_data
  expect_true(tibble::is_tibble(scatterplot_data))
  expect_named(scatterplot_data, c("x", "y", "text"))
  heatmap_data <- res$heatmap_data
  expect_true(tibble::is_tibble(heatmap_data))
}


test_that("heatmap_server", {

  shiny::testServer(
    heatmap_server,
    args = list(
      "feature_sample_data_function" = shiny::reactive(example_iris_data),
      "response_sample_data_function" = shiny::reactive(example_iris_data),
      "feature_data" = shiny::reactive(example_iris_data_features_1_class()),
      "response_data" = shiny::reactive(example_iris_data_features_1_class()),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "summarise_function_list" = shiny::reactive(
        purrr::partial(stats::cor, method = "pearson")
      ),
      "drilldown" = shiny::reactive(T)
    ),
    {
      session$setInputs("class_choice" = "Length")
      session$setInputs("response_choice" = "Sepal.Width")
      session$setInputs("heatmap-mock_event_data" = data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "Setosa",
        "y" = "Sepal Length",
        "z" = "0.1805093"
      ))

      expect_true(tibble::is_tibble(validated_feature_data()))
      expect_named(
        validated_feature_data(),
        c("feature_name", "feature_display", "feature_class", "feature_order")
      )
      expect_true(tibble::is_tibble(validated_response_data()))
      expect_named(
        validated_response_data(),
        c("feature_name", "feature_display", "feature_class")
      )

      expect_null(default_class())
      expect_equal(default_class2(), "Length")


      expect_null(default_response())
      expect_equal(default_response2(), "Sepal.Length")

      expect_type(response_choices(), "list")
      expect_named(response_choices(), c("Length", "Width"))

      expect_type(output$class_selection_ui, "list")
      expect_type(output$response_selection_ui, "list")
      expect_false(display_summarise_function_ui())
      expect_true(tibble::is_tibble(feature_sample_data()))
      expect_named(
        feature_sample_data(),
        c(
          "sample_name",
          "feature_name",
          "group_name",
          "feature_value"
        )
      )
      expect_true(tibble::is_tibble(response_sample_data()))
      expect_named(
        response_sample_data(),
        c(
          "sample_name",
          "feature_name",
          "feature_value"
        )
      )
      expect_true(tibble::is_tibble(heatmap_data()))
      expect_named(
        heatmap_data(),
        c(
          'sample_name',
          'feature_value',
          'feature_display',
          'feature_order',
          'group_name',
          'response_value',
          'response_display'
        )
      )

      expect_type(summarise_function, "closure")

      test_result_object(session$getReturned()())
    }
  )
})

test_that("heatmap_server_multiple_summarise_functions", {

  shiny::testServer(
    heatmap_server,
    args = list(
      "feature_sample_data_function" = shiny::reactive(example_iris_data),
      "response_sample_data_function" = shiny::reactive(example_iris_data),
      "feature_data" = shiny::reactive(example_iris_data_features_1_class()),
      "response_data" = shiny::reactive(example_iris_data_features_1_class()),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "summarise_function_list" = shiny::reactive(
        list(
          "Pearson" = purrr::partial(stats::cor, method = "pearson"),
          "Spearman" = purrr::partial(stats::cor, method = "spearman")
        )
      ),
      "drilldown" = shiny::reactive(T)
    ),
    {
      session$setInputs("summarise_function_choice" = "Spearman")

      expect_true(display_summarise_function_ui())
      expect_type(output$summarise_function_ui, "list")

      expect_type(summarise_function(), "closure")
    }
  )
})


test_that("heatmap_server_error_no_feature_data", {

  shiny::testServer(
    heatmap_server,
    args = list(
      "feature_sample_data_function" = shiny::reactive(example_iris_data),
      "response_sample_data_function" = shiny::reactive(example_iris_data),
      "feature_data" = shiny::reactive(example_iris_data_features_1_class()),
      "response_data" = shiny::reactive(example_iris_data_features_1_class()),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "summarise_function_list" = shiny::reactive(
        purrr::partial(stats::cor, method = "pearson")
      )
    ),
    {
      session$setInputs("class_choice" = "Adaptive Receptor - T cell")
      expect_error(
        feature_sample_data(),
        regexp = "Feature class choice did not produce any data, please select a different one."
      )
    }
  )
})

test_that("heatmap_server_error_no_response_data", {

  shiny::testServer(
    heatmap_server,
    args = list(
      "feature_sample_data_function" = shiny::reactive(example_iris_data),
      "response_sample_data_function" = shiny::reactive(example_iris_data),
      "feature_data" = shiny::reactive(example_iris_data_features_1_class()),
      "response_data" = shiny::reactive(example_iris_data_features_1_class()),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "summarise_function_list" = shiny::reactive(
        purrr::partial(stats::cor, method = "pearson")
      )
    ),
    {
      session$setInputs("response_choice" = "age_at_diagnosis")
      expect_error(
        response_sample_data(),
        regexp = "Response feature choice did not produce any data, please select a different one."
      )
    }
  )
})


