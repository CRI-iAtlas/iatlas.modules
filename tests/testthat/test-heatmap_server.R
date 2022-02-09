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
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "Setosa",
        "y" = "Sepal Length",
        "z" = "0.1805093"
      ))

      expect_true(tibble::is_tibble(valid_feature_data()))
      expect_named(
        valid_feature_data(),
        c("feature_name", "feature_display", "feature_class", "feature_order")
      )
      expect_true(tibble::is_tibble(valid_response_data()))
      expect_named(
        valid_response_data(),
        c("feature_name", "feature_display", "feature_class")
      )
      expect_true(tibble::is_tibble(valid_group_data()))
      expect_named(
        valid_group_data(),
        c("group_name", "group_display", "group_color", "group_description")
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
      expect_true(tibble::is_tibble(feature_sample_tbl()))
      expect_named(
        feature_sample_tbl(),
        c(
          "sample_name",
          "feature_name",
          "feature_value",
          "group_name"feature_display
        )
      )
      expect_true(tibble::is_tibble(response_sample_tbl()))
      expect_named(
        response_sample_tbl(),
        c(
          "sample_name",
          "feature_name",
          "feature_value"
        )
      )
      expect_true(tibble::is_tibble(joined_tbl()))
      expect_named(
        joined_tbl(),
        c(
          'sample_name',
          'feature_value',
          'feature_display',
          'feature_order',
          'group_display',
          'group_color',
          'group_description',
          'response_value',
          'response_display'
        )
      )

      expect_type(summarise_function, "closure")
      expect_true(tibble::is_tibble(heatmap_tibble()))
      expect_named(heatmap_tibble(), c('feature', 'Setosa', 'Versicolor', 'Virginica'))
      expect_type(heatmap_matrix(), "double")
      expect_type(summarise_function(), "closure")
      expect_equal(heatmap_source_name(), "proxy1-heatmap")
      expect_type(output$heatmap, "character")

      expect_type(heatmap_eventdata(), "list")
      expect_named(
        heatmap_eventdata(),
        c("curveNumber", "pointNumber", "x", "y", "z")
      )
      expect_named(group_data(), c('group_name', 'group_display', 'group_color', 'group_description'))
      expect_equal(selected_feature(), "Sepal Length")
      expect_equal(selected_group(), "Setosa")
      expect_equal(response_feature(), "Sepal Width")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c('sample_name', 'group_display', 'Sepal Length', 'Sepal Width')
      )
      expect_true(nrow(scatterplot_data()) > 0)

      res <- session$getReturned()
      scatterplot_data <- res$scatterplot_data()
      expect_type(scatterplot_data, "list")
      expect_named(scatterplot_data, c("x", "y", "text"))
      heatmap_data <- res$heatmap_data()
      expect_type(heatmap_data, "list")
      expect_named(
        heatmap_data,
        c('feature', 'Setosa', 'Versicolor', 'Virginica')
      )
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
      session$setInputs("class_choice" = "Length")
      session$setInputs("response_choice" = "Sepal.Width")
      session$setInputs("summarise_function_choice" = "Spearman")
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "Setosa",
        "y" = "Sepal Length",
        "z" = "0.1805093"
      ))

      expect_true(display_summarise_function_ui())
      expect_type(output$summarise_function_ui, "list")

      expect_type(summarise_function(), "closure")
      expect_equal(heatmap_source_name(), "proxy1-heatmap")
      expect_type(output$heatmap, "character")

      res <- session$getReturned()
      scatterplot_data <- res$scatterplot_data()
      expect_type(scatterplot_data, "list")
      expect_named(scatterplot_data, c("x", "y", "text"))
      heatmap_data <- res$heatmap_data()
      expect_type(heatmap_data, "list")
      expect_named(
        heatmap_data,
        c('feature', 'Setosa', 'Versicolor', 'Virginica')
      )
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
        feature_sample_tbl(),
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
        response_sample_tbl(),
        regexp = "Response feature choice did not produce any data, please select a different one."
      )
    }
  )
})


test_that("heatmap_server_chosen_features_are_equal", {

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
      session$setInputs("response_choice" = "Sepal.Length")
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "Setosa",
        "y" = "Sepal Length",
        "z" = "0.1805093"
      ))
      expect_error(
        scatterplot_data(),
        regexp = "Selected features to compare are the same, please select new features."
      )
    }
  )
})

test_that("heatmap_server_plot_updated", {

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
      session$setInputs("response_choice" = "Sepal.Length")
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "X",
        "y" = "Sepal Length",
        "z" = "0.1805093"
      ))

      expect_error(
        scatterplot_data(),
        regexp = "Plot has been updated, please click on plot."
      )
    }
  )
})

