test_result_object <- function(res){
  scatterplot_data <- res$scatterplot_data
  expect_true(tibble::is_tibble(scatterplot_data))
  expect_named(scatterplot_data, c("x", "y", "text"))
  heatmap_data <- res$heatmap_data
  expect_true(tibble::is_tibble(heatmap_data))
}

test_that("heatmap_server2_bad_x_column", {

  shiny::testServer(
    heatmap_server2,
    args = list(
      "heatmap_data" = shiny::reactive(example_heatmap_iris_data()),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "drilldown" = shiny::reactive(T),
      "mock_event_data" = shiny::reactive(data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "X",
        "y" = "Sepal Length",
        "z" = "0.1805093"
      ))
    ),
    {
      expect_error(
        scatterplot_data(),
        regexp = "mock_event_data column x value: X not in merged_heatmap_data column group_display"
      )
    }
  )
})

test_that("heatmap_server2_bad_y_column", {

  shiny::testServer(
    heatmap_server2,
    args = list(
      "heatmap_data" = shiny::reactive(example_heatmap_iris_data()),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "drilldown" = shiny::reactive(T),
      "mock_event_data" = shiny::reactive(data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "Setosa",
        "y" = "Y",
        "z" = "0.1805093"
      ))
    ),
    {
      expect_error(
        scatterplot_data(),
        regexp = "mock_event_data column y value: Y not in merged_heatmap_data column feature_display"
      )
    }
  )
})


test_that("heatmap_server2", {

  shiny::testServer(
    heatmap_server2,
    args = list(
      "heatmap_data" = shiny::reactive(example_heatmap_iris_data()),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "drilldown" = shiny::reactive(T),
      "mock_event_data" = shiny::reactive(data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "Setosa",
        "y" = "Sepal Length",
        "z" = "0.1805093"
      ))
    ),
    {
      expect_true(tibble::is_tibble(validated_group_data()))
      expect_named(
        validated_group_data(),
        c("group_name", "group_display", "group_color", "group_description")
      )

      expect_true(tibble::is_tibble(validated_heatmap_data()))

      expect_true(tibble::is_tibble(merged_heatmap_data()))

      expect_true(tibble::is_tibble(summarized_heatmap_data()))
      expect_named(
        summarized_heatmap_data(),
        c('feature_display', 'Setosa', 'Versicolor', 'Virginica')
      )

      expect_type(heatmap_matrix(), "double")

      expect_equal(heatmap_source_name(), "proxy1-heatmap")
      expect_type(heatmap_plot(), "list")
      expect_type(output$heatmap, "character")

      expect_type(heatmap_eventdata(), "list")
      expect_named(
        heatmap_eventdata(),
        c("curveNumber", "pointNumber", "x", "y", "z")
      )

      expect_equal(group_text(), "Iris Species: Setosa")

      expect_equal(selected_feature(), "Sepal Length")
      expect_equal(selected_group(), "Setosa")
      expect_equal(response_feature(), "Sepal Width")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c('sample_name', 'group_display', 'Sepal Length', 'Sepal Width')
      )
      expect_true(nrow(scatterplot_data()) > 0)

      test_result_object(session$getReturned()())
    }
  )
})

test_that("heatmap_server2_no_group_data", {

  shiny::testServer(
    heatmap_server2,
    args = list(
      "heatmap_data" = shiny::reactive(example_heatmap_iris_data()),
      "drilldown" = shiny::reactive(T),
      "mock_event_data" = shiny::reactive(data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "setosa",
        "y" = "Sepal Length",
        "z" = "0.1805093"
      ))
    ),
    {
      expect_true(tibble::is_tibble(validated_group_data()))
      expect_named(
        validated_group_data(),
        c("group_name", "group_display", "group_color", "group_description")
      )

      expect_true(tibble::is_tibble(validated_heatmap_data()))

      expect_true(tibble::is_tibble(merged_heatmap_data()))

      expect_true(tibble::is_tibble(summarized_heatmap_data()))
      expect_named(
        summarized_heatmap_data(),
        c('feature_display', 'setosa', 'versicolor', 'virginica')
      )

      expect_type(heatmap_matrix(), "double")

      expect_equal(heatmap_source_name(), "proxy1-heatmap")
      expect_type(heatmap_plot(), "list")
      expect_type(output$heatmap, "character")

      expect_type(heatmap_eventdata(), "list")
      expect_named(
        heatmap_eventdata(),
        c("curveNumber", "pointNumber", "x", "y", "z")
      )
      expect_equal(selected_feature(), "Sepal Length")
      expect_equal(selected_group(), "setosa")
      expect_equal(response_feature(), "Sepal Width")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c('sample_name', 'group_display', 'Sepal Length', 'Sepal Width')
      )
      expect_true(nrow(scatterplot_data()) > 0)

      test_result_object(session$getReturned()())
    }
  )
})

test_that("heatmap_server2_chosen_features_are_equal", {

  shiny::testServer(
    heatmap_server2,
    args = list(
      "heatmap_data" = shiny::reactive(example_heatmap_iris_data()),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "drilldown" = shiny::reactive(T),
      "mock_event_data" = shiny::reactive(data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "Setosa",
        "y" = "Sepal Width",
        "z" = "0.1805093"
      ))
    ),
    {

      expect_error(
        scatterplot_data(),
        regexp = "Selected features to compare are the same, please select new features."
      )
    }
  )
})

