
test_that("drilldown_scatterplot_server_wide_data_2_features", {

  shiny::testServer(
    drilldown_scatterplot_server,
    args = list(
      "scatterplot_data" = shiny::reactive(example_scatterplot_iris_data_2_features()),
      "selected_group" = shiny::reactive("setosa")
    ),
    {
      expect_type(scatterplot_data(), "list")
      expect_equal(
        scatterplot_feature_columns(),
        c("Sepal.Length", "Sepal.Width")
      )
      expect_false(display_feature_selection_ui())
      expect_error(output$x_feature_selection_ui)
      expect_error(output$y_feature_selection_ui)
      expect_equal(x_feature(), "Sepal.Length")
      expect_equal(y_feature(), "Sepal.Width")

      expect_type(formatted_scatterplot_data(), "list")
      expect_named(formatted_scatterplot_data(), c("x", "y", "text"))

      res <- session$getReturned()()
      expect_type(res, "list")
      expect_named(res, c("x", "y", "text"))
    }
  )
})

test_that("drilldown_scatterplot_server_wide_data_3_features", {

  shiny::testServer(
    drilldown_scatterplot_server,
    args = list(
      "scatterplot_data" = shiny::reactive(example_scatterplot_iris_data_3_features()),
      "selected_group" = shiny::reactive("setosa")
    ),
    {
      session$setInputs("x_feature_choice" = "Sepal.Length")

      expect_type(scatterplot_data(), "list")
      expect_equal(
        scatterplot_feature_columns(),
        c("Sepal.Length", "Sepal.Width", "Petal.Length")
      )
      expect_true(display_feature_selection_ui())
      expect_type(output$x_feature_selection_ui, "list")
      expect_type(output$y_feature_selection_ui, "list")
      expect_equal(x_feature(), "Sepal.Length")
      expect_equal(y_feature(), "Sepal.Width")

      expect_type(formatted_scatterplot_data(), "list")
      expect_named(formatted_scatterplot_data(), c("x", "y", "text"))

      res <- session$getReturned()()
      expect_type(res, "list")
      expect_named(res, c("x", "y", "text"))
    }
  )
})

test_that("drilldown_scatterplot_server_group_set_features", {

  shiny::testServer(
    drilldown_scatterplot_server,
    args = list(
      "scatterplot_data" = shiny::reactive(example_scatterplot_iris_data_4_features()),
      "selected_group" = shiny::reactive("setosa"),
      "x_feature_input"  = shiny::reactive("Petal.Length"),
      "y_feature_input"  = shiny::reactive("Petal.Width")
    ),
    {
      expect_type(scatterplot_data(), "list")
      expect_equal(
        scatterplot_feature_columns(),
        c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
      )
      expect_false(display_feature_selection_ui())
      expect_error(output$x_feature_selection_ui)
      expect_error(output$y_feature_selection_ui)
      expect_equal(x_feature(), "Petal.Length")
      expect_equal(y_feature(), "Petal.Width")

      expect_type(formatted_scatterplot_data(), "list")
      expect_named(formatted_scatterplot_data(), c("x", "y", "text"))

      res <- session$getReturned()()
      expect_type(res, "list")
      expect_named(res, c("x", "y", "text"))
    }
  )
})
