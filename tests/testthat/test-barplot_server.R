
test_that("barplot_server_iris", {

  shiny::testServer(
    barplot_server,
    args = list(
      "plot_data_func" = shiny::reactive(example_iris_data_func),
      "feature_classes" = shiny::reactive(c("Length", "Width")),
      "drilldown" = shiny::reactive(T)
    ),
    {
      session$setInputs("feature_class_choice" = "Length")
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = 1,
        "pointNumber" = 2,
        "x" = "virginica",
        "y" = 6.588,
        "key" = "virginica"
      ))

      expect_true(display_feature_class_selection_ui())
      expect_type(output$feature_class_selection_ui, "list")

      expect_type(barplot_data(), "list")
      expect_named(
        barplot_data(),
        c(
          "sample_name",
          "feature_name",
          "feature_value",
          "group_name",
          "group_description"
        )
      )
      expect_type(summarized_barplot_data(), "list")
      expect_named(
        summarized_barplot_data(),
        c("group_name", "feature_name", "text", "MEAN", "SE")
      )
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")
      expect_type(group_data(), "list")
      expect_named(group_data(), c("group_name", "group_description"))

      expect_type(barplot_eventdata(), "list")
      expect_named(barplot_eventdata(), c("curveNumber", "pointNumber", "x", "y", "key"))
      expect_equal(selected_group(), "virginica")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c("sample_name", "group_name", "Sepal.Length", "Petal.Length")
      )

      res <- session$getReturned()
      scatterplot_data <- res$scatterplot_data()
      expect_type(scatterplot_data, "list")
      expect_named(scatterplot_data, c("x", "y", "text"))
      barplot_data <- res$barplot_data()
      expect_type(barplot_data, "list")
      expect_named(
        barplot_data,
        c('group_name', 'feature_name', 'text', 'MEAN', 'SE')
      )
    }
  )
})

test_that("barplot_server_iris2", {

  shiny::testServer(
    barplot_server,
    args = list(
      "plot_data_func" = shiny::reactive(example_iris_data_func),
      "drilldown" = shiny::reactive(T)
    ),
    {
      expect_false(display_feature_class_selection_ui())
      expect_type(barplot_data(), "list")
      expect_named(
        barplot_data(),
        c(
          "sample_name",
          "feature_name",
          "feature_value",
          "group_name",
          "group_description"
        )
      )
      expect_type(summarized_barplot_data(), "list")
      expect_named(
        summarized_barplot_data(),
        c("group_name", "feature_name", "text", "MEAN", "SE")
      )
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")
      expect_error(
        barplot_eventdata(),
        regexp = "Click on above barplot.",
        class = c("shiny.silent.error")
      )
      expect_type(group_data(), "list")
      expect_named(group_data(), c("group_name", "group_description"))
    }
  )
})

test_that("barplot_server_starwars", {

  shiny::testServer(
    barplot_server,
    args = list(
      "plot_data" = shiny::reactive(example_starwars_data_func)
    ),
    {
      expect_false(display_feature_class_selection_ui())
      expect_type(barplot_data(), "list")
      expect_named(
        barplot_data(),
        c(
          "sample_name",
          "feature_name",
          "feature_value",
          "group_name"
        )
      )
      expect_type(summarized_barplot_data(), "list")
      expect_named(
        summarized_barplot_data(),
        c("group_name", "feature_name", "text", "MEAN", "SE")
      )
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")
      expect_error(
        barplot_eventdata(),
        regexp = "Click on above barplot.",
        class = c("shiny.silent.error")
      )
    }
  )
})
