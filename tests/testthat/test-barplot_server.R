
test_that("barplot_server_iris", {

  shiny::testServer(
    barplot_server,
    args = list(
      "plot_data_func" = shiny::reactive(example_iris_data_func),
      "feature_classes" = shiny::reactive(c("Length", "Width")),
      "drilldown" = shiny::reactive(T)
    ),
    {
      expect_true(display_feature_class_selection_ui())
      expect_type(output$feature_class_selection_ui, "list")
      session$setInputs("feature_class_choice" = "Length")
      expect_type(barplot_data(), "list")
      expect_named(
        barplot_data(),
        c("sample", "feature", "feature_value", "group", "group_description")
      )
      expect_type(summarized_barplot_data(), "list")
      expect_named(
        summarized_barplot_data(),
        c("group", "feature", "text", "MEAN", "SE")
      )
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")
      expect_type(group_data(), "list")
      expect_named(group_data(), c("group", "description"))

      expect_error(
        barplot_eventdata(),
        regexp = "Click on above barplot.",
        class = c("shiny.silent.error")
      )
      session$setInputs("test_event_data" = data.frame("key" = "setosa"))
      expect_type(barplot_eventdata(), "list")
      expect_named(barplot_eventdata(), "key")
      expect_equal(selected_group(), "setosa")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c("sample", "group", "Sepal.Length", "Petal.Length")
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
        c("sample", "feature", "feature_value", "group", "group_description")
      )
      expect_type(summarized_barplot_data(), "list")
      expect_named(
        summarized_barplot_data(),
        c("group", "feature", "text", "MEAN", "SE")
      )
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")
      expect_error(
        barplot_eventdata(),
        regexp = "Click on above barplot.",
        class = c("shiny.silent.error")
      )
      expect_type(group_data(), "list")
      expect_named(group_data(), c("group", "description"))
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
        c("sample", "feature", "feature_value", "group")
      )
      expect_type(summarized_barplot_data(), "list")
      expect_named(
        summarized_barplot_data(),
        c("group", "feature", "text", "MEAN", "SE")
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
