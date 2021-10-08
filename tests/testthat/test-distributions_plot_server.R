test_that("distributions_plot_server_no_classes", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "plot_data_func" = shiny::reactive(example_iris_data_func),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species")
    ),
    {
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")

      expect_equal(feature_classes(), character(0))
      expect_false(display_feature_class_selection_ui())
      expect_false(display_feature_selection_ui())
      expect_named(
        distplot_data(),
        c("sample", "feature", "feature_value", "group", "group_description", "color")
      )
      session$setInputs("plot_type_choice" = "Violin")
      expect_type(output$distplot, "character")
    }
  )
})

test_that("distributions_plot_server_1_class", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "plot_data_func" = shiny::reactive(example_iris_data_func),
      "features" = shiny::reactiveVal(
        example_iris_data() %>%
          dplyr::select(
            "feature_class",
            "feature_name" = "feature",
            "feature_display",
          ) %>%
          dplyr::distinct()
      ),
      "drilldown" = shiny::reactive(T)
    ),
    {
      session$setInputs("feature_choice" = "Sepal.Length")
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")

      expect_equal(feature_classes(), "feature_class")
      expect_false(display_feature_class_selection_ui())
      expect_type(output$feature_class_selection_ui, "list")
      expect_true(display_feature_selection_ui())
      expect_type(output$feature_selection_ui, "list")

      expect_named(
        distplot_data(),
        c("sample", "feature", "feature_value", "group", "group_description", "color")
      )
      expect_equal(distplot_source_name(), "proxy1-distplot")
      session$setInputs("plot_type_choice" = "Violin")
      expect_type(plotly_function(), "closure")
      expect_type(plot_fill_colors(), "character")
      expect_type(plot_title(), "character")
      expect_type(output$distplot, "character")
    }
  )
})

test_that("distributions_plot_server_with_2_classes", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "plot_data_func" = shiny::reactive(example_iris_data_func),
      "features" = shiny::reactiveVal(
        example_iris_data() %>%
          dplyr::select(
            "feature_class",
            "feature_name" = "feature",
            "feature_display",
            "feature_class2",
          ) %>%
          dplyr::distinct()
      ),
      "scale_method_default" = shiny::reactive("Log2"),
      "feature_default" = shiny::reactive("Sepal.Length"),
      "drilldown" = shiny::reactive(T)
    ),
    {
      session$setInputs("feature_choice" = "Sepal.Length")
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")
      session$setInputs("feature_class_choice" = "feature_class2")

      expect_true(display_feature_class_selection_ui())
      expect_equal(feature_classes(), c("feature_class", "feature_class2"))
      expect_type(output$feature_class_selection_ui, "list")
      expect_true(display_feature_selection_ui())

      expect_type(output$feature_selection_ui, "list")

      expect_named(
        distplot_data(),
        c("sample", "feature", "feature_value", "group", "group_description", "color")
      )
      session$setInputs("plot_type_choice" = "Violin")
      expect_type(output$distplot, "character")
    }
  )
})
