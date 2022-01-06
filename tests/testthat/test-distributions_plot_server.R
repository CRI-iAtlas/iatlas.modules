test_that("distributions_plot_server_no_features_no_class", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species")
    ),
    {
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")
      session$setInputs("plot_type_choice" = "Violin")
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = c(0,0),
        "pointNumber" = c(0,0),
        "x" = "setosa",
        "y" = c(5.1, 2.1),
        "key" = "setosa"
      ))

      expect_null(feature_data())
      expect_null(validated_feature_data())
      expect_null(group_data())
      expect_null(validated_group_data())

      expect_equal(feature_classes(), character(0))
      expect_false(display_feature_class_selection_ui())
      expect_false(display_feature_selection_ui())
      expect_named(
        validated_sample_data(),
        c(
          "sample_name",
          "feature_name",
          "group_name",
          "feature_value"
        )
      )

      expect_named(
        distplot_data(),
        c(
          "sample_name",
          "feature_name",
          "feature_display",
          "group_name",
          "group_display",
          "feature_value"
        )
      )

      expect_equal(distplot_source_name(), "proxy1-distplot")
      expect_equal(plotly_function(), plotly_violin)
      expect_null(plot_fill_colors())
      expect_equal(plot_title(), "")

      expect_type(output$distplot, "character")
      expect_type(distplot_eventdata(), "list")
      expect_named(
        distplot_eventdata(),
        c("curveNumber", "pointNumber", "x", "y", "key")
      )

      res <- session$getReturned()
      histogram_data <- res$histogram_data()
      expect_type(histogram_data, "list")
      expect_named(histogram_data, "feature_value")
      distplot_data <- res$distplot_data()
      expect_type(distplot_data, "list")
      expect_named(
        distplot_data,
        c(
          'sample_name',
          'feature_name',
          'feature_display',
          'group_name',
          'group_display',
          'feature_value'
        )
      )

    }
  )
})

test_that("distributions_plot_server_1_class", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data),
      "feature_data" = shiny::reactive(example_iris_data_features_1_class()),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species")
    ),
    {
      session$setInputs("feature_choice" = "Sepal.Length")
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")
      session$setInputs("plot_type_choice" = "Violin")
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = c(0,0),
        "pointNumber" = c(0,0),
        "x" = "setosa",
        "y" = c(5.1, 2.1),
        "key" = "setosa"
      ))

      expect_type(feature_data(), "list")
      expect_named(
        feature_data(),
        c(
          "feature_name",
          "feature_display",
          "Class1"
        )
      )
      expect_type(validated_feature_data(), "list")
      expect_named(
        validated_feature_data(),
        c(
          "feature_name",
          "feature_display",
          "Class1"
        )
      )

      expect_null(group_data())
      expect_null(validated_group_data())

      expect_equal(feature_classes(), "Class1")
      expect_false(display_feature_class_selection_ui())
      expect_type(output$feature_class_selection_ui, "list")
      expect_true(display_feature_selection_ui())
      expect_type(feature_list(), "list")
      expect_type(output$feature_selection_ui, "list")

      expect_named(
        validated_sample_data(),
        c(
          "sample_name",
          "feature_name",
          "group_name",
          "feature_value"
        )
      )

      expect_named(
        distplot_data(),
        c(
          "sample_name",
          "feature_name",
          "feature_display",
          "group_name",
          "group_display",
          "feature_value"
        )
      )

      expect_equal(distplot_source_name(), "proxy1-distplot")
      expect_equal(plotly_function(), plotly_violin)
      expect_null(plot_fill_colors())
      expect_equal(plot_title(), "Sepal Length")

      expect_type(output$distplot, "character")
      expect_type(distplot_eventdata(), "list")
      expect_named(
        distplot_eventdata(),
        c("curveNumber", "pointNumber", "x", "y", "key")
      )

      res <- session$getReturned()
      histogram_data <- res$histogram_data()
      expect_type(histogram_data, "list")
      expect_named(histogram_data, "feature_value")
      distplot_data <- res$distplot_data()
      expect_type(distplot_data, "list")
      expect_named(
        distplot_data,
        c(
          'sample_name',
          'feature_name',
          'feature_display',
          'group_name',
          'group_display',
          'feature_value'
        )
      )
    }
  )
})

test_that("distributions_plot_server_2_classes", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data),
      "feature_data" = shiny::reactive(example_iris_data_features_2_classes()),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species")
    ),
    {
      session$setInputs("feature_choice" = "Sepal.Length")
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")
      session$setInputs("plot_type_choice" = "Violin")
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = c(0,0),
        "pointNumber" = c(0,0),
        "x" = "setosa",
        "y" = c(5.1, 2.1),
        "key" = "Setosa"
      ))

      expect_type(feature_data(), "list")
      expect_type(validated_feature_data(), "list")
      expect_named(
        validated_feature_data(),
        c(
          "feature_name",
          "feature_display",
          "Class1",
          "Class2"
        )
      )

      expect_type(group_data(), "list")
      expect_type(validated_group_data(), "list")
      expect_named(
        validated_group_data(),
        c(
          "group_name",
          "group_display",
          "group_color",
          "group_description"
        )
      )

      expect_equal(feature_classes(), c("Class1", "Class2"))
      expect_true(display_feature_class_selection_ui())
      expect_type(output$feature_class_selection_ui, "list")
      expect_true(display_feature_selection_ui())
      expect_type(feature_list(), "list")
      expect_type(output$feature_selection_ui, "list")

      expect_named(
        validated_sample_data(),
        c(
          "sample_name",
          "feature_name",
          "group_name",
          "feature_value"
        )
      )

      expect_named(
        distplot_data(),
        c(
          "sample_name",
          "feature_name",
          "feature_display",
          "group_name",
          "group_display",
          "feature_value"
        )
      )

      expect_equal(distplot_source_name(), "proxy1-distplot")
      expect_equal(plotly_function(), plotly_violin)
      expect_equal(
        names(plot_fill_colors()),
        c('Setosa', 'Versicolor', 'Virginica')
      )
      expect_equal(
        unname(plot_fill_colors()),
        c('#FF0000', '#0000FF', '#FFFF00')
      )
      expect_equal(plot_title(), "Sepal Length")

      expect_type(output$distplot, "character")
      expect_type(distplot_eventdata(), "list")
      expect_named(
        distplot_eventdata(),
        c("curveNumber", "pointNumber", "x", "y", "key")
      )

      res <- session$getReturned()
      histogram_data <- res$histogram_data()
      expect_type(histogram_data, "list")
      expect_named(histogram_data, "feature_value")
      distplot_data <- res$distplot_data()
      expect_type(distplot_data, "list")
      expect_named(
        distplot_data,
        c(
          'sample_name',
          'feature_name',
          'feature_display',
          'group_name',
          'group_display',
          'feature_value'
        )
      )
    }
  )
})

test_that("distributions_plot_server_2_classes", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data),
      "feature_data" = shiny::reactive(example_iris_data_features_2_classes()),
      "group_data" = shiny::reactive(example_iris_data_groups2()),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species")
    ),
    {
      session$setInputs("feature_choice" = "Sepal.Length")
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")
      session$setInputs("plot_type_choice" = "Violin")
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = c(0,0),
        "pointNumber" = c(0,0),
        "x" = "setosa",
        "y" = c(5.1, 2.1),
        "key" = "Setosa"
      ))

      expect_named(
        validated_group_data(),
        c('group_name', 'group_display', 'group_color', 'group_description')
      )

      expect_null(plot_fill_colors())

      res <- session$getReturned()
      histogram_data <- res$histogram_data()
      expect_type(histogram_data, "list")
      expect_named(histogram_data, "feature_value")
      distplot_data <- res$distplot_data()
      expect_type(distplot_data, "list")
      expect_named(
        distplot_data,
        c(
          'sample_name',
          'feature_name',
          'feature_display',
          'group_name',
          'group_display',
          'feature_value'
        )
      )
    }
  )
})

test_that("distributions_plot_server_data_missing_column", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data_missing_column),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species")
    ),
    {
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")
      session$setInputs("plot_type_choice" = "Violin")

      expect_error(
        validated_sample_data(),
        "Columns in table from sample_data_function"
      )
    }
  )
})

test_that("distributions_plot_server_data_missing_column2", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data),
      "feature_data" = shiny::reactive(example_iris_data_features_missing_column()),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species")
    ),
    {
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")
      session$setInputs("plot_type_choice" = "Violin")

      expect_type(feature_data(), "list")
      expect_named(
        feature_data(),
        c(
          "feature_name",
          "Class1"
        )
      )
      expect_error(validated_feature_data(), "Columns in fetaure_data")
    }
  )
})

test_that("distributions_plot_server_data_duplicated_row", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data),
      "feature_data" = shiny::reactive(example_iris_data_features_duplicated_feature()),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species")
    ),
    {
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")
      session$setInputs("plot_type_choice" = "Violin")

      expect_type(feature_data(), "list")
      expect_named(
        feature_data(),
        c(
          "feature_name",
          "feature_display",
          "Class1"
        )
      )
      expect_error(validated_feature_data(), "Values in feature_data")
    }
  )
})
