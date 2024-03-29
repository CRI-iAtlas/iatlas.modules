validated_sample_names <- c(
  "sample_name",
  "feature_name",
  "group_name",
  "dataset_name",
  "feature_value"
)

distplot_data_names <-  c(
  "sample_name",
  "feature_name",
  "feature_display",
  "group_name",
  "dataset_name",
  "feature_value"
)

feature_names <- c(
  "Sepal.Length",  "Sepal.Width", "Petal.Length",  "Petal.Width"
)


test_result_object <- function(res){
  expect_named(
    res,
    c("histogram_data", "distplot_data", "group_text")
  )

  expect_type(res$group_text, "character")

  histogram_data <- res$histogram_data
  expect_true(tibble::is_tibble(histogram_data))
  expect_named(histogram_data, "feature_value")
  distplot_data <- res$distplot_data
  expect_type(distplot_data, "list")
  expect_named(
    distplot_data,
    c(
      'sample_name',
      'group_display',
      'group_color',
      'group_description',
      'feature_display',
      'feature_value',
      'dataset_display'
    )
  )
}

test_that("distributions_plot_server_no_features_no_feature_data", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data_one_dataset),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species"),
      "mock_event_data" = shiny::reactive(data.frame(
        "curveNumber" = c(0,0),
        "pointNumber" = c(0,0),
        "x" = "setosa",
        "y" = c(5.1, 2.1),
        "key" = "Iris"
      ))
    ),
    {
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")
      session$setInputs("plot_type_choice" = "Violin")

      expect_null(feature_data())
      expect_null(validated_feature_data())

      expect_equal(feature_classes(), character(0))
      expect_false(display_feature_class_selection_ui())
      expect_error(feature_list())
      expect_false(display_feature_selection_ui())


      expect_named(validated_sample_data(), validated_sample_names)
      expect_named(distplot_data(), distplot_data_names)


      test_result_object(ploted_data())
    }
  )
})

test_that("distributions_plot_server_no_features_no_classes", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data_one_dataset),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species"),
      "feature_data" = shiny::reactive(
        example_iris_data_feature_data_no_class()
      ),
      "mock_event_data" = shiny::reactive(data.frame(
        "curveNumber" = c(0,0),
        "pointNumber" = c(0,0),
        "x" = "setosa",
        "y" = c(5.1, 2.1),
        "key" = "Iris"
      ))
    ),
    {
      session$setInputs("feature_choice" = "Sepal.Length")
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")
      session$setInputs("plot_type_choice" = "Violin")

      expect_true(tibble::is_tibble(validated_feature_data()))
      expect_named(
        validated_feature_data(),
        c("feature_name", "feature_display")
      )

      expect_equal(feature_classes(), character(0))
      expect_false(display_feature_class_selection_ui())
      expect_type(feature_list(), "character")
      expect_named(feature_list(), feature_names)
      expect_true(display_feature_selection_ui())

      expect_named(validated_sample_data(), validated_sample_names)
      expect_named(distplot_data(), distplot_data_names)

      test_result_object(ploted_data())

    }
  )
})

test_that("distributions_plot_server_1_class", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data_one_dataset),
      "feature_data" = shiny::reactive(
        example_distributions_iris_data_feature_data_1_class()
      ),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species"),
      "mock_event_data" = shiny::reactive(data.frame(
        "curveNumber" = c(0,0),
        "pointNumber" = c(0,0),
        "x" = "setosa",
        "y" = c(5.1, 2.1),
        "key" = "Iris"
      ))
    ),
    {
      session$setInputs("feature_choice" = "Sepal.Length")
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")
      session$setInputs("plot_type_choice" = "Violin")


      expect_true(tibble::is_tibble(feature_data()))
      expect_named(
        feature_data(),
        c(
          "feature_name",
          "feature_display",
          "Class1"
        )
      )
      expect_true(tibble::is_tibble(validated_feature_data()))
      expect_named(
        validated_feature_data(),
        c(
          "feature_name",
          "feature_display",
          "Class1"
        )
      )

      expect_equal(feature_classes(), "Class1")
      expect_false(display_feature_class_selection_ui())
      expect_error(output$feature_class_selection_ui)
      expect_true(display_feature_selection_ui())
      expect_type(feature_list(), "list")
      expect_named(feature_list(), c("Length", "Width"))
      expect_type(output$feature_selection_ui, "list")

      expect_named(validated_sample_data(), validated_sample_names)
      expect_named(distplot_data(), distplot_data_names)

      test_result_object(ploted_data())
    }
  )
})

test_that("distributions_plot_server_2_classes", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data_one_dataset),
      "feature_data" = shiny::reactive(
        example_distributions_iris_data_feature_data_2_classes()
      ),
      "group_data" = shiny::reactive(example_iris_data_groups()),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species"),
      "mock_event_data" = shiny::reactive(data.frame(
        "curveNumber" = c(0,0),
        "pointNumber" = c(0,0),
        "x" = "Setosa",
        "y" = c(5.1, 2.1),
        "key" = "Iris"
      ))
    ),
    {
      session$setInputs("feature_class_choice" = "Class1")
      session$setInputs("feature_choice" = "Sepal.Length")
      session$setInputs("scale_method_choice" = "None")
      session$setInputs("reorder_method_choice" = "None")
      session$setInputs("plot_type_choice" = "Violin")


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

      expect_equal(feature_classes(), c("Class1", "Class2"))
      expect_true(display_feature_class_selection_ui())
      expect_type(output$feature_class_selection_ui, "list")
      expect_true(display_feature_selection_ui())
      expect_type(feature_list(), "list")
      expect_named(feature_list(), c("Length", "Width"))
      expect_type(output$feature_selection_ui, "list")

      expect_named(validated_sample_data(), validated_sample_names)
      expect_named(distplot_data(), distplot_data_names)

      test_result_object(ploted_data())
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
        "Columns in sample_data"
      )
    }
  )
})

test_that("distributions_plot_server_data_duplicated_row", {

  shiny::testServer(
    distributions_plot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data_one_dataset),
      "feature_data" = shiny::reactive(
        example_distributions_iris_data_feature_data_duplicated_feature()
      ),
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



