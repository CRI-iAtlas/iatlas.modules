sample_data_names <- c(
  "sample_name",
  "feature_name",
  "group_name",
  "feature_value"
)

barplot_data_names <- c(
  "sample_name",
  "feature_name",
  "feature_display",
  "group_name",
  "group_display",
  "feature_value"
)

summarized_barplot_data_names <- c(
  "group_display",
  "feature_display",
  "text",
  "MEAN",
  "SE"
)

barplot_eventdata_names <- c("curveNumber", "pointNumber", "x", "y", "key")

test_result_object <- function(res){
  scatterplot_data <- res$scatterplot_data()
  expect_type(scatterplot_data, "list")
  expect_named(scatterplot_data, c("x", "y", "text"))
  barplot_data <- res$barplot_data()
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

      expect_null(feature_data())
      expect_null(validated_feature_data())
      expect_null(group_data())
      expect_null(validated_group_data())

      expect_false(display_feature_class_selection_ui())

      expect_named(validated_sample_data(), sample_data_names)
      expect_type(barplot_data(), "list")
      expect_named(barplot_data(), barplot_data_names)
      expect_type(summarized_barplot_data(), "list")
      expect_named(summarized_barplot_data(), summarized_barplot_data_names)
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")

      expect_type(barplot_eventdata(), "list")
      expect_named(barplot_eventdata(), barplot_eventdata_names)
      expect_equal(selected_group(), "virginica")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c("sample_name", "group_display", "Sepal.Length", "Petal.Length")
      )

      test_result_object(session$getReturned())
    }
  )
})


test_that("barplot_server_iris_feature_data", {

  shiny::testServer(
    barplot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data),
      "feature_data" = shiny::reactive(example_iris_data_features_1_class2()),
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

      expect_type(feature_data(), "list")
      expect_type(validated_feature_data(), "list")
      expect_named(
        validated_feature_data(),
        c('feature_name', 'feature_display', 'feature_class')
      )
      expect_null(group_data())
      expect_null(validated_group_data())

      expect_true(display_feature_class_selection_ui())
      expect_type(output$feature_class_selection_ui, "list")
      expect_named(validated_sample_data(), sample_data_names)

      expect_type(barplot_data(), "list")
      expect_named(barplot_data(), barplot_data_names)
      expect_type(summarized_barplot_data(), "list")
      expect_named(summarized_barplot_data(), summarized_barplot_data_names)
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")

      expect_type(barplot_eventdata(), "list")
      expect_named(barplot_eventdata(), barplot_eventdata_names)
      expect_equal(selected_group(), "virginica")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c("sample_name", "group_display", "Sepal Length", "Petal Length")
      )

      test_result_object(session$getReturned())
    }
  )
})

test_that("barplot_server_iris_group_data", {

  shiny::testServer(
    barplot_server,
    args = list(
      "sample_data_func" = shiny::reactive(example_iris_data),
      "group_data" = shiny::reactive(example_iris_data_groups()),
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

      expect_null(feature_data())
      expect_null(validated_feature_data())
      expect_type(group_data(), "list")
      expect_type(validated_group_data(), "list")
      expect_named(
        validated_group_data(),
        c("group_name", "group_display", "group_color", "group_description")
      )

      expect_false(display_feature_class_selection_ui())

      expect_named(validated_sample_data(), sample_data_names)
      expect_type(barplot_data(), "list")
      expect_named(barplot_data(), barplot_data_names)
      expect_type(summarized_barplot_data(), "list")
      expect_named(summarized_barplot_data(), summarized_barplot_data_names)
      expect_equal(barplot_source_name(), "proxy1-barplot")
      expect_type(output$barplot, "character")

      expect_type(barplot_eventdata(), "list")
      expect_named(barplot_eventdata(), barplot_eventdata_names)
      expect_equal(selected_group(), "virginica")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c("sample_name", "group_display", "Sepal.Length", "Petal.Length")
      )

      test_result_object(session$getReturned())
    }
  )
})

#
# test_that("barplot_server_iris2", {
#
#   shiny::testServer(
#     barplot_server,
#     args = list(
#       "plot_data_func" = shiny::reactive(example_iris_data_func),
#       "drilldown" = shiny::reactive(T)
#     ),
#     {
#       session$setInputs("mock_event_data" = data.frame(
#         "curveNumber" = 1,
#         "pointNumber" = 2,
#         "x" = "virginica",
#         "y" = 6.588,
#         "key" = "virginica"
#       ))
#       expect_false(display_feature_class_selection_ui())
#       expect_type(barplot_data(), "list")
#       expect_named(
#         barplot_data(),
#         c(
#           "sample_name",
#           "feature_name",
#           "feature_display",
#           "feature_value",
#           "group_name",
#           "group_description"
#         )
#       )
#       expect_type(summarized_barplot_data(), "list")
#       expect_named(
#         summarized_barplot_data(),
#         c("group_name", "feature_display", "text", "MEAN", "SE")
#       )
#       expect_equal(barplot_source_name(), "proxy1-barplot")
#       expect_type(output$barplot, "character")
#
#       expect_type(group_data(), "list")
#       expect_named(group_data(), c("group_name", "group_description"))
#
#       expect_type(group_data(), "list")
#       expect_named(group_data(), c("group_name", "group_description"))
#
#       expect_type(barplot_eventdata(), "list")
#       expect_named(barplot_eventdata(), c("curveNumber", "pointNumber", "x", "y", "key"))
#       expect_equal(selected_group(), "virginica")
#       expect_type(scatterplot_data(), "list")
#       expect_named(
#         scatterplot_data(),
#         c(
#           'sample_name',
#           'group_name',
#           'Sepal Length',
#           'Sepal Width',
#           'Petal Length',
#           'Petal Width'
#           )
#       )
#
#       res <- session$getReturned()
#       scatterplot_data <- res$scatterplot_data()
#       expect_type(scatterplot_data, "list")
#       expect_named(scatterplot_data, c("x", "y", "text"))
#       barplot_data <- res$barplot_data()
#       expect_type(barplot_data, "list")
#       expect_named(
#         barplot_data,
#         c('group_name', 'feature_display', 'text', 'MEAN', 'SE')
#       )
#     }
#   )
# })
#
# test_that("barplot_server_starwars", {
#
#   shiny::testServer(
#     barplot_server,
#     args = list(
#       "plot_data_function" = shiny::reactive(example_starwars_data_func)
#     ),
#     {
#       expect_false(display_feature_class_selection_ui())
#       expect_type(barplot_data(), "list")
#       expect_named(
#         barplot_data(),
#         c(
#           "sample_name",
#           "feature_name",
#           "feature_display",
#           "feature_value",
#           "group_name"
#         )
#       )
#       expect_type(summarized_barplot_data(), "list")
#       expect_named(
#         summarized_barplot_data(),
#         c("group_name", "feature_display", "text", "MEAN", "SE")
#       )
#       expect_equal(barplot_source_name(), "proxy1-barplot")
#       expect_type(output$barplot, "character")
#       expect_error(
#         barplot_eventdata(),
#         regexp = "Click on above barplot.",
#         class = c("shiny.silent.error")
#       )
#     }
#   )
# })
#
# test_that("barplot_server_pcawg", {
#
#   shiny::testServer(
#     barplot_server,
#     args = list(
#       "plot_data_function" = shiny::reactive(get_pcawg_feature_values_by_class),
#       "feature_classes" = shiny::reactive(
#         "Immune Cell Proportion - Common Lymphoid and Myeloid Cell Derivative Class"
#       )
#     ),
#     {
#       session$setInputs(
#         "feature_class_choice" =
#           "Immune Cell Proportion - Common Lymphoid and Myeloid Cell Derivative Class"
#         )
#       session$setInputs("mock_event_data" = data.frame(
#         "curveNumber" = 1,
#         "pointNumber" = 2,
#         "x" = "C4",
#         "y" = 6.588,
#         "key" = "C4"
#       ))
#       expect_true(display_feature_class_selection_ui())
#       expect_type(barplot_data(), "list")
#       expect_named(
#         barplot_data(),
#         c(
#           "sample_name",
#           "feature_name",
#           "feature_display",
#           "feature_value",
#           "group_name",
#           "group_description"
#         )
#       )
#       expect_type(summarized_barplot_data(), "list")
#       expect_named(
#         summarized_barplot_data(),
#         c("group_name", "feature_display", "text", "MEAN", "SE")
#       )
#       expect_equal(barplot_source_name(), "proxy1-barplot")
#       expect_type(output$barplot, "character")
#       expect_type(group_data(), "list")
#       expect_named(group_data(), c("group_name", "group_description"))
#
#       expect_type(barplot_eventdata(), "list")
#       expect_named(
#         barplot_eventdata(),
#         c("curveNumber", "pointNumber", "x", "y", "key")
#       )
#       expect_equal(selected_group(), "C4")
#       expect_type(scatterplot_data(), "list")
#       expect_named(
#         scatterplot_data(),
#         c(
#           'sample_name',
#           'group_name',
#           'B Cells',
#           'Dendritic Cells',
#           'Eosinophils',
#           'Macrophages',
#           'Mast Cells',
#           'Neutrophils',
#           'NK Cells',
#           'T Cells CD4',
#           'T Cells CD8'
#           )
#       )
#
#       res <- session$getReturned()
#       scatterplot_data <- res$scatterplot_data()
#       expect_type(scatterplot_data, "list")
#       expect_named(scatterplot_data, c("x", "y", "text"))
#       barplot_data <- res$barplot_data()
#       expect_type(barplot_data, "list")
#       expect_named(
#         barplot_data,
#         c('group_name', 'feature_display', 'text', 'MEAN', 'SE')
#       )
#
#     }
#   )
# })
#
# test_that("barplot_server_tcga", {
#
#   shiny::testServer(
#     barplot_server,
#     args = list(
#       "plot_data_function" = shiny::reactive(get_tcga_cell_proportions),
#       "barplot_xlab"    = shiny::reactive("Fraction type by group"),
#       "barplot_ylab"    = shiny::reactive("Fraction mean"),
#       "barplot_label"   = shiny::reactive("Fraction"),
#       "drilldown"       = shiny::reactive(T),
#       "y_feature_input" = shiny::reactive("Leukocyte Fraction"),
#       "x_feature_input" = shiny::reactive("Stromal Fraction")
#     ),
#     {
#       session$setInputs("mock_event_data" = data.frame(
#         "curveNumber" = 1,
#         "pointNumber" = 2,
#         "x" = "C4",
#         "y" = 6.588,
#         "key" = "C4"
#       ))
#       expect_false(display_feature_class_selection_ui())
#       expect_type(barplot_data(), "list")
#       expect_named(
#         barplot_data(),
#         c(
#           "sample_name",
#           "feature_name",
#           "feature_display",
#           "feature_value",
#           "group_name",
#           "group_description"
#         )
#       )
#       expect_type(summarized_barplot_data(), "list")
#       expect_named(
#         summarized_barplot_data(),
#         c("group_name", "feature_display", "text", "MEAN", "SE")
#       )
#       expect_equal(barplot_source_name(), "proxy1-barplot")
#       expect_type(output$barplot, "character")
#       expect_type(group_data(), "list")
#       expect_named(group_data(), c("group_name", "group_description"))
#
#       expect_type(barplot_eventdata(), "list")
#       expect_named(
#         barplot_eventdata(),
#         c("curveNumber", "pointNumber", "x", "y", "key")
#       )
#       expect_equal(selected_group(), "C4")
#       expect_type(scatterplot_data(), "list")
#       expect_named(
#         scatterplot_data(),
#         c(
#           'sample_name',
#           'group_name',
#           'Leukocyte Fraction',
#           'Stromal Fraction',
#           'Tumor Fraction'
#         )
#       )
#
#       res <- session$getReturned()
#       scatterplot_data <- res$scatterplot_data()
#       expect_type(scatterplot_data, "list")
#       expect_named(scatterplot_data, c("x", "y", "text"))
#       barplot_data <- res$barplot_data()
#       expect_type(barplot_data, "list")
#       expect_named(
#         barplot_data,
#         c('group_name', 'feature_display', 'text', 'MEAN', 'SE')
#       )
#
#     }
#   )
# })
