merged_distplot_names <- c(
  "sample_name",
  "group_display",
  "group_color",
  "group_description",
  "feature_display",
  "feature_value",
  "dataset_display"
)

formatted_distplot_names <- c(
  "group_display", "dataset_display", "feature_value"
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
  expect_named(distplot_data, merged_distplot_names)
}

#
# test_that("distributions_plot_server2_one_dataset", {
#
#   shiny::testServer(
#     distributions_plot_server2,
#     args = list(
#       "distplot_data" = shiny::reactive(
#         dplyr::rename(
#           example_iris_data_one_dataset(),
#           "feature_display" = "feature_name"
#         )
#       ),
#       "plot_type" = shiny::reactive("Violin"),
#       "drilldown" = shiny::reactive(T),
#       "distplot_xlab" = shiny::reactive("Species")
#     ),
#     {
#       session$setInputs("mock_event_data" = data.frame(
#         "curveNumber" = c(0,0),
#         "pointNumber" = c(0,0),
#         "x" = "setosa",
#         "y" = c(5.1, 2.1),
#         "key" = "Iris"
#       ))
#
#       expect_true(tibble::is_tibble(validated_group_data()))
#       expect_true(tibble::is_tibble(validated_dataset_data()))
#       expect_true(tibble::is_tibble(validated_distplot_data()))
#
#       expect_true(tibble::is_tibble(merged_distplot_data()))
#       expect_true(nrow(merged_distplot_data()) > 0)
#       expect_named(merged_distplot_data(), merged_distplot_names)
#
#       expect_true(tibble::is_tibble(formatted_distplot_data()))
#       expect_true(nrow(formatted_distplot_data()) > 0)
#       expect_named(formatted_distplot_data(), formatted_distplot_names)
#
#       expect_equal(distplot_source_name(), "proxy1-distplot")
#       expect_equal(plotly_function(), plotly_violin)
#       expect_null(plot_fill_colors())
#
#       expect_type(distplots(), "list")
#       expect_length(distplots(), 1)
#       expect_type(output$distplot, "character")
#       test_result_object(session$getReturned()())
#     }
#   )
# })
#
# test_that("distributions_plot_server2_two_datasets", {
#
#   shiny::testServer(
#     distributions_plot_server2,
#     args = list(
#       "distplot_data" = shiny::reactive(
#         dplyr::rename(
#           example_iris_data_two_datasets(),
#           "feature_display" = "feature_name"
#         )
#       ),
#       "plot_type" = shiny::reactive("Violin"),
#       "drilldown" = shiny::reactive(T),
#       "distplot_xlab" = shiny::reactive("Species")
#     ),
#     {
#       session$setInputs("mock_event_data" = data.frame(
#         "curveNumber" = c(0,0),
#         "pointNumber" = c(0,0),
#         "x" = "setosa",
#         "y" = c(5.1, 2.1),
#         "key" = "Iris1"
#       ))
#
#       expect_true(tibble::is_tibble(validated_group_data()))
#       expect_true(tibble::is_tibble(validated_dataset_data()))
#       expect_true(tibble::is_tibble(validated_distplot_data()))
#
#       expect_true(tibble::is_tibble(merged_distplot_data()))
#       expect_true(nrow(merged_distplot_data()) > 0)
#       expect_named(merged_distplot_data(), merged_distplot_names)
#
#       expect_true(tibble::is_tibble(formatted_distplot_data()))
#       expect_true(nrow(formatted_distplot_data()) > 0)
#       expect_named(formatted_distplot_data(), formatted_distplot_names)
#
#       expect_equal(distplot_source_name(), "proxy1-distplot")
#       expect_equal(plotly_function(), plotly_violin)
#       expect_null(plot_fill_colors())
#
#       expect_type(distplots(), "list")
#       expect_length(distplots(), 2)
#       expect_type(output$distplot, "character")
#       test_result_object(session$getReturned()())
#     }
#   )
# })
#
# test_that("distributions_plot_server2_with_group_data", {
#
#   shiny::testServer(
#     distributions_plot_server2,
#     args = list(
#       "distplot_data" = shiny::reactive(
#         dplyr::rename(
#           example_iris_data_one_dataset(),
#           "feature_display" = "feature_name"
#         )
#       ),
#       "group_data" = shiny::reactive(example_iris_data_groups()),
#       "drilldown" = shiny::reactive(T),
#       "distplot_xlab" = shiny::reactive("Species")
#     ),
#     {
#       session$setInputs("mock_event_data" = data.frame(
#         "curveNumber" = c(0,0),
#         "pointNumber" = c(0,0),
#         "x" = "Setosa",
#         "y" = c(5.1, 2.1),
#         "key" = "Iris"
#       ))
#
#       expect_true(tibble::is_tibble(validated_group_data()))
#       expect_true(tibble::is_tibble(validated_dataset_data()))
#       expect_true(tibble::is_tibble(validated_distplot_data()))
#
#       expect_true(tibble::is_tibble(merged_distplot_data()))
#       expect_true(nrow(merged_distplot_data()) > 0)
#       expect_named(merged_distplot_data(), merged_distplot_names)
#
#       expect_true(tibble::is_tibble(formatted_distplot_data()))
#       expect_true(nrow(formatted_distplot_data()) > 0)
#       expect_named(formatted_distplot_data(), formatted_distplot_names)
#
#       expect_equal(distplot_source_name(), "proxy1-distplot")
#       expect_equal(plotly_function(), plotly_violin)
#       expect_type(plot_fill_colors(), "character")
#
#       expect_type(output$distplot, "character")
#       test_result_object(session$getReturned()())
#     }
#   )
# })
#
# test_that("distributions_plot_server2_with_dataset_data", {
#
#   shiny::testServer(
#     distributions_plot_server2,
#     args = list(
#       "distplot_data" = shiny::reactive(
#         dplyr::rename(
#           example_iris_data_two_datasets(),
#           "feature_display" = "feature_name"
#         )
#       ),
#       "dataset_data" = shiny::reactive(dplyr::tibble(
#         "dataset_name" = c("Iris1", "Iris2"),
#         "dataset_display" = c("Iris 1", "Iris 2")
#       )),
#       "drilldown" = shiny::reactive(T),
#       "distplot_xlab" = shiny::reactive("Species")
#     ),
#     {
#       session$setInputs("mock_event_data" = data.frame(
#         "curveNumber" = c(0,0),
#         "pointNumber" = c(0,0),
#         "x" = "setosa",
#         "y" = c(5.1, 2.1),
#         "key" = "Iris 1"
#       ))
#
#       expect_true(tibble::is_tibble(validated_group_data()))
#       expect_true(tibble::is_tibble(validated_dataset_data()))
#       expect_true(tibble::is_tibble(validated_distplot_data()))
#
#       expect_true(tibble::is_tibble(merged_distplot_data()))
#       expect_true(nrow(merged_distplot_data()) > 0)
#       expect_named(merged_distplot_data(), merged_distplot_names)
#
#       expect_true(tibble::is_tibble(formatted_distplot_data()))
#       expect_true(nrow(formatted_distplot_data()) > 0)
#       expect_named(formatted_distplot_data(), formatted_distplot_names)
#
#       expect_equal(distplot_source_name(), "proxy1-distplot")
#       expect_equal(plotly_function(), plotly_violin)
#       expect_null(plot_fill_colors())
#
#       expect_type(distplots(), "list")
#       expect_length(distplots(), 2)
#       expect_type(output$distplot, "character")
#       test_result_object(session$getReturned()())
#     }
#   )
# })

test_that("distributions_plot_server2_reorder", {

  shiny::testServer(
    distributions_plot_server2,
    args = list(
      "distplot_data" = shiny::reactive(
        dplyr::rename(
          example_iris_data_one_dataset(),
          "feature_display" = "feature_name"
        )
      ),
      "plot_type" = shiny::reactive("Violin"),
      "drilldown" = shiny::reactive(T),
      "distplot_xlab" = shiny::reactive("Species"),
      "reorder_method" = shiny::reactive("Median")
    ),
    {
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = c(0,0),
        "pointNumber" = c(0,0),
        "x" = "setosa",
        "y" = c(5.1, 2.1),
        "key" = "Iris"
      ))

      expect_true(tibble::is_tibble(validated_group_data()))
      expect_true(tibble::is_tibble(validated_dataset_data()))
      expect_true(tibble::is_tibble(validated_distplot_data()))

      expect_true(tibble::is_tibble(merged_distplot_data()))
      expect_true(nrow(merged_distplot_data()) > 0)
      expect_named(merged_distplot_data(), merged_distplot_names)

      expect_true(tibble::is_tibble(formatted_distplot_data()))
      expect_true(nrow(formatted_distplot_data()) > 0)
      expect_named(formatted_distplot_data(), formatted_distplot_names)

      expect_equal(distplot_source_name(), "proxy1-distplot")
      expect_equal(plotly_function(), plotly_violin)
      expect_null(plot_fill_colors())

      expect_type(distplots(), "list")
      expect_length(distplots(), 1)
      expect_type(output$distplot, "character")
      test_result_object(session$getReturned()())
    }
  )
})
