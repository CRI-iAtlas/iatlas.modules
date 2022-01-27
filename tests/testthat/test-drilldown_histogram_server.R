test_that("drilldown_histogram_server", {

  shiny::testServer(
    drilldown_histogram_server,
    args = list(
      "plot_data" = shiny::reactive(
        example_iris_data() %>%
          dplyr::select(
            "group_name",
            "feature_value"
          ) %>%
          dplyr::mutate(
            "group_display" = .data$group_name
          )
      ),
      "eventdata" = shiny::reactive(dplyr::tibble("key" =  "setosa"))
    ),
    {
      expect_equal(selected_group(), "setosa")
      expect_named(histogram_data(), "feature_value")
      expect_type(histogram_data(), "list")
      expect_type(output$histogram, "character")
    }
  )
})
