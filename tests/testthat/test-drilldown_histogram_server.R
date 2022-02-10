test_that("drilldown_histogram_server", {

  shiny::testServer(
    drilldown_histogram_server,
    args = list(
      "plot_data" = shiny::reactive(
        example_iris_data_one_dataset() %>%
          dplyr::mutate(
            "group_display"   = .data$group_name,
            "dataset_display" = .data$dataset_name
          )
      ),
      "eventdata" = shiny::reactive(dplyr::tibble(
        "x"   =  c("setosa", "setosa"),
        "key" =  c("Iris", "Iris")
      ))
    ),
    {
      expect_equal(selected_group(), "setosa")
      expect_equal(selected_dataset(), "Iris")
      expect_named(histogram_data(), "feature_value")
      expect_type(histogram_data(), "list")
      expect_type(output$histogram, "character")
    }
  )
})
