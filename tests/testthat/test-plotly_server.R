test_that("plotly_server", {
  plot_data <- example_iris_data() %>%
    dplyr::select(
      "x" = "feature_name",
      "color" = "group_name",
      "y" = "feature_value"
    ) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$x, .data$color) %>%
    dplyr::summarise(
      "y" = mean(.data$y),
      "count" = dplyr::n(),
      .groups = "drop"
    )  %>%
  dplyr::mutate("error" = .data$y / sqrt(.data$count))

  eventdata <- dplyr::filter(plot_data, x == "Petal.Length")

  group_data <- plot_data %>%
    dplyr::select("group_display" = "x") %>%
    dplyr::distinct() %>%
    dplyr::mutate("group_description" = stringr::str_c("Race: ", .data$group_display))


  shiny::testServer(
    plotly_server,
    args = list(
      "plot_data" = shiny::reactiveVal(plot_data),
      "group_data" = shiny::reactiveVal(group_data),
      "eventdata" = shiny::reactiveVal(eventdata)
    ),
    {
      expect_true(show_group_text())
      expect_equal(output$plot_group_text, "Race: Petal.Length")
      expect_type(output$download_tbl, "character")
    }
  )
})
