test_that("plotly_server", {
  plot_data <- dplyr::starwars %>%
    dplyr::select(
      "x" = "species",
      "color" = "gender",
      "y" = "height"
    ) %>%
    tidyr::drop_na() %>%
    dplyr::group_by(.data$x, .data$color) %>%
    dplyr::summarise(
      "y" = mean(.data$y),
      "count" = dplyr::n(),
      .groups = "drop"
    )  %>%
  dplyr::mutate("error" = .data$y / sqrt(.data$count))

  eventdata <- dplyr::filter(plot_data, x == "Aleena")

  group_data <- plot_data %>%
    dplyr::select("group_name" = "x") %>%
    dplyr::distinct() %>%
    dplyr::mutate("group_description" = stringr::str_c("Race: ", .data$group_name))


  shiny::testServer(
    plotly_server,
    args = list(
      "plot_data" = shiny::reactiveVal(plot_data),
      "group_data" = shiny::reactiveVal(group_data),
      "eventdata" = shiny::reactiveVal(eventdata)
    ),
    {
      expect_true(show_group_text())
      expect_equal(output$plot_group_text, "Race: Aleena")
      expect_type(output$download_tbl, "character")
    }
  )
})
