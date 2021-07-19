
test_that("drilldown_scatterplot_server_wide_data_2_features", {

  shiny::testServer(
    drilldown_scatterplot_server,
    args = list(
      "scatterplot_data" = shiny::reactive(
        get_pcawg_scatterplot_example() %>%
          dplyr::select("sample", "group", "TCR Evenness", "TCR Richness"),
      ),
      "selected_group" = shiny::reactive("C1")
    ),
    {
      expect_type(scatterplot_data(), "list")
      expect_equal(
        scatterplot_feature_columns(),
        c("TCR Evenness", "TCR Richness")
      )
      expect_false(display_feature_selection_ui())
      expect_type(formatted_scatterplot_data(), "list")
      expect_named(formatted_scatterplot_data(), c("x", "y", "text"))
    }
  )
})

test_that("drilldown_scatterplot_server_wide_data_3_features", {

  shiny::testServer(
    drilldown_scatterplot_server,
    args = list(
      "scatterplot_data" = shiny::reactive(get_pcawg_scatterplot_example()),
      "selected_group" = shiny::reactive("C1")
    ),
    {
      expect_type(scatterplot_data(), "list")
      expect_equal(
        scatterplot_feature_columns(),
        c("TCR Evenness", "TCR Richness", "TCR Shannon")
      )

      expect_null(x_feature_input)
      expect_null(y_feature_input)
      expect_true(display_feature_selection_ui())

      expect_type(output$x_feature_selection_ui, "list")
      session$setInputs("x_feature_choice" = "TCR Evenness")
      expect_type(output$y_feature_selection_ui, "list")
      session$setInputs("y_feature_choice" = "TCR Richness")
      expect_type(formatted_scatterplot_data(), "list")
      expect_named(formatted_scatterplot_data(), c("x", "y", "text"))

      session$setInputs("x_feature_choice" = "not_a_feature_x")
      expect_error(formatted_scatterplot_data())
    }
  )
})

test_that("drilldown_scatterplot_server_group_set_features", {

  shiny::testServer(
    drilldown_scatterplot_server,
    args = list(
      "scatterplot_data" = shiny::reactive(get_pcawg_scatterplot_example()),
      "x_feature_input"  = shiny::reactive("TCR Shannon"),
      "y_feature_input"  = shiny::reactive("TCR Richness"),
      "selected_group" = shiny::reactive("C1")
    ),
    {
      expect_type(scatterplot_data(), "list")
      expect_equal(
        scatterplot_feature_columns(),
        c("TCR Evenness", "TCR Richness", "TCR Shannon")
      )
      expect_equal(x_feature_input(), "TCR Shannon")
      expect_equal(y_feature_input(), "TCR Richness")
      expect_false(display_feature_selection_ui())

      expect_type(formatted_scatterplot_data(), "list")
      expect_named(formatted_scatterplot_data(), c("x", "y", "text"))
    }
  )
})
