test_that("heatmap_server", {

  shiny::testServer(
    heatmap_server,
    args = list(
      "feature_classes" = shiny::reactive(get_pcawg_feature_class_list()),
      "response_features" = shiny::reactive(get_pcawg_feature_list()),
      "feature_data_function" = shiny::reactive(get_pcawg_feature_values_by_class),
      "response_data_function" = shiny::reactive(get_pcawg_feature_values_by_feature),
      "summarise_function_list" = shiny::reactive(
        purrr::partial(stats::cor, method = "pearson")
      ),
      "drilldown" = shiny::reactive(T)
    ),
    {
      session$setInputs("feature_class_choice" = "Adaptive Receptor - T cell")
      session$setInputs("response_feature_choice" = "age_at_diagnosis")
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "C4",
        "y" = "TCR Richness",
        "z" = "0.1805093"
      ))

      expect_type(output$class_selection_ui, "list")
      expect_type(output$response_selection_ui, "list")
      expect_false(display_summarise_function_ui())
      expect_type(feature_values_tbl(), "list")
      expect_named(
        feature_values_tbl(),
        c(
          "sample_name",
          "feature_name",
          "feature_display",
          "feature_value",
          "feature_order",
          "group_name",
          "group_description",
          "group_color"
        )
      )
      expect_type(response_values_tbl(), "list")
      expect_named(
        response_values_tbl(),
        c("sample_name", "response_name", "response_display", "response_value")
      )
      expect_type(summarise_function, "closure")
      expect_type(heatmap_tibble(), "list")
      expect_named(heatmap_tibble(), c('feature', 'C1', 'C2', 'C3', 'C4', 'C6'))
      expect_type(heatmap_matrix(), "double")
      expect_type(summarise_function(), "closure")
      expect_equal(heatmap_source_name(), "proxy1-heatmap")
      expect_type(output$heatmap, "character")

      expect_type(heatmap_eventdata(), "list")
      expect_named(
        heatmap_eventdata(),
        c("curveNumber", "pointNumber", "x", "y", "z")
      )
      expect_named(group_data(), c("group_name", "group_description"))
      expect_equal(selected_feature(), "TCR Richness")
      expect_equal(selected_group(), "C4")
      expect_equal(response_feature(), "Age At Diagnosis")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c("sample_name", "group_name", "TCR Richness", "Age At Diagnosis")
      )
      expect_true(nrow(scatterplot_data()) > 0)

      res <- session$getReturned()
      scatterplot_data <- res$scatterplot_data()
      expect_type(scatterplot_data, "list")
      expect_named(scatterplot_data, c("x", "y", "text"))
      heatmap_data <- res$heatmap_data()
      expect_type(heatmap_data, "list")
      expect_named(
        heatmap_data,
        c('feature', 'C1', 'C2', 'C3', 'C4', 'C6')
      )
    }
  )
})

test_that("heatmap_server_multiple_summarise_functions", {

  shiny::testServer(
    heatmap_server,
    args = list(
      "feature_classes" = shiny::reactive(get_pcawg_feature_class_list()),
      "response_features" = shiny::reactive(get_pcawg_feature_list()),
      "feature_data_function" = shiny::reactive(get_pcawg_feature_values_by_class),
      "response_data_function" = shiny::reactive(get_pcawg_feature_values_by_feature),
      "summarise_function_list" = shiny::reactive(
        list(
          "Pearson" = purrr::partial(stats::cor, method = "pearson"),
          "Spearman" = purrr::partial(stats::cor, method = "spearman")
        )
      ),
      "drilldown" = shiny::reactive(T)
    ),
    {
      session$setInputs("feature_class_choice" = "Adaptive Receptor - T cell")
      session$setInputs("response_feature_choice" = "age_at_diagnosis")
      session$setInputs("summarise_function_choice" = "Spearman")
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "C4",
        "y" = "TCR Richness",
        "z" = "0.1805093"
      ))

      expect_null(default_class())
      expect_equal(default_class2(), "Adaptive Receptor - T cell")

      expect_null(default_feature())
      expect_equal(default_feature2(), "TCR_Evenness")

      expect_type(output$class_selection_ui, "list")
      expect_type(output$response_selection_ui, "list")
      expect_true(display_summarise_function_ui())
      expect_type(output$summarise_function_ui, "list")

      expect_type(feature_values_tbl(), "list")
      expect_named(
        feature_values_tbl(),
        c(
          "sample_name",
          "feature_name",
          "feature_display",
          "feature_value",
          "feature_order",
          "group_name",
          "group_description",
          "group_color"
        )
      )
      expect_type(response_values_tbl(), "list")
      expect_named(
        response_values_tbl(),
        c("sample_name", "response_name", "response_display", "response_value")
      )
      expect_type(summarise_function, "closure")
      expect_type(heatmap_matrix(), "double")
      expect_type(summarise_function(), "closure")
      expect_equal(heatmap_source_name(), "proxy1-heatmap")
      expect_type(output$heatmap, "character")

      expect_type(heatmap_eventdata(), "list")
      expect_named(
        heatmap_eventdata(),
        c("curveNumber", "pointNumber", "x", "y", "z")
      )
      expect_named(group_data(), c("group_name", "group_description"))
      expect_equal(selected_feature(), "TCR Richness")
      expect_equal(selected_group(), "C4")
      expect_equal(response_feature(), "Age At Diagnosis")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c("sample_name", "group_name", "TCR Richness", "Age At Diagnosis")
      )

      res <- session$getReturned()
      scatterplot_data <- res$scatterplot_data()
      expect_type(scatterplot_data, "list")
      expect_named(scatterplot_data, c("x", "y", "text"))
      heatmap_data <- res$heatmap_data()
      expect_type(heatmap_data, "list")
      expect_named(heatmap_data, c('feature', 'C1', 'C2', 'C3', 'C4', 'C6'))
    }
  )
})


test_that("heatmap_server_error_default_class_and_feature", {

  shiny::testServer(
    heatmap_server,
    args = list(
      "feature_classes" = shiny::reactive(get_pcawg_feature_class_list()),
      "response_features" = shiny::reactive(get_pcawg_feature_list()),
      "feature_data_function" = shiny::reactive(get_feature_values_by_class_no_data),
      "response_data_function" = shiny::reactive(get_pcawg_feature_values_by_feature),
      "summarise_function_list" = shiny::reactive(
        purrr::partial(stats::cor, method = "pearson")
      ),
      "default_feature" = shiny::reactive("T_cells_gamma_delta"),
      "default_class" = shiny::reactive("MCPcounter")
    ),
    {
      expect_equal(default_class(), "MCPcounter")
      expect_equal(default_class2(), "MCPcounter")

      expect_equal(default_feature(), "T_cells_gamma_delta")
      expect_equal(default_feature2(), "T_cells_gamma_delta")
    }
  )
})


test_that("heatmap_server_error_no_feature_data", {

  shiny::testServer(
    heatmap_server,
    args = list(
      "feature_classes" = shiny::reactive(get_pcawg_feature_class_list()),
      "response_features" = shiny::reactive(get_pcawg_feature_list()),
      "feature_data_function" = shiny::reactive(get_feature_values_by_class_no_data),
      "response_data_function" = shiny::reactive(get_pcawg_feature_values_by_feature),
      "summarise_function_list" = shiny::reactive(
        purrr::partial(stats::cor, method = "pearson")
      )
    ),
    {
      session$setInputs("feature_class_choice" = "Adaptive Receptor - T cell")
      expect_error(
        feature_values_tbl(),
        regexp = "Feature class choice did not produce any data, please select a different one."
      )
    }
  )
})


test_that("heatmap_server_error_no_response_data", {

  shiny::testServer(
    heatmap_server,
    args = list(
      "feature_classes" = shiny::reactive(get_pcawg_feature_class_list()),
      "response_features" = shiny::reactive(get_pcawg_feature_list()),
      "feature_data_function" = shiny::reactive(get_pcawg_feature_values_by_class),
      "response_data_function" = shiny::reactive(get_feature_values_by_feature_no_data),
      "summarise_function_list" = shiny::reactive(
        purrr::partial(stats::cor, method = "pearson")
      )
    ),
    {
      session$setInputs("response_feature_choice" = "age_at_diagnosis")
      expect_error(
        response_values_tbl(),
        regexp = "Response feature choice did not produce any data, please select a different one."
      )
    }
  )
})


test_that("heatmap_server_chosen_features_are_equal", {

  shiny::testServer(
    heatmap_server,
    args = list(
      "feature_classes" = shiny::reactive(get_pcawg_feature_class_list()),
      "response_features" = shiny::reactive(get_pcawg_feature_list()),
      "feature_data_function" = shiny::reactive(get_pcawg_feature_values_by_class),
      "response_data_function" = shiny::reactive(get_pcawg_feature_values_by_feature),
      "summarise_function_list" = shiny::reactive(
        purrr::partial(stats::cor, method = "pearson")
      ),
      "drilldown" = shiny::reactive(T)
    ),
    {
      session$setInputs("feature_class_choice" = "Adaptive Receptor - T cell")
      session$setInputs("response_feature_choice" = "TCR_Richness")
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "C4",
        "y" = "TCR Richness",
        "z" = "0.1805093"
      ))
      expect_error(
        scatterplot_data(),
        regexp = "Selected features to compare are the same, please select new features."
      )
    }
  )
})

test_that("heatmap_server_plot_updated", {

  shiny::testServer(
    heatmap_server,
    args = list(
      "feature_classes" = shiny::reactive(get_pcawg_feature_class_list()),
      "response_features" = shiny::reactive(get_pcawg_feature_list()),
      "feature_data_function" = shiny::reactive(get_pcawg_feature_values_by_class),
      "response_data_function" = shiny::reactive(get_pcawg_feature_values_by_feature),
      "summarise_function_list" = shiny::reactive(
        purrr::partial(stats::cor, method = "pearson")
      ),
      "drilldown" = shiny::reactive(T)
    ),
    {
      session$setInputs("feature_class_choice" = "Adaptive Receptor - T cell")
      session$setInputs("response_feature_choice" = "age_at_diagnosis")
      session$setInputs("mock_event_data" = data.frame(
        "curveNumber" = 0,
        "pointNumber" = 1,
        "x" = "C4",
        "y" = "Eosinophils",
        "z" = "0.1805093"
      ))

      expect_error(
        scatterplot_data(),
        regexp = "Plot has been updated, please click on plot."
      )
    }
  )
})

