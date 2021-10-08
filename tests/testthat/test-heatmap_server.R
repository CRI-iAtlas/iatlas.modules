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
      session$setInputs("test_event_data" = data.frame(
        "x" = "C1", "y" = "TCR Richness"
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
      session$setInputs("test_event_data" = data.frame(
        "x" = "C1", "y" = "Eosinophils"
      ))

      expect_error(
        scatterplot_data(),
        regexp = "Plot has been updated, please click on plot."
      )
    }
  )
})


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

      expect_type(output$class_selection_ui, "list")
      expect_type(output$response_selection_ui, "list")
      expect_false(display_summarise_function_ui())
      expect_type(feature_values_tbl(), "list")
      expect_named(
        feature_values_tbl(),
        c(
          "sample",
          "feature",
          "feature_value",
          "feature_order",
          "group",
          "group_description",
          "color"
        )
      )
      expect_type(response_values_tbl(), "list")
      expect_named(
        response_values_tbl(), c("sample", "response", "response_value")
      )
      expect_type(summarise_function, "closure")
      expect_type(heatmap_matrix(), "double")
      expect_type(summarise_function(), "closure")
      expect_equal(heatmap_source_name(), "proxy1-heatmap")
      expect_type(output$heatmap, "character")
      expect_error(
        heatmap_eventdata(),
        regexp = "Click on above heatmap.",
        class = c("shiny.silent.error")
      )
      session$setInputs("test_event_data" = data.frame(
        "x" = "C1", "y" = "TCR Richness"
      ))
      expect_type(heatmap_eventdata(), "list")
      expect_named(heatmap_eventdata(), c("x", "y"))
      expect_named(group_data(), c("group", "description"))
      expect_equal(selected_feature(), "TCR Richness")
      expect_equal(selected_group(), "C1")
      expect_equal(response_feature(), "Age At Diagnosis")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c("sample", "group", "TCR Richness", "Age At Diagnosis")
      )
      expect_true(nrow(scatterplot_data()) > 0)

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

      expect_type(output$class_selection_ui, "list")
      expect_type(output$response_selection_ui, "list")
      expect_true(display_summarise_function_ui())
      expect_type(output$summarise_function_ui, "list")

      expect_type(feature_values_tbl(), "list")
      expect_named(
        feature_values_tbl(),
        c(
          "sample",
          "feature",
          "feature_value",
          "feature_order",
          "group",
          "group_description",
          "color"
        )
      )
      expect_type(response_values_tbl(), "list")
      expect_named(
        response_values_tbl(), c("sample", "response", "response_value")
      )
      expect_type(summarise_function, "closure")
      expect_type(heatmap_matrix(), "double")
      expect_type(summarise_function(), "closure")
      expect_equal(heatmap_source_name(), "proxy1-heatmap")
      expect_type(output$heatmap, "character")
      expect_error(
        heatmap_eventdata(),
        regexp = "Click on above heatmap.",
        class = c("shiny.silent.error")
      )
      session$setInputs("test_event_data" = data.frame(
        "x" = "C1", "y" = "TCR Richness"
      ))
      expect_type(heatmap_eventdata(), "list")
      expect_named(heatmap_eventdata(), c("x", "y"))
      expect_named(group_data(), c("group", "description"))
      expect_equal(selected_feature(), "TCR Richness")
      expect_equal(selected_group(), "C1")
      expect_equal(response_feature(), "Age At Diagnosis")
      expect_type(scatterplot_data(), "list")
      expect_named(
        scatterplot_data(),
        c("sample", "group", "TCR Richness", "Age At Diagnosis")
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

