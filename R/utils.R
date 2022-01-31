utils::globalVariables(".")

# feature transforms ----------------------------------------------------------

#' Transform Feature String
#'
#' @param feature A string, the name of the feature
#' @param transformation A string, the name of the transformation
#'
#' @export
transform_feature_string <- function(feature, transformation){
  switch(
    transformation,
    "None"       = feature,
    "Log2"       = paste("Log2(",   feature,  ")"),
    "Log2 + 1"   = paste("Log2(",   feature,  "+ 1 )"),
    "Log10"      = paste("Log10(",  feature,  ")"),
    "Log10 + 1"  = paste("Log10(",  feature,  "+ 1 )"),
    "Squared"    = paste0(feature, "**2"),
    "Reciprocal" = paste0("1/", feature)
  )
}

#' Transform Feature Formula
#'
#' @param feature A string, the name of the feature
#' @param transformation A string, the name of the transformation
#'
#' @export
transform_feature_formula <- function(feature, transformation){
  switch(
    transformation,
    "None"       = feature,
    "Squared"    = paste0("I(",       feature, "**2)"),
    "Log10"      = paste0("I(log10(", feature, "))"),
    "Reciprocal" = paste0("I(1/",     feature, ")")
  )
}

#' Log Tibble Value Column
#'
#' @param tbl A Tibble with column "feature_value"
#' @param base An integer, used as the base in log
#' @param add_amt A numeric, added to the value column before logging
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
log_tbl_value_column <- function(tbl, base = 10, add_amt = 0){
  tbl %>%
    dplyr::mutate("feature_value" = .data$feature_value + add_amt) %>%
    dplyr::filter(.data$feature_value > 0) %>%
    dplyr::mutate("feature_value" = log(.data$feature_value, base))
}

#' Scale Tibble Value Column
#'
#' @param tbl A Tibble with column "feature_value"
#' @param scale_method One of "Log2", "Log2 + 1", "Log10 + 1", "Log10"
#'
#' @export
scale_tbl_value_column <- function(tbl, scale_method = "None"){
  if (scale_method %in% c("Log2", "Log2 + 1", "Log10 + 1", "Log10")) {
    add_amt <- 0
    base    <- 10
    if (scale_method %in% c("Log2", "Log2 + 1")) {
      base <- 2
    }
    if (scale_method %in% c("Log10 + 1", "Log2 + 1")) {
      add_amt <- 1
    }
    tbl <- log_tbl_value_column(tbl, base, add_amt)
  } else if (scale_method == "None") {
    tbl <- tbl
  } else {
    stop("Scale method does not exist")
  }
  return(tbl)
}

#' Refactor By Tibble Value Column
#'
#' @param reorder_method One of "None", "Median", "Mean", "Max", "Min
#' @param tbl A Tibble with columns "feature_value", "group_name"
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats median
#'
#' @export
reafctor_by_tbl_value_column <- function(tbl, reorder_method = "None"){
  if(reorder_method == "None") {
    tbl <- tbl %>%
      dplyr::mutate(
        "group_name" = factor(.data$group_name)
      )
  } else {
    reorder_method <- switch(
      reorder_method,
      "Median" = median,
      "Mean" = mean,
      "Max" = max,
      "Min" = min
    )
    new_levels <- tbl %>%
      dplyr::group_by(.data$group) %>%
      dplyr::summarise(
        "feature_value" = reorder_method(.data$feature_value), .groups = "drop"
      ) %>%
      dplyr::arrange(.data$feature_value) %>%
      dplyr::pull("group_name")
    tbl <- tbl %>%
      dplyr::mutate(
        "group_name" = factor(.data$group, levels = new_levels)
      )
  }
}

#' Summarise Tibble At Column
#
#' @param tbl A tibble
#' @param column A string, a column in the tibble
#' @param grouping_columns A vector of strings, columns in the tibble
#' @param function_names A function fun, a quosure style lambda ~ fun(.) or a
#' list of either form.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data :=
#'
#' @export
summarise_tbl_at_column <- function(
  tbl, column, grouping_columns, function_names
  ){
  assert_tbl_has_columns(tbl, c(column, grouping_columns))
  result_tbl <- tbl %>%
    dplyr::group_by_at(dplyr::vars(dplyr::one_of(grouping_columns))) %>%
    dplyr::summarise_at(column, .funs = function_names) %>%
    dplyr::ungroup()
  if(length(function_names) == 1){
    result_tbl <- dplyr::rename(result_tbl, !!function_names := column)
  }
  assert_tbl_has_columns(result_tbl, c(grouping_columns, function_names))
  assert_tbl_has_rows(result_tbl)
  return(result_tbl)
}

# Assert functions ------------------------------------------------------------

#' Assert Tibble Has Columns
#'
#' @param tbl A tibble
#' @param columns a vector of columns
#'
#' @export
assert_tbl_has_columns <- function(tbl, columns){
  missing_columns <- columns[!columns %in% colnames(tbl)]
  if (length(missing_columns) != 0) {
    stop("tbl has missing columns: ",
         paste0(missing_columns, collapse = ", "))
  }
}

#' Assert Tibble has Rows
#'
#' @param tbl A tibble
#'
#' @export
assert_tbl_has_rows <- function(tbl){
  if (nrow(tbl) == 0) {
    stop("result tbl is empty")
  }
}

# plotly text -----------------------------------------------------------------

#' Add Plotly Text
#'
#' @param tbl A tibble
#' @param title A string
#' @param name Name of a column
#' @param group Name of a column
add_plotly_text <- function(tbl, title, name, group){
  dplyr::mutate(tbl, text = paste0(
    "<b>", title, ":</b> ", {{name}}, " (", {{group}}, ")"
  ))
}

#' Add Plotly Value Text
#'
#' @param tbl A tibble with column text
#' @param cols A vector of strings that are columns in the tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
add_plotly_value_text <- function(tbl, cols){
  tbl %>%
    tidyr::pivot_longer(
      .,
      tidyselect::all_of(cols),
      names_to  = "value_name",
      values_to = "value"
    ) %>%
    dplyr::mutate(value_text = stringr::str_glue(
      "{name}: {value}",
      name = stringr::str_to_upper(.data$value_name),
      value = sprintf("%0.3f", .data$value)
    )) %>%
    dplyr::group_by(.data$text) %>%
    dplyr::mutate(value_text = paste0(
      .data$value_text,
      collapse = "</br>"
    )) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
      .,
      names_from = .data$value_name,
      values_from = .data$value
    )
}

#' Create Plotly Text
#'
#' @param tbl A tibble
#' @param name A column
#' @param group A column
#' @param cols A vector of strings, which are columns in the tibble
#' @param title A string
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
create_plotly_text <- function(tbl, name, group, cols, title){
  tbl %>%
    add_plotly_text(title, {{name}}, {{group}}) %>%
    add_plotly_value_text(tidyselect::all_of(cols)) %>%
    tidyr::unite(
      "text",
      .data$text,
      .data$value_text,
      sep = "</br></br>"
    )
}

# event data utils -------------------------------------------------------------

#' Get Values from Eventdata Dataframe
#'
#' @param eventdata Eventdata from "plotly_click" plotly::event_data
#' @param col The column to get the values from
#' @importFrom magrittr %>%
#'
#' @export
get_values_from_eventdata <- function(eventdata, col = "x"){
  eventdata %>%
    dplyr::as_tibble() %>%
    magrittr::extract2(col) %>%
    unique()
}

#' Create Group Text from Eventdata Dataframe
#'
#' @param eventdata Eventdata from "plotly_click" plotly::event_data
#' @param group_tbl A Tibble with columns group, name, characteristics
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
create_group_text_from_eventdata <- function(eventdata, group_tbl){
  selected_group <- get_values_from_eventdata(eventdata)
  group_tbl %>%
    dplyr::filter(.data$group_display == selected_group) %>%
    dplyr::pull("group_description")
}

# misc ------------------------------------------------------------------------

#' Create Nested Named List
#'
#' @param tbl A tibble with the below columns
#' @param names_col1 A column that will be the names of the top list
#' @param names_col2 A column that will be the names of the nested lists
#' @param values_col A column that will be the values of the nested lists
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#'
#' @export
create_nested_named_list <- function(
  tbl,
  names_col1 = "feature_class",
  names_col2 = "feature_display",
  values_col = "feature_name"
){
  list <- tbl %>%
    dplyr::select(tidyselect::all_of(c(
      n1 = names_col1,
      n2 = names_col2,
      v  = values_col
    ))) %>%
    tidyr::drop_na() %>%
    tidyr::nest(data = c(.data$n2, .data$v)) %>%
    dplyr::mutate(data = purrr::map(.data$data, tibble::deframe)) %>%
    tibble::deframe(.)
  return(list)
}

#' Get Unique Values from Column
#'
#' @param tbl A tibble
#' @param col A column in the tibble
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#'
#' @export
get_unique_values_from_col <- function(tbl, col){
  tbl %>%
    dplyr::select({{col}}) %>%
    tidyr::drop_na() %>%
    dplyr::distinct() %>%
    dplyr::pull({{col}})
}


#' Standard Error
#'
#' @param x A vector of numerics
#'
#' @export
se <- function(x){
  mean(x) / sqrt(length(x))
}

# system files ----------------------------------------------------------------

#' Get System Path File
#'
#' @param prefix A string, the file prefix
#' @param extension A the string, the file extension
#' @param folder A string, the file's folder
#' @param package A string, the package the file is in
#'
#' @export
get_system_path_file <- function(
  prefix, extension, folder, package = "iatlas.modules"
  ){
  file_name <- stringr::str_c(prefix, extension)
  file.path(system.file(folder, package = package), file_name)
}

get_markdown_path <- function(name, extension = ".markdown"){
  get_system_path_file(name, extension, "markdown")
}


# input validation ------------------------------------------------------------

validate_feature_data <- function(data){

  needed_col_names <- c("feature_name", "feature_display")
  col_names <- colnames(data)

  if(!all(needed_col_names %in% col_names)) {
    msg <- stringr::str_c(
      "Columns in fetaure_data (",
      stringr::str_c(col_names, collapse = ", "),
      ") missing one or more of (",
      stringr::str_c(needed_col_names, collapse = ", "),
      ")."
    )
    stop(msg)
  }

  if(nrow(data) > length(unique(data$feature_name)))
    stop("Values in feature_data$feature_name are not unique.")
}

validate_group_data <- function(data){

  needed_col_names <- c("group_name", "group_display")
  col_names <- colnames(data)

  if(!all(needed_col_names %in% col_names)) {
    msg <- stringr::str_c(
      "Columns in fetaure_data (",
      stringr::str_c(col_names, collapse = ", "),
      ") missing one or more of (",
      stringr::str_c(needed_col_names, collapse = ", "),
      ")."
    )
    stop(msg)
  }

  if(nrow(data) > length(unique(data$group_name)))
    stop("Values in group_data$group_name are not unique.")

  if(!"group_color" %in% col_names){
    data <- dplyr::mutate(data, "group_color" = NA)
  }

  if(!"group_description" %in% col_names){
    data <- dplyr::mutate(data, "group_description" = "")
  }
}

