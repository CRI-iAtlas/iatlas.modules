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
#' @param tbl A Tibble with columns "feature_value", "group_display"
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @importFrom stats median
#'
#' @export
refactor_by_tbl_value_column <- function(tbl, reorder_method = "None"){
  if(reorder_method == "None") {
    tbl <- tbl %>%
      dplyr::mutate(
        "group_display" = factor(.data$group_display)
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
      dplyr::group_by(.data$group_display) %>%
      dplyr::summarise(
        "feature_value" = reorder_method(.data$feature_value), .groups = "drop"
      ) %>%
      dplyr::arrange(.data$feature_value) %>%
      dplyr::pull("group_display")

    tbl <- tbl %>%
      dplyr::mutate(
        "group_display" = factor(.data$group_display, levels = new_levels)
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

validate_data <- function(
  data,
  required_columns,
  table_name,
  table_key = NULL,
  optional_columns = NULL
){

  validate_data_columns(data, required_columns, table_name)
  validate_data_key(data, table_name, table_key)

  if(is.null(optional_columns)) columns <- required_columns
  else columns <- c(required_columns, optional_columns)
  dplyr::select(data, dplyr::any_of(columns))
}

validate_data_columns <- function(data, columns, table_name){
  if(!all(columns %in% colnames(data))) {
    msg <- stringr::str_c(
      "Columns in ",
      table_name,
      "(",
      sort(stringr::str_c(colnames(data), collapse = ", ")),
      ") missing one or more of (",
      sort(stringr::str_c(columns, collapse = ", ")),
      ")."
    )
    stop(msg)
  }
}

validate_data_key <- function(data, table_name, table_key){
  if(!is.null(table_key)){
    if(nrow(data) > length(unique(data[[table_key]]))){
      msg <- stringr::str_c(
        "Values in ",
        table_name,
        " column ",
        table_key,
        " are not unique."
      )
      stop(msg)
    }
  }
}

validate_sample_data <- purrr::partial(
  .f = validate_data,
  required_columns = c(
    "sample_name", "feature_name", "group_name", "dataset_name", "feature_value"
  ),
  table_name = "sample_data"
)

validate_feature_data <- function(
  data,
  required_columns = c("feature_name"),
  table_name = "feature_data",
  table_key = "feature_name",
  optional_columns = c("feature_display", "feature_class", "feature_order")
){
  data <- validate_data(
    data,
    required_columns,
    table_name,
    table_key,
    optional_columns
  )

  add_display_column <- all(
    "feature_display" %in% optional_columns,
    !"feature_display" %in% colnames(data)
  )
  if(add_display_column){
    data <- dplyr::mutate(data, "feature_display" = .data$feature_name)
  }

  add_order_column <- all(
    "feature_order" %in% optional_columns,
    !"feature_order" %in% colnames(data)
  )
  if(add_order_column){
    data <- dplyr::mutate(data, "feature_order" = NA_integer_)
  }
  return(data)
}

validate_group_data <- function(
  data,
  required_columns = c("group_name"),
  table_name = "group_data",
  table_key = "group_name",
  optional_columns = c("group_display", "group_color", "group_description")
){
  data <- validate_data(
    data,
    required_columns,
    table_name,
    table_key,
    optional_columns
  )

  add_display_column <- all(
    "group_display" %in% optional_columns,
    !"group_display" %in% colnames(data)
  )
  if(add_display_column){
    data <- dplyr::mutate(data, "group_display" = .data$group_name)
  }

  add_color_column <- all(
    "group_color" %in% optional_columns,
    !"group_color" %in% colnames(data)
  )
  if(add_color_column){
    data <- dplyr::mutate(data, "group_color" = NA_character_)
  }

  add_description_column <- all(
    "group_description" %in% optional_columns,
    !"group_description" %in% colnames(data)
  )
  if(add_description_column){
    data <- dplyr::mutate(data, "group_description" = "")
  }
  return(data)
}

validate_dataset_data <- function(
  data,
  required_columns = c("dataset_name", "dataset_display"),
  table_name = "dataset_data",
  table_key = "dataset_name",
  optional_columns = c()
){
  data <- validate_data(
    data,
    required_columns,
    table_name,
    table_key,
    optional_columns
  )
}

