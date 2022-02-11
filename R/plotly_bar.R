#' Plotly Bar
#'
#' @param plot_data A dataframe
#' @param x_col A string
#' @param y_col A string
#' @param error_col A string or NA
#' @param color_col A string or NA
#' @param key_col A string or NA
#' @param text_col A string or NA
#' @param xlab A string
#' @param ylab A string
#' @param title A string
#' @param source_name A string or NULL
#' @param bar_colors A string or NULL
#' @param format_func A function
#'
#' @importFrom magrittr %>%
#'
#' @export
plotly_bar <- function(
  plot_data,
  x_col = "x",
  y_col = "y",
  error_col = NA,
  color_col = x_col,
  key_col = x_col,
  text_col = x_col,
  xlab = "",
  ylab = "",
  title = "",
  source_name = NULL,
  bar_colors = NULL,
  format_func = format_plotly
) {

  select_cols <- c(
    "x"     = x_col,
    "y"     = y_col,
    "color" = color_col,
    "key"   = key_col,
    "text"  = text_col
  )

  if(is.na(error_col)){
    error_y_list <- NULL
  } else {
    select_cols <- c(select_cols, "error" = error_col)
    error_y_list <- list(
      array = ~error,
      color = "black",
      thickness = 1
    )
  }

  plot_data <- dplyr::select(plot_data, dplyr::all_of(select_cols))

  if (is.null(bar_colors)) {
    bar_colors <- plot_data %>%
      dplyr::select("color") %>%
      dplyr::n_distinct() %>%
      viridis::viridis_pal(option = "D")()
  }

  p <- plotly::plot_ly(
    plot_data,
    x = ~x,
    y = ~y,
    color = ~color,
    text = ~text,
    textposition = 'none',
    key = ~key,
    type = "bar",
    source = source_name,
    colors = bar_colors,
    error_y = error_y_list,
    hoverinfo = "text"
  ) %>%
    plotly::layout(
      legend = list(orientation = "h", x = 0, y = 1),
      title = title,
      xaxis = list(title = xlab),
      yaxis = list(title = ylab)
    )
  if(!is.null(format_func)){
    p <- format_func(p)
  }
  return(p)
}
