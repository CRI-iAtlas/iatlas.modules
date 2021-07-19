plotly_heatmap <- function(
  plot_matrix,
  source_name = NULL,
  scale_colors = F,
  legend_title = NULL,
  format_func = format_plotly
){
  zmin <- NULL
  zmax <- NULL
  if(scale_colors){
    extreme <- max(abs(min(plot_matrix)),
                   abs(max(plot_matrix)))
    zmax <- extreme
    zmin <- -extreme
  }

  p <-
    plotly::plot_ly(
      z = plot_matrix,
      x = colnames(plot_matrix),
      y = rownames(plot_matrix),
      type = "heatmap",
      source = source_name,
      colors = rev(RColorBrewer::brewer.pal(8, "RdBu")),
      colorbar = list(title = legend_title),
      zmin = zmin,
      zmax = zmax
    ) %>%
    plotly::layout(xaxis = list(tickangle = 90)) %>%
    format_func() %>%
    I
}
