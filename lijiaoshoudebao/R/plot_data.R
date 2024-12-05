#' Plot Data
#'
#' This function creates a scatter plot with a regression line.
#'
#' @param data A data frame containing the variables to plot.
#' @param x The variable for the x-axis.
#' @param y The variable for the y-axis.
#' @return A ggplot object representing the scatter plot with a regression line.
#' @export
plot_data <- function(data, x, y) {
  library(ggplot2)

  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    theme_minimal() +
    labs(title = paste("Scatter plot of", y, "vs", x), x = x, y = y)

  return(p)
}
