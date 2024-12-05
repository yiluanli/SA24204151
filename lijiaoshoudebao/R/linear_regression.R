#' Linear Regression Analysis
#'
#' This function performs linear regression analysis.
#'
#' @param data A data frame containing the variables for regression.
#' @param formula A formula object (e.g., y ~ x1 + x2).
#' @return A summary of the linear regression model.
#' @export
linear_regression <- function(data, formula) {
  # Fit the linear model
  model <- lm(formula, data = data)

  # Return the model summary
  return(summary(model))
}
