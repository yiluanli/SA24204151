#' Clean Data
#'
#' This function cleans a dataset by removing rows with missing values and renaming columns.
#'
#' @param data A data frame to be cleaned.
#' @param rename_cols A named vector where names are old column names and values are new names.
#' @return A cleaned data frame.
#' @export
clean_data <- function(data, rename_cols = NULL) {
  # Remove rows with missing values
  data <- na.omit(data)

  # Rename columns if provided
  if (!is.null(rename_cols)) {
    colnames(data) <- rename_cols
  }

  return(data)
}
