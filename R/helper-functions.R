#' Ensure Non-empty Data
#'
#' This function ensures that the data is non-empty. If the input data is empty, it returns a default data frame.  ggplot2 layers require a data argument but this allows layers to operate without data.
#'
#' @param data The input data to check.
#'
#' @return A data frame. If the input data is empty, returns a default data frame with a single row.
#'
#' @keywords internal
#'
ensure_nonempty_data <- function(data) {
  if (length(data) == 0) {
    data.frame(x = 1)
  } else {
    data
  }
}
