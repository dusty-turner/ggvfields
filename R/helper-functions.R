#' Ensure Non-empty Data
#'
#' This function ensures that the data is non-empty. If the input data is empty, it returns a default data frame. This is useful because ggplot2 layers require a data argument, and this function allows layers to operate without explicit data.
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

#' Times Operator
#'
#' This function allows for multiplication with a custom operator.
#'
#' @param x, y Numeric values to be multiplied.
#'
#' @return The product of x and y.
#' @keywords internal
#'
times <- `*`

#' Radians to Degrees Conversion
#'
#' This function converts an angle from radians to degrees.
#'
#' @param rad A numeric value representing an angle in radians.
#' @param rotate A numeric value to rotate the angle. Default is 0.
#'
#' @return The angle in degrees.
#' @keywords internal
#'

rad2deg <- function(rad, rotate = 0) ((rad + rotate) * 360/(2*pi)) %% 360
