#' Generate a Hexagonal Lattice
#'
#' This function generates a hexagonal lattice of points within the given x and y limits,
#' using a specified hexagon diameter.
#'
#' @param xlim A numeric vector of length 2 specifying the x-axis limits.
#' @param ylim A numeric vector of length 2 specifying the y-axis limits.
#' @param d A numeric value specifying the hexagon diameter.
#'
#' @return A data frame with two columns, `x` and `y`, containing the coordinates of the hexagonal grid points.
#' @examples
#' xlim <- c(-10, 10)
#' ylim <- c(-10, 10)
#' d <- 0.5
#' hex_lattice <- generate_hexagonal_lattice(xlim, ylim, d)
#' @export
generate_hexagonal_lattice <- function(xlim, ylim, d) {
  # Compute hexagonal basis vectors
  T <- matrix(c(1, 1/2, 0, sqrt(3)/2), nrow=2, byrow=TRUE)

  # Dynamically determine buffer based on xlim, ylim, and d
  x_span <- abs(xlim[2] - xlim[1])
  y_span <- abs(ylim[2] - ylim[1])
  buffer <- ceiling(max(x_span, y_span) / d)  # Adaptive buffer

  m_range <- seq(floor(xlim[1] / d) - buffer, ceiling(xlim[2] / d) + buffer, by = 1)
  n_range <- seq(floor(ylim[1] / d) - buffer, ceiling(ylim[2] / d) + buffer, by = 1)

  # Create a larger integer grid
  grid <- expand.grid(m = m_range, n = n_range)

  # Apply affine transformation
  hex_points <- as.data.frame(t(T %*% t(as.matrix(grid))))
  colnames(hex_points) <- c("x", "y")

  # Scale by d
  hex_points <- hex_points * d

  # Strict rectangular clipping to match xlim and ylim exactly
  hex_points <- hex_points[hex_points$x >= xlim[1] & hex_points$x <= xlim[2] &
                             hex_points$y >= ylim[1] & hex_points$y <= ylim[2], ]

  hex_points
}
