#' Generate a Hexagonal Lattice
#'
#' This function generates a hexagonal lattice of points within the given x and
#' y limits, using a specified hexagon diameter. The diameter is 2 times the
#' distance between adjacent x (and y) values, see examples.
#'
#' @param xlim A numeric vector of length 2 specifying the x-axis limits.
#' @param ylim A numeric vector of length 2 specifying the y-axis limits.
#' @param d A numeric value specifying the hexagon diameter.
#' @export
#'
#' @return A data frame with two columns, `x` and `y`, containing the
#'   coordinates of the hexagonal grid points.
#'
#' @examples
#'
#' xlim <- c(-1, 1)
#' ylim <- c(-1, 0)
#'
#' grid <- grid_hex(xlim, ylim, .25)
#'
#' head( grid )
#' str( grid )
#' plot( grid, asp = 1 )
#'
#' diff(sort(unique(grid$x)))
#'
#'
grid_hex <- function(xlim, ylim, d) {

  # compute hexagonal basis vectors
  T <- matrix(c(1, 1/2, 0, sqrt(3)/2), nrow = 2, byrow = TRUE)

  # assure sorted xlim, ylim
  xlim <- sort(xlim); xmin <- xlim[1]; xmax <- xlim[2]
  ylim <- sort(ylim); ymin <- ylim[1]; ymax <- ylim[2]

  # dynamically determine buffer based on xlim, ylim, and d
  x_span <- diff(xlim)
  y_span <- diff(ylim)
  buffer <- ceiling( max(x_span, y_span) / d )  # adaptive buffer

  m_range <- seq(floor(xlim[1] / d) - buffer, ceiling(xlim[2] / d) + buffer, by = 1)
  n_range <- seq(floor(ylim[1] / d) - buffer, ceiling(ylim[2] / d) + buffer, by = 1)

  # create a larger integer grid
  grid <- expand.grid(m = m_range, n = n_range)

  # apply affine transformation
  hex_points <- as.data.frame(t(T %*% t(as.matrix(grid))))
  colnames(hex_points) <- c("x", "y")

  # scale by d
  hex_points <- hex_points * d

  # strict rectangular clipping to match xlim and ylim exactly
  hex_points <- subset( hex_points, xmin <= x & x <= xmax & ymin <= y & y <= ymax )

  # wipe row names and return
  row.names(hex_points) <- NULL
  hex_points
}
