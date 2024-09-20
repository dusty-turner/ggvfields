#' Create a Vector Field Plot Layer
#'
#' `geom_vector_field` generates a vector field plot layer using a
#' user-defined function to compute the vector end points, which are converted
#' into displacements (`dx`, `dy`) to be used by `geom_vector`. This is useful
#' for visualizing vector fields in a two-dimensional space.
#' @inheritParams ggplot2::geom_raster
#' @inheritParams ggplot2::stat_identity
#' @param fun A user-defined function that takes a vector (x, y) and returns a vector (xend, yend),
#'   the end points of the vector field at that point.
#' @param xlim,ylim Numeric vectors of length 2 giving the x/y-axis limits for the grid.
#' @param n Integer specifying the number of grid points along each axis (resolution of the grid).
#' @param center Logical; if `TRUE`, centers the vectors on their respective grid points.
#' @param normalize Logical; if `TRUE`, normalizes the vectors to unit length.
#' @param arrow Arrow specification, as created by `grid::arrow()`.
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a
#'   vector field plot.
#' @name geom_vector_field
#' @rdname geom_vector_field
#' @examples
#'
#' # example user-defined function
#' f <- function(v) {
#'   x <- v[1]; y <- v[2]
#'   c(x + y, y - x)  # return end points
#' }
#'
#' # create a ggplot with the vector field layer
#'
#' @export
geom_vector_field <- function(mapping = NULL, data = NULL, stat = StatVector,
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, fun, xlim = c(-10, 10), ylim = c(-10, 10),
                              n = 16, center = TRUE, normalize = TRUE,
                              arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                              ...) {

  # Create a grid of points based on xlim, ylim, and n
  grid <- expand.grid(
    x = seq(xlim[1], xlim[2], length.out = n),
    y = seq(ylim[1], ylim[2], length.out = n)
  )

  # Apply the user-defined function to each grid point to get dx, dy (displacements)
  vectors <- t(apply(grid, 1, fun))

  # Assign dx and dy (the displacements) directly from the user function's output
  grid$dx <- vectors[, 1]
  grid$dy <- vectors[, 2]

  # Ensure default mappings for x, y, dx, dy if not provided
  if (is.null(mapping)) {
    mapping <- aes(x = x, y = y, dx = dx, dy = dy)
  } else {
    # Add dx, dy to the mapping if not present
    mapping <- modifyList(mapping, aes(x = x, y = y, dx = dx, dy = dy))
  }

  # Return the layer that will handle plotting via geom_vector and stat_vector
  layer(
    stat = stat,
    data = grid,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      center = center,
      normalize = normalize,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}
