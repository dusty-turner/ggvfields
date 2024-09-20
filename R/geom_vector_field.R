#' Create a Vector Field Plot Layer
#'
#' `geom_vector_field` generates a vector field plot layer using a
#' user-defined function to compute the vector displacements (`dx`, `dy`) at each grid
#' point. The function automatically generates a grid of points
#' (specified by `xlim` and `ylim`) and evaluates the vector field displacements
#' at those points.
#'
#' The user provides a function that takes a vector `(x, y)` and returns the vector
#' **displacements** `(dx, dy)` at that point in the vector field. The layer automatically
#' computes calculus measures such as divergence, curl, and vector norm that can be mapped to
#' aesthetics using `after_stat()`.
#'
#' @inheritParams ggplot2::geom_raster
#' @inheritParams ggplot2::stat_identity
#' @param fun A user-defined function that takes a vector (x, y) and returns a vector (dx, dy),
#'   representing the displacements at that point in the vector field.
#' @param xlim,ylim Numeric vectors of length 2 giving the x/y-axis limits for the grid.
#' @param n Integer specifying the number of grid points along each axis (resolution of the grid).
#' @param center Logical; if `TRUE`, centers the vectors on their respective grid points.
#' @param normalize Logical; if `TRUE`, normalizes the vectors to unit length.
#' @param arrow Arrow specification, as created by `grid::arrow()`, to add arrowheads to vectors.
#' @param ... Other arguments passed to `layer()`, such as aesthetic mappings.
#'
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a
#'   vector field plot. The layer includes optional calculations of:
#'
#' ### Curl
#' The curl of a vector field represents the rotation or "twisting" of the vectors around a point.
#'
#' $$
#' \text{curl}(\mathbf{f})(x, y) = f_2^x(x, y) - f_1^y(x, y)
#' $$
#' where \( f_1^y(x, y) \) is the partial derivative of the first component with respect to \( y \), and \( f_2^x(x, y) \) is the partial derivative of the second component with respect to \( x \).
#'
#' ### Divergence
#' The divergence of a vector field measures the rate at which vectors are "spreading out" from a point. This property helps visualize sources and sinks within the field, like areas of compression or expansion in fluid flow.
#'
#' $$
#' \text{div}(\mathbf{f})(x, y) = f_1^x(x, y) + f_2^y(x, y)
#' $$
#' where \( f_1^x(x, y) \) is the partial derivative of the first component with respect to \( x \), and \( f_2^y(x, y) \) is the partial derivative of the second component with respect to \( y \).
#'
#' ### Norm
#' The norm of a vector represents its magnitude (or length):
#'
#' $$
#' \|\mathbf{f}(x, y)\| = \sqrt{dx^2 + dy^2}
#' $$
#' where \( dx \) and \( dy \) are the displacements in the x and y directions, respectively.
#'
#' @section Aesthetic mappings:
#' The following aesthetics can be mapped using `after_stat()`:
#' - `divergence`: Divergence of the vector field at each point.
#' - `curl`: Curl of the vector field at each point.
#' - `norm`: Norm (magnitude) of the vector at each point.
#'
#' For example, to map `norm` to color, you can use:
#'
#' ```
#' aes(color = after_stat(norm))
#' ```
#'
#' @examples
#'
#' # Example user-defined vector field function
#' f <- function(v) {
#'   x <- v[1]; y <- v[2]
#'   c(x + y, y - x)  # Return displacements (dx, dy)
#' }
#'
#' # Create a ggplot with the vector field layer
#' ggplot() +
#'   geom_vector_field(fun = f, xlim = c(-5, 5), ylim = c(-5, 5), n = 20)
#'
#' # Example of mapping norm to length
#' ggplot() +
#'   geom_vector_field(fun = f, xlim = c(-5, 5), ylim = c(-5, 5),
#'                     n = 20, normalize = FALSE,
#'                     aes(length = after_stat(norm),
#'                         color = after_stat(curl)))
#'
#' @export

geom_vector_field <- function(mapping = NULL, data = NULL, stat = StatVector,
                              geom = GeomVector, position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE, fun,
                              xlim = c(-10, 10), ylim = c(-10, 10), n = 16,
                              center = TRUE, normalize = TRUE,
                              arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                              ...) {

  # Create a grid of points based on xlim, ylim, and n, then convert to matrix
  grid <- expand.grid(
    x = seq(xlim[1], xlim[2], length.out = n),
    y = seq(ylim[1], ylim[2], length.out = n)
  ) |> as.matrix()

  # Apply the user-defined vectorized function to the entire grid
  vectors <- vectorize(fun)(grid)

  # Split the vectors into dx and dy components
  grid <- as.data.frame(grid)  # Convert back to data frame for compatibility with ggplot2
  grid$dx <- vectors[, 1]
  grid$dy <- vectors[, 2]

  # Calculate calculus measures: Gradient, Divergence, Curl
  grad <- apply(grid[, c("x", "y")], 1, function(v) numDeriv::grad(fun, v)) |> t()
  grad_u <- grad[, 1]
  grad_v <- grad[, 2]

  # Divergence: sum of partial derivatives of dx and dy
  grid$divergence <- grad_u + grad_v

  # Curl: difference between partial derivatives
  grid$curl <- grad_v - grad_u

  # Ensure default mappings for x, y, dx, dy, if not provided
  if (is.null(mapping)) {
    mapping <- aes(x = x, y = y, dx = dx, dy = dy)
  } else {
    # Add dx and dy to the mapping if not already present
    mapping <- modifyList(mapping, aes(x = x, y = y, dx = dx, dy = dy))
  }

  # Return the layer that will handle plotting via geom_vector and stat_vector
  layer(
    geom = geom,
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

