#' Create a Vector Field Plot Layer
#'
#' `geom_vector_field` generates a vector field plot layer using a user-defined
#' function to compute the vector displacements (`dx`, `dy`) at each grid point.
#' The function automatically generates a grid of points (specified by `x_lim`
#' and `y_lim`) and evaluates the vector displacements at those points.
#'
#' The user must provide a function that takes a vector `(x, y)` and returns the
#' **displacements** `(dx, dy)` at that point in the vector field. Optionally, the
#' layer can compute **divergence**, **curl**, and **vector norm**, which can be
#' mapped to aesthetics using `after_stat()`.
#'
#' **Default Behavior**:
#' - The **magnitude of each vector (`norm`) is mapped to the `color` aesthetic** by default.
#' - **Vector lengths** are scaled to **90% of the grid spacing**.
#' - **Vectors are normalized to unit length** before scaling by grid spacing.
#'   - **To see the original lengths of the vectors, set `normalize = FALSE`.**
#' - **Arrowheads** are included by default to indicate direction.
#'
#' @inheritParams ggplot2::geom_raster
#' @inheritParams ggplot2::stat_identity
#' @importFrom boot boot
#' @param fun A user-defined function that takes a vector `(x, y)` and returns a
#'   vector `(dx, dy)`, representing the displacements at that point in the vector
#'   field.
#' @param x_lim,y_lim Numeric vectors of length 2 specifying the x/y-axis limits for
#'   the grid.
#' @param n Integer specifying the number of grid points along each axis
#'   (resolution of the grid).
#' @param center Logical; if `TRUE`, centers the vectors on their respective
#'   grid points.
#' @param normalize Logical; if `TRUE`, normalizes the vectors to unit length.
#'   Set to `FALSE` to view the original lengths of the vectors.
#' @param arrow Arrow specification, created by `grid::arrow()`, to add
#'   arrowheads to vectors.
#' @param ... Other arguments passed to `layer()`, such as aesthetic mappings.
#'
#' @return A `ggplot2` layer that can be added to a ggplot object to create a
#'   vector field plot. The following mathematical measures are available:
#'
#' ### Curl
#' The curl represents the rotation or "twisting" of vectors around a point:
#' \deqn{\text{curl}(\mathbf{f})(x, y) = \frac{\partial f_2}{\partial x} - \frac{\partial f_1}{\partial y}}
#'
#' ### Divergence
#' The divergence measures the rate at which vectors "spread out" from a point:
#' \deqn{\text{div}(\mathbf{f})(x, y) = \frac{\partial f_1}{\partial x} + \frac{\partial f_2}{\partial y}}
#'
#' ### Norm
#' The norm (magnitude) of a vector:
#' \deqn{\|\mathbf{f}(x, y)\| = \sqrt{dx^2 + dy^2}}
#'
#' @section Aesthetic mappings:
#' You can map the following measures using `after_stat()`:
#' - **`norm`**: Magnitude (norm) of the vector at each point.
#' - **`divergence`**: Divergence of the vector field at each point.
#' - **`curl`**: Curl of the vector field at each point.
#'
#' For example, to map `curl` to color:
#' ```r
#' aes(color = after_stat(curl))
#' ```
#'
#' @examples
#' # Example user-defined vector field function
#' f <- function(v) {
#'   x <- v[1]; y <- v[2]
#'   c(x + y, y - x)  # Return displacements (dx, dy)
#' }
#'
#' # Create a ggplot with the vector field layer
#' ggplot() +
#'   geom_vector_field(fun = f, x_lim = c(-5, 5), y_lim = c(-5, 5), n = 20)
#'
#' # Example of mapping norm to length
#' ggplot() +
#'   geom_vector_field(
#'     aes(length = after_stat(norm)),
#'     fun = f, x_lim = c(-5, 5), y_lim = c(-5, 5), n = 20,
#'   )
#'
#' # Example with both length and curl mapped
#' ggplot() +
#'   geom_vector_field(
#'     fun = f, x_lim = c(-5, 5), y_lim = c(-5, 5), n = 20, normalize = FALSE,
#'     mapping = aes(length = after_stat(norm), color = after_stat(curl))
#'   )
#'
#' @export
#' @rdname geom_vector_field
#' @export
geom_vector_field <- function(
    mapping = NULL,
    data = NULL,
    stat = StatVector,
    geom = GeomVector,
    position = "identity",
    center = TRUE,
    normalize = TRUE,
    tail_point = FALSE,
    tail_point.size = 2,
    arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed"),
    fun = NULL,       # User-provided function for grid-based vector field
    x_lim = NULL, # Default x limits for the grid
    y_lim = NULL, # Default y limits for the grid
    n = NULL,           # Grid resolution
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {
  # Default aesthetic mappings: Ensure color reflects norm
  default_mapping <- aes(color = after_stat(norm), length = after_stat(NA))

  # Merge user-provided mappings with defaults
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  # Pass the parameters via `params` only
  layer(
    stat = stat,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      center = center,
      normalize = normalize,
      tail_point = tail_point,
      tail_point.size = tail_point.size,
      arrow = arrow,
      fun = fun,        # Pass user-provided function
      x_lim = x_lim,    # Pass x limits
      y_lim = y_lim,    # Pass y limits
      n = n,            # Pass grid resolution
      ...
    )
  )
}




#' @rdname geom_vector_field
#' @export
stat_vector_field <- function(
    mapping = NULL,
    data = NULL,
    geom = GeomVector,
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    fun = NULL,        # User-defined function for vector fields
    x_lim = c(-10, 10), # Default x limits for the grid
    y_lim = c(-10, 10), # Default y limits for the grid
    n = 10,            # Grid resolution
    center = TRUE,
    normalize = TRUE,
    tail_point = FALSE,
    tail_point.size = 2,
    arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
    ...
) {
  # Default aesthetics: color reflects norm (magnitude) and length defaults to NA
  default_aes <- aes(color = after_stat(norm), length = after_stat(NA))

  # Merge user-provided mappings with defaults
  if (is.null(mapping)) {
    mapping <- default_aes
  } else {
    mapping <- modifyList(default_aes, mapping)
  }

  # Pass the parameters via `params` only
  layer(
    stat = StatVector,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      center = center,
      normalize = normalize,
      tail_point = tail_point,
      tail_point.size = tail_point.size,
      fun = fun,        # Pass user-provided function
      x_lim = x_lim,    # Pass x limits
      y_lim = y_lim,    # Pass y limits
      n = n,            # Pass grid resolution
      ...
    )
  )
}


utils::globalVariables(c("x", "y", "dx", "dy"))
