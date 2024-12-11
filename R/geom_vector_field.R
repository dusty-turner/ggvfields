#' Create a Vector Field Plot Layer
#'
#' `geom_vector_field` generates a vector field plot layer using a user-defined
#' function to compute the vector displacements (`dx`, `dy`) at each grid point.
#' The function automatically generates a grid of points (specified by `xlim`
#' and `ylim`) and evaluates the vector displacements at those points.
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
#' @param xlim,ylim Numeric vectors of length 2 specifying the x/y-axis limits for
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
#'   geom_vector_field(fun = f, xlim = c(-5, 5), ylim = c(-5, 5), n = 20)
#'
#' # Example of mapping norm to length
#' ggplot() +
#'   geom_vector_field(
#'     aes(length = after_stat(norm)),
#'     fun = f, xlim = c(-5, 5), ylim = c(-5, 5), n = 20,
#'   )
#'
#' # Example with both length and curl mapped
#' ggplot() +
#'   geom_vector_field(
#'     fun = f, xlim = c(-5, 5), ylim = c(-5, 5), n = 20, normalize = FALSE,
#'     mapping = aes(length = after_stat(norm), color = after_stat(curl))
#'   )
#'
#' @export
geom_vector_field <- function(
  mapping = NULL,
  data = NULL,
  stat = StatVector,
  geom = GeomVector,
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE, fun,
  xlim = NULL,
  ylim = NULL, n = 16,
  center = TRUE,
  normalize = TRUE,
  arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
  ...
) {

  if(is.null(xlim)){
    xlim <- c(-10,10)
    message("xlim not provided. Defaulting to c(-10,10)")
  }
  if(is.null(ylim)){
    ylim <- c(-10,10)
    message("ylim not provided. Defaulting to c(-10,10)")
  }

  # Create a grid of points based on xlim, ylim, and n
  grid <- expand.grid(
    x = seq(xlim[1], xlim[2], length.out = n),
    y = seq(ylim[1], ylim[2], length.out = n)
  )

  # Apply the user-defined vectorized function to the entire grid
  vectors <- vectorize(fun)(as.matrix(grid))

  # Split the vectors into dx and dy components
  grid$dx <- vectors[, 1]
  grid$dy <- vectors[, 2]

  # Ensure default mappings for x, y, dx, dy, length, if not provided
  if (is.null(mapping)) {
    mapping <- aes(x = x, y = y, dx = dx, dy = dy)
  } else {
    # Add dx and dy to the mapping if not already present
    mapping <- modifyList(mapping, aes(x = x, y = y, dx = dx, dy = dy))
  }

  # Pass the grid and function along to geom_vector for further processing
  geom_vector(
    mapping = mapping,
    data = grid,
    stat = stat,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    fun = fun,
    center = center,
    normalize = normalize,
    arrow = arrow,
    ...
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
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 16,
  center = TRUE,
  normalize = TRUE,
  arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
  ...
) {
  geom_vector_field(
    mapping = mapping,
    data = data,
    stat = StatVector,
    geom = geom,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    fun = fun,
    xlim = xlim,
    ylim = ylim,
    n = n,
    center = center,
    normalize = normalize,
    arrow = arrow,
    ...
  )
}

utils::globalVariables(c("x", "y", "dx", "dy"))
