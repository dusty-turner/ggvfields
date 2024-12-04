#' Create a Gradient Field Plot Layer
#'
#' `geom_gradient_field` generates a vector field plot layer by computing the gradient
#' of a user-defined scalar function at each grid point. The gradient is calculated
#' using `numDeriv::grad()` and then visualized as vectors on the plot.
#'
#' This function is a wrapper around `geom_vector_field`, inheriting all its functionalities
#' and capabilities. By converting a scalar field into its gradient vector field, you can
#' leverage features such as divergence, curl, and norm calculations provided by
#' `geom_vector_field`.
#'
#' The function automatically generates a grid of points (specified by `xlim`
#' and `ylim`) and evaluates the gradient at those points.
#'
#' **Default Behavior**:
#' - The **magnitude of each gradient vector (`norm`) is mapped to the `color` aesthetic** by default.
#' - **Vector lengths** are scaled to **90% of the grid spacing**.
#' - **Vectors are normalized to unit length** before scaling by grid spacing.
#'   - **To see the original lengths of the vectors, set `normalize = FALSE`.**
#' - **Arrowheads** are included by default to indicate direction.
#'
#' @inheritParams ggplot2::geom_raster
#' @inheritParams ggplot2::stat_identity
#' @importFrom numDeriv grad
#' @param fun A user-defined scalar function that takes a numeric vector `(x, y)`
#'   and returns a single numeric value, representing the scalar field.
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
#' @param ... Other arguments passed to `geom_vector_field()`, such as aesthetic mappings.
#'
#' @return A `ggplot2` layer that can be added to a ggplot object to create a
#'   gradient field plot. Since `geom_gradient_field` is a wrapper for `geom_vector_field`,
#'   **all functionalities of `geom_vector_field` are available**. This includes the computation
#'   and mapping of the following mathematical measures for the resulting gradient vector field:
#'
#' ### Mathematical Measures:
#' - **Curl**: Represents the rotation or "twisting" of vectors around a point:
#'   \deqn{\text{curl}(\mathbf{f})(x, y) = \frac{\partial f_2}{\partial x} - \frac{\partial f_1}{\partial y}}
#' - **Divergence**: Measures the rate at which vectors "spread out" from a point:
#'   \deqn{\text{div}(\mathbf{f})(x, y) = \frac{\partial f_1}{\partial x} + \frac{\partial f_2}{\partial y}}
#' - **Norm**: The magnitude of a vector:
#'   \deqn{\|\mathbf{f}(x, y)\| = \sqrt{dx^2 + dy^2}}
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
#' library(ggplot2)
#'
#' # Define a scalar function
#' paraboloid_field <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   x^2 + y^2
#' }
#'
#' saddle_field <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   x^3 - 3 * x * y^2
#' }
#'
#' # Create a ggplot with the gradient field layer
#' ggplot() +
#'   geom_gradient_field(fun = paraboloid_field)
#'
#' ggplot() +
#'   geom_gradient_field(fun = saddle_field)
#'
#' @export
geom_gradient_field <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    geom = "vector",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    fun,
    xlim = c(-10, 10),
    ylim = c(-10, 10),
    n = 16,
    center = TRUE,
    normalize = TRUE,
    arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
    ...
) {
  if (missing(fun) || !is.function(fun)) {
    stop("Please provide a valid scalar function 'fun' that takes a numeric vector (x, y) and returns a single numeric value.")
  }

  # Define a gradient function using numDeriv::grad
  gradient_fun <- function(v) {
    # Ensure that v is a numeric vector of length 2
    if (!is.numeric(v) || length(v) != 2) {
      stop("Input to the gradient function must be a numeric vector of length 2 (x, y).")
    }
    grad_val <- numDeriv::grad(func = fun, x = v)
    return(grad_val)
  }

  # Pass the gradient function to geom_vector_field
  geom_vector_field(
    mapping = mapping,
    data = data,
    stat = stat,
    geom = geom,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    fun = gradient_fun,
    xlim = xlim,
    ylim = ylim,
    n = n,
    center = center,
    normalize = normalize,
    arrow = arrow,
    ...
  )
}


#' @rdname geom_gradient_field
#' @export
stat_gradient_field <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    geom = "vector",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    fun,
    xlim = c(-10, 10),
    ylim = c(-10, 10),
    n = 16,
    center = TRUE,
    normalize = TRUE,
    arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
    ...
) {

  geom_gradient_field(
    mapping = mapping,
    data = data,
    stat = stat,
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
