#' Create a Gradient Field Plot Layer
#'
#' `geom_gradient_field` generates a vector field plot layer by computing the gradient
#' of a user-defined scalar function at each grid point. This function wraps
#' `geom_vector_field` and inherits its features and capabilities, while automatically
#' converting a scalar field into its gradient vector field for visualization.
#'
#' `geom_gradient_field2` is an enhanced version of `geom_gradient_field` that
#' works in conjunction with `geom_vector_field2`. It includes additional features
#' and improvements for advanced use cases, providing more flexibility and customization.
#'
#' The gradient is calculated using `numDeriv::grad()` at grid points defined by
#' the `xlim` and `ylim` parameters, with the grid resolution controlled by `n`.
#' The resulting vectors are scaled, normalized, and visualized as arrows on the plot.
#'
#' **Default Behavior**:
#' - The **magnitude of each gradient vector (`norm`) is mapped to the `color` aesthetic** by default.
#' - **Vectors are normalized to unit length** by default. Set `normalize = FALSE` to view
#'   original vector lengths.
#' - **Arrowheads** are included by default to indicate direction.
#'
#' @inheritParams geom_vector_field
#' @inheritParams geom_vector_field2
#' @importFrom numDeriv grad
#' @param fun A user-defined scalar function that takes a numeric vector `(x, y)`
#'   and returns a single numeric value, representing the scalar field.
#' @param xlim,ylim Numeric vectors of length 2 specifying the x/y-axis limits for
#'   the grid.
#' @param n Integer specifying the number of grid points along each axis
#'   (resolution of the grid).
#' @param ... Other arguments passed to `geom_vector_field` or `geom_vector_field2`.
#'
#' @return A `ggplot2` layer that can be added to a ggplot object to create a
#'   gradient field plot. Both `geom_gradient_field` and `geom_gradient_field2` wrap their
#'   respective `geom_vector_field` functions, inheriting their functionalities.
#'
#' @examples
#'
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
#' # Visualizing a paraboloid scalar field with geom_gradient_field
#' ggplot() +
#'   geom_gradient_field(fun = paraboloid_field, xlim = c(-10, 10), ylim = c(-10, 10))
#'
#' # Visualizing a paraboloid scalar field with geom_gradient_field2
#' ggplot() +
#'   geom_gradient_field2(fun = paraboloid_field, xlim = c(-10, 10), ylim = c(-10, 10))
#'
#' # Visualizing a saddle scalar field with geom_gradient_field
#' ggplot() +
#'   geom_gradient_field(fun = saddle_field, xlim = c(-10, 10), ylim = c(-10, 10))
#'
#' # Visualizing a saddle scalar field with geom_gradient_field2
#' ggplot() +
#'   geom_gradient_field2(fun = saddle_field, xlim = c(-10, 10), ylim = c(-10, 10))
#'
#'
#' @export

geom_gradient_field <- function(mapping = NULL, data = NULL,
                                stat = "identity", geom = "vector",
                                ...,
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
                                arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")
) {
  if (missing(fun) || !is.function(fun)) {
    stop("Please provide a valid scalar function 'fun' that takes a numeric vector (x, y) and returns a single numeric value.")
  }

  grad_function <- gradient_fun(fun)

  geom_vector_field(
    mapping = mapping,
    data = data,
    stat = StatVector,
    geom = GeomVector,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    fun = grad_function,
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
stat_gradient_field <- function(mapping = NULL, data = NULL,
                                geom = "vector",
                                ...,
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
                                arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")) {
  if (missing(fun) || !is.function(fun)) {
    stop("Please provide a valid scalar function 'fun' that takes a numeric vector (x, y) and returns a single numeric value.")
  }

  gradient_fun <- function(fun) {
    function(v) {
      if (!is.numeric(v) || length(v) != 2) {
        stop("Input to the gradient function must be a numeric vector of length 2 (x, y).")
      }
      numDeriv::grad(func = fun, x = v)
    }
  }

  grad_function <- gradient_fun(fun)

  stat_vector_field(
    mapping = mapping,
    data = data,
    geom = GeomVector,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    fun = grad_function,
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
geom_gradient_field2 <- function(mapping = NULL, data = NULL,
                                stat = "identity", geom = "vector",
                                ...,
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
                                arrow = NULL
) {
  if (missing(fun) || !is.function(fun)) {
    stop("Please provide a valid scalar function 'fun' that takes a numeric vector (x, y) and returns a single numeric value.")
  }

  grad_function <- gradient_fun(fun)

  geom_vector_field2(
    mapping = mapping,
    data = data,
    stat = StatVector,
    geom = GeomVector,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    fun = grad_function,
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
stat_gradient_field2 <- function(mapping = NULL, data = NULL,
                                # geom = "GeomVector",
                                geom = "vector",
                                ...,
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
                                arrow = NULL) {
  if (missing(fun) || !is.function(fun)) {
    stop("Please provide a valid scalar function 'fun' that takes a numeric vector (x, y) and returns a single numeric value.")
  }

  gradient_fun <- function(fun) {
    function(v) {
      if (!is.numeric(v) || length(v) != 2) {
        stop("Input to the gradient function must be a numeric vector of length 2 (x, y).")
      }
      numDeriv::grad(func = fun, x = v)
    }
  }

  grad_function <- gradient_fun(fun)

  stat_vector_field2(
    mapping = mapping,
    data = data,
    geom = GeomVector,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    fun = grad_function,
    xlim = xlim,
    ylim = ylim,
    n = n,
    center = center,
    normalize = normalize,
    arrow = arrow,
    ...
  )

}

