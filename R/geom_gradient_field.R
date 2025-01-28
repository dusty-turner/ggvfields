#' Geom Gradient Field
#'
#' `geom_gradient_field()` creates a ggplot2 layer that visualizes the gradient of a scalar field
#' by generating a vector field. It internally computes the gradient using numerical differentiation
#' and leverages StatGradientField to process the data, which is then rendered using a specified geom
#' (defaulting to [GeomStream]).
#'
#' @param mapping Aesthetic mappings created by [ggplot2::aes()]. You can map aesthetics like
#'   `color`, `size`, `linetype`, etc., to variables computed by the stat.
#' @param data A data frame or other object, as in [ggplot2::layer()]. If `NULL`, the layer
#'   uses the plot's data.
#' @param stat The statistical transformation to use on the data for this layer. Default is [`StatVectorField`].
#' @param geom The geometric object to use to display the data. Defaults to [GeomStream].
#' @param position Position adjustment, either as a string, or the result of a call to a
#'   position adjustment function. Defaults to `"identity"`.
#' @param ... Other arguments passed on to [ggplot2::layer()] and the underlying
#'   StatGradientField and [GeomStream]. These are often used to set aesthetics like `color = "blue"`
#'   or `size = 1.0`.
#' @param na.rm Logical. If `FALSE` (default), removes missing values with a warning.
#'   If `TRUE`, silently removes missing values.
#' @param show.legend Logical. Should this layer be included in the legends? `NA`, the default,
#'   includes it if any aesthetics are mapped. `FALSE` never includes it, and `TRUE` always includes
#'   it.
#' @param inherit.aes Logical. If `FALSE`, overrides the default aesthetics, rather than
#'   combining with them. This is most useful for helper functions that define both data and
#'   aesthetics, and should not inherit behavior from the main ggplot call.
#' @param fun A scalar function that defines the field. It should take a numeric vector of
#'   length 2 (representing \eqn{(x, y)} coordinates) and return a single numeric value representing
#'   the scalar field at that point. **(Required)**
#' @param xlim Numeric vector of length two. Specifies the limits of the x-axis domain.
#'   Defaults to `c(-1, 1)`.
#' @param ylim Numeric vector of length two. Specifies the limits of the y-axis domain.
#'   Defaults to `c(-1, 1)`.
#' @param n Integer. Grid resolution specifying the number of seed points along each axis.
#'   Higher values produce a denser vector field. Defaults to `11`.
#' @param center Logical. If `TRUE`, centers the seed points around the midpoint of the domain.
#'   Useful for symmetric flows. Defaults to `TRUE`.
#' @param normalize Logical; if `TRUE`, normalizes each vector to a unit length before
#'   applying any scaling. This can help prevent overplotting in dense plots and
#'   ensures consistent visual representation.
#' @param arrow A [grid::arrow()] specification to add arrowheads to the vectors, indicating
#'   direction. Defaults to a closed arrow with a 30-degree angle and length `0.02` npc.
#'
#' @return A ggplot2 **Layer** object that can be added to a plot. It computes the gradient
#'   vectors based on the specified scalar field function and visualizes them.
#'
#' @details
#' - **Scalar Field Function (`fun`)**: The function should encapsulate the behavior of the scalar field.
#'   For example, a paraboloid field can be defined as `function(v) { v[1]^2 + v[2]^2 }`.
#' - **Gradient Calculation**:
#'   The gradient is computed numerically using the `numDeriv::grad` function, which approximates
#'   the gradient vector \eqn{(\partial f / \partial x, \partial f / \partial y)} at each point.
#' - **Aesthetic Mappings**: Vector aesthetics like `color`, `size`, and `linetype` can be customized
#'   via the `mapping` parameter or by setting them directly in the `geom_gradient_field` call.
#'
#' @section Aesthetics:
#' `geom_gradient_field()` understands the following aesthetics (optional):
#' - `color`: Color of the gradient vectors.
#' - `size`: Thickness of the gradient vectors.
#' - `linetype`: Line type of the gradient vectors.
#' - `alpha`: Transparency level of the gradient vectors.
#'
#' @examples
#'
#' paraboloid_field <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   x^2 + y^2
#' }
#'
#' # Create the gradient field plot
#' ggplot() +
#'   geom_gradient_field(fun = paraboloid_field)
#'
#' @export
geom_gradient_field <- function(mapping = NULL, data = NULL,
                                stat = StatVectorField,
                                position = "identity",
                                ...,
                                na.rm = FALSE,
                                show.legend = TRUE,
                                inherit.aes = TRUE,
                                fun,
                                xlim = c(-1, 1),
                                ylim = c(-1, 1),
                                n = 11,
                                center = TRUE,
                                normalize = TRUE,
                                arrow = grid::arrow(angle = 30,
                                                    length = grid::unit(0.02, "npc"),
                                                    type = "closed")) {

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  default_mapping <- aes(color = after_stat(norm))

  if (!is.null(mapping)) {
    if (!"color" %in% names(mapping)) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    mapping <- default_mapping
  }

  if (missing(fun) || !is.function(fun)) {
    stop("Please provide a valid scalar function 'fun' that takes a numeric vector (x, y) and returns a single numeric value.")
  }

  gradient_fun <- function(v) {
    if (!is.numeric(v) || length(v) != 2) {
      stop("Input to the gradient function must be a numeric vector of length 2 (x, y).")
    }
    numDeriv::grad(func = fun, x = v)
  }

  layer(
    stat = stat,
    geom = GeomStream,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = gradient_fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      method = "euler",
      na.rm = na.rm,
      dt = 1,
      L = 0.1,
      center = center,
      normalize = normalize,
      arrow = arrow,
      ...
    )
  )
}


#' @rdname geom_gradient_field
#' @export
stat_gradient_field <- function(mapping = NULL, data = NULL,
                                geom = GeomStream,
                                position = "identity",
                                ...,
                                na.rm = FALSE,
                                show.legend = TRUE,
                                inherit.aes = TRUE,
                                fun,
                                xlim = c(-1, 1),
                                ylim = c(-1, 1),
                                n = 11,
                                center = TRUE,
                                normalize = TRUE,
                                arrow = grid::arrow(angle = 30,
                                                    length = grid::unit(0.02, "npc"),
                                                    type = "closed")) {

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  default_mapping <- aes(color = after_stat(norm))

  if (!is.null(mapping)) {
    if (!"color" %in% names(mapping)) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    mapping <- default_mapping
  }

  if (missing(fun) || !is.function(fun)) {
    stop("Please provide a valid scalar function 'fun' that takes a numeric vector (x, y) and returns a single numeric value.")
  }

  gradient_fun <- function(v) {
    if (!is.numeric(v) || length(v) != 2) {
      stop("Input to the gradient function must be a numeric vector of length 2 (x, y).")
    }
    numDeriv::grad(func = fun, x = v)
  }

  layer(
    stat = StatVectorField,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = gradient_fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      method = "euler",
      na.rm = na.rm,
      dt = 1,
      L = 0.1,
      center = center,
      normalize = normalize,
      arrow = arrow,
      ...
    )
  )
}
