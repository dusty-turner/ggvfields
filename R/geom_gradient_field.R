#' Create a Gradient Field Layer in ggplot2
#'
#' These functions provide convenient ggplot2 layers for drawing gradient fields
#' by computing the gradient of a scalar field. A user-defined function (`fun`)
#' specifies the behavior of the scalar field by taking a numeric vector of
#' length 2 (representing \eqn{(x, y)}) and returning a single numeric value.
#' The underlying [StatStreamField] computes the gradient via numerical
#' differentiation (using \code{numDeriv::grad()}) and [GeomStream] renders the
#' resulting vectors.
#'
#' Two variants are provided:
#'
#' - **geom_gradient_field()** uses a default mapping that sets `color = after_stat(norm)`.
#' - **geom_gradient_field2()** leaves `color` unmapped and marks each seed
#' point with a dot. With `type = "vector"` it sets `length = after_stat(norm)`,
#' so vectors are scaled by the size of the gradient, like
#' [geom_vector_field2()]. Streams (the default) ignore `length`, so they get no
#' default mapping, like [geom_stream_field2()].
#'
#' @inheritParams geom_vector
#' @inheritParams geom_stream_field
#'
#' @param mapping A set of aesthetic mappings created by \code{ggplot2::aes()}.
#'   Additional aesthetics such as `color`, `size`, `linetype`, and `alpha` can
#'   be defined. In
#'   **geom_gradient_field** the default mapping includes `color = after_stat(norm)`,
#'   whereas in **geom_gradient_field2** with `type = "vector"` it includes
#'   `length = after_stat(norm)`.
#' @param data A data frame containing the input data.
#' @param stat The statistical transformation to use on the data for this layer.
#'   Defaults to [StatStreamField].
#' @param position Position adjustment, either as a string or the result of a
#'   position adjustment function.
#' @param na.rm Logical. If `FALSE` (the default), missing values are removed
#'   with a warning.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes Logical. If `FALSE`, overrides the default aesthetics
#'   rather than combining with them.
#' @param fun A function that defines the scalar field. It should take a numeric
#'   vector of length 2 (representing \eqn{(x, y)}) and return a single numeric
#'   value. **(Required)**
#' @param xlim Numeric vector of length two. Specifies the limits of the x-axis
#'   domain. Defaults to `c(-1, 1)`.
#' @param ylim Numeric vector of length two. Specifies the limits of the y-axis
#'   domain. Defaults to `c(-1, 1)`.
#' @param n Integer. Grid resolution specifying the number of seed points along
#'   each axis. Higher values produce a denser gradient field. Defaults to `11`.
#' @param center Logical. If `TRUE`, centers the seed points so that the
#'   original (x, y) becomes the midpoint.
#' @param normalize Logical. If `TRUE`, gradient vectors are normalized based on
#'   grid spacing. Defaults to `TRUE`.
#' @param tail_point Logical. If `TRUE`, a point is drawn at the tail of each
#'   gradient vector.
#' @param eval_point Logical. If `TRUE`, a point is drawn at the evaluation
#'   point where the gradient was computed. Defaults to `FALSE`.
#' @param grid A data frame containing precomputed grid points for seed
#'   placement. If `NULL` (default), a regular Cartesian grid is generated based
#'   on `xlim`, `ylim`, and `n`.
#' @param arrow A \code{grid::arrow()} specification to add arrowheads to the
#'   gradient vectors. In **geom_gradient_field**, the default is a closed arrow
#'   with a 30° angle and length `0.02` npc; in `geom_gradient_field2()`, the
#'   default is `NULL`.
#' @param max_it Integer. Maximum number of integration steps allowed when
#'   computing the gradient stream. Defaults to `1000`.
#' @param T Numeric. Time increment used for numerical integration when
#'   `normalize` is FALSE. If not provided, it is computed automatically based
#'   on grid spacing and the vector field’s magnitude.
#' @param L Numeric. Target length for the gradient vectors or streamlines. When
#'   `normalize` is TRUE, computed vectors are scaled to have length L. If not
#'   provided, L is computed automatically from the grid spacing.
#' @param type Character. Specifies the type of field to compute: use `"stream"`
#'   to generate integrated streamlines or `"vector"` for individual vector
#'   segments. Defaults to `"stream"`.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
#' @param ... Other arguments passed on to \code{grid::layer()}.
#'
#' @section Aesthetics: `geom_gradient_field()` and `geom_gradient_field2()`
#'   understand the following aesthetics (required aesthetics are in **bold**):
#'
#'   - **`x`**: The x-coordinate of the seed point.
#'   - **`y`**: The y-coordinate of the seed point.
#'   - **`color`**: In **geom_gradient_field**, the color of the gradient vector.
#'   In **geom_gradient_field2**, color is not mapped by default.
#'   - **`length`**: In **geom_gradient_field2** with `type = "vector"`, the
#'   computed vector norm.
#'   - `size`, `linetype`, `alpha`: Additional aesthetics to control appearance.
#'
#' @return A ggplot2 layer that computes and plots a gradient field by
#'   numerically differentiating a scalar field.
#'
#' @section Computed Variables:
#'
#' The following variables are computed internally by [StatStreamField] when
#' generating the gradient field from a scalar function:
#'
#' \describe{
#'   \item{norm}{The Euclidean norm of the gradient vector, calculated as
#'     \eqn{\sqrt{fx^2 + fy^2}}. This value is used, by default, for mapping color or scaling
#'     arrow lengths in the visualization.}
#'
#'   \item{avg_spd}{This variable may represent an average speed computed
#'     from the gradient magnitude. In the default mapping for **geom_gradient_field**, the
#'     color aesthetic is mapped to \code{after_stat(avg_spd)}.}
#' }
#'
#' @examples
#' Si <- matrix(c(1, 0.75, 0.75, 1), nrow = 2)
#' f <- function(u) exp(-as.numeric(u %*% solve(Si) %*% u) / 2) / (2 * pi * det(Si))
#'
#' ggplot() +
#'   geom_gradient_field(fun = f, xlim = c(-3, 3), ylim = c(-3, 3))
#'
#' \donttest{
#' df <- expand.grid(x = seq(-3, 3, 0.1), y = seq(-3, 3, 0.1)) |>
#'   transform(fxy = apply(cbind(x, y), 1, f))
#'
#' ggplot() +
#'   geom_raster(aes(x, y, fill = fxy), data = df) +
#'   geom_gradient_field(fun = f, xlim = c(-3, 3), ylim = c(-3, 3)) +
#'   coord_equal()
#'
#' # over highest density regions, if ggdensity is installed
#' if (requireNamespace("ggdensity", quietly = TRUE)) {
#'   fxy <- function(x, y) apply(cbind(x, y), 1, f)
#'
#'   ggplot() +
#'     ggdensity::geom_hdr_fun(fun = fxy, xlim = c(-3, 3), ylim = c(-3, 3)) +
#'     geom_gradient_field(fun = f, xlim = c(-3, 3), ylim = c(-3, 3)) +
#'     coord_equal()
#' }
#' }
#' @aliases geom_gradient_field stat_gradient_field geom_gradient_field2
#'   stat_gradient_field2
#' @name geom_gradient_field
#' @export
NULL

#' @rdname geom_gradient_field
#' @export
geom_gradient_field <- function(
  mapping = NULL,
  data = NULL,
  stat = StatStreamField,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = TRUE,
  inherit.aes = FALSE,
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 11,
  max_it = 1000,
  T = NULL,
  L = NULL,
  center = TRUE,
  method = "rk4",
  type = "vector",
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  grid = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = grid::arrow(angle = 30, length = grid::unit(0.02, "npc"), type = "closed")
) {

  call_args <- as.list(environment())

  construct_layer(
    gradient_field_layer, call_args, list(...),
    geom = GeomStream,
    map_colour = TRUE
  )
}

#' @rdname geom_gradient_field
#' @export
stat_gradient_field <- function(
  mapping = NULL,
  data = NULL,
  geom = GeomStream,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = TRUE,
  inherit.aes = FALSE,
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 11,
  max_it = 1000,
  T = NULL,
  L = NULL,
  center = TRUE,
  method = "rk4",
  type = "vector",
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  grid = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = grid::arrow(angle = 30, length = grid::unit(0.02, "npc"), type = "closed")
) {

  call_args <- as.list(environment())

  construct_layer(
    gradient_field_layer, call_args, list(...),
    stat = StatStreamField,
    map_colour = TRUE
  )
}


#' @rdname geom_gradient_field
#' @export
geom_gradient_field2 <- function(
  mapping = NULL,
  data = NULL,
  stat = StatStreamField,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = TRUE,
  inherit.aes = FALSE,
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 11,
  max_it = 1000,
  T = NULL,
  L = NULL,
  center = FALSE,
  method = "rk4",
  type = "stream",
  normalize = TRUE,
  tail_point = TRUE,
  eval_point = FALSE,
  grid = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = NULL
) {

  call_args <- as.list(environment())

  construct_layer(
    gradient_field_layer, call_args, list(...),
    geom = GeomStream,
    map_colour = FALSE
  )
}


#' @rdname geom_gradient_field
#' @export
stat_gradient_field2 <- function(
  mapping = NULL,
  data = NULL,
  geom = GeomStream,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = TRUE,
  inherit.aes = FALSE,
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 11,
  max_it = 1000,
  T = NULL,
  L = NULL,
  center = FALSE,
  method = "rk4",
  type = "stream",
  normalize = TRUE,
  tail_point = TRUE,
  eval_point = FALSE,
  grid = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = NULL
) {

  call_args <- as.list(environment())

  construct_layer(
    gradient_field_layer, call_args, list(...),
    stat = StatStreamField,
    map_colour = FALSE
  )
}
