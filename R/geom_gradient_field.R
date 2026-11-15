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
#' - **geom_gradient_field()** maps color to the size of the gradient:
#' `color = after_stat(norm)` for vectors (the default) and `color =
#' after_stat(avg_spd)` for streams.
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
#'   **geom_gradient_field** the default mapping is `color = after_stat(norm)`
#'   for vectors and `color = after_stat(avg_spd)` for streams, whereas in **geom_gradient_field2** with `type = "vector"` it includes
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
#' @param normalize Logical. If `TRUE` (the default), every vector or stream
#'   has the same length `L`. If `FALSE`, lengths show the size of the
#'   gradient; see [geom_stream_field()] for how streams are then run.
#' @param tail_point Logical. If `TRUE`, a point is drawn at the tail of each
#'   gradient vector.
#' @param eval_point Logical. If `TRUE`, a point is drawn at the evaluation
#'   point where the gradient was computed. Defaults to `FALSE`.
#' @param grid A data frame containing precomputed grid points for seed
#'   placement. If `NULL` (default), a regular Cartesian grid is generated based
#'   on `xlim`, `ylim`, and `n`. For streams, `grid = "even"` places evenly
#'   spaced streamlines instead (Jobard and Lefer, 1997): each stream is grown
#'   in both directions until it comes within half a grid spacing of another
#'   stream, reaches a sink or leaves the plot, and new streams are started a
#'   grid spacing to either side of earlier ones, so the streams fill the plot
#'   about one grid spacing apart. `L`, `T` and `normalize` are not used then;
#'   increase `n` for denser streams.
#' @param arrow A \code{grid::arrow()} specification to add arrowheads to the
#'   gradient vectors. In **geom_gradient_field**, the default is a closed arrow
#'   with a 30° angle and length `0.02` npc; in `geom_gradient_field2()`, the
#'   default is `NULL`.
#' @param max_it Integer. Maximum number of integration steps allowed when
#'   computing the gradient stream. Defaults to `1000`.
#' @param L Numeric. The length of each vector or stream; with `normalize =
#'   FALSE` and no `T`, streams are grown to `L` and cut back to the time of
#'   the fastest one (see [geom_stream_field()]). Defaults to `NULL`, which uses 85% of the
#'   spacing of the grid.
#' @param type Character. Specifies the type of field to compute: use `"stream"`
#'   to generate integrated streamlines or `"vector"` for individual vector
#'   segments. Defaults to `"vector"` in `geom_gradient_field()` and
#'   `stat_gradient_field()`, and to `"stream"` in `geom_gradient_field2()` and
#'   `stat_gradient_field2()`.
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
#'   - `color`: In **geom_gradient_field**, mapped to `norm` (vectors) or
#'   `avg_spd` (streams) by default. In **geom_gradient_field2**, color is not
#'   mapped by default.
#'   - `length`: In **geom_gradient_field2** with `type = "vector"`, the
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
#'   \item{norm}{The Euclidean norm of the gradient at the evaluation point,
#'     \eqn{\sqrt{f_x^2 + f_y^2}}. By default it is the color of vectors in
#'     **geom_gradient_field** and their length in **geom_gradient_field2**.}
#'
#'   \item{avg_spd}{For streams, the average speed along the stream, as in
#'     [geom_stream_field()]. By default it is the color of streams in
#'     **geom_gradient_field**.}
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
#' @aliases geom_gradient_field stat_gradient_field geom_gradient_field2 stat_gradient_field2
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
  method = "rk4",
  T = NULL,
  L = NULL,
  center = TRUE,
  type = "vector",
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  grid = NULL,
  domain = NULL,
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
  method = "rk4",
  T = NULL,
  L = NULL,
  center = TRUE,
  type = "vector",
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  grid = NULL,
  domain = NULL,
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
  method = "rk4",
  T = NULL,
  L = NULL,
  center = FALSE,
  type = "stream",
  normalize = TRUE,
  tail_point = TRUE,
  eval_point = FALSE,
  grid = NULL,
  domain = NULL,
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
  method = "rk4",
  T = NULL,
  L = NULL,
  center = FALSE,
  type = "stream",
  normalize = TRUE,
  tail_point = TRUE,
  eval_point = FALSE,
  grid = NULL,
  domain = NULL,
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
