#' Vector Field Layers for ggplot2
#'
#' These functions provide convenient ggplot2 layers for drawing vector fields
#' using streamlines.
#'
#' A user-defined function (`fun`) specifies the behavior of the vector field by
#' taking a numeric vector of length 2 (representing \eqn{(x, y)}) and returning
#' a numeric vector of length 2 (representing \eqn{(dx, dy)}). The underlying
#' [StatStreamField] computes the streamlines based on the vector field
#' function, and [GeomStream] renders them.
#'
#' Two variants are provided:
#'
#' - **geom_vector_field()** uses a default mapping that sets `color = after_stat(norm)`.
#' - **geom_vector_field2()** uses a default mapping that sets `length = after_stat(norm)`
#' (with `color` unmapped by default).
#'
#' @inheritParams geom_vector
#' @inheritParams geom_stream_field
#'
#' @param mapping A set of aesthetic mappings created by [ggplot2::aes()].
#'   Additional aesthetics such as `color`, `size`, `linetype`, and `alpha` can
#'   be defined. In
#'   **geom_vector_field**, the default mapping includes `color = after_stat(norm)`,
#'   whereas in **geom_vector_field2** the default mapping includes `length =
#'   after_stat(norm)`.
#' @param data A data frame containing the input data.
#' @param stat The statistical transformation to use on the data for this layer.
#'   Defaults to [StatStreamField].
#' @param position Position adjustment, either as a string or the result of a
#'   call to a position adjustment function.
#' @param na.rm Logical. If `FALSE` (the default), missing values are removed
#'   with a warning.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes Logical. If `FALSE`, overrides the default aesthetics
#'   rather than combining with them.
#' @param fun A function that defines the vector field. It should take a numeric
#'   vector of length 2 (representing \eqn{(x, y)}) and return a numeric vector
#'   of length 2 (representing \eqn{(dx, dy)}). **(Required)**
#' @param xlim Numeric vector of length two. Specifies the limits of the x-axis
#'   domain. Defaults to `c(-1, 1)`.
#' @param ylim Numeric vector of length two. Specifies the limits of the y-axis
#'   domain. Defaults to `c(-1, 1)`.
#' @param n Integer. Grid resolution specifying the number of seed points along
#'   each axis. Higher values produce a denser vector field. Defaults to `11`.
#' @param args List of additional arguments passed on to the function defined by
#'   `fun`.
#' @param center Logical. If `TRUE`, centers the seed points or the vectors so
#'   that the original (x, y) becomes the midpoint. Defaults differ between the
#'   variants.
#' @param normalize Logical. If `TRUE`, stream lengths are normalized based on
#'   grid spacing. If `FALSE`, a default arc length is used. (Default is `TRUE`;
#'   if `TRUE`, it is converted internally to `"vector"`.)
#' @param tail_point Logical. If `TRUE`, a point is drawn at the tail of each
#'   streamline.
#' @param eval_point Logical. If `TRUE`, a point is drawn at the evaluation
#'   point, corresponding to the original (untransformed) seed point before any
#'   centering or normalization is applied.
#' @param grid A data frame containing precomputed grid points for seed
#'   placement. If `NULL` (default), a regular Cartesian grid is generated based
#'   on `xlim`, `ylim`, and `n`.
#' @param arrow A [grid::arrow()] specification to add arrowheads to the
#'   streamlines. In **geom_vector_field**, the default is a closed arrow with a
#'   30° angle and length `0.02` npc; in **geom_vector_field2** the default is
#'   `NULL`.
#' @param ... Other arguments passed on to [ggplot2::layer()].
#'
#' @section Aesthetics: `geom_vector_field()` and `geom_vector_field2()`
#'   understand the following aesthetics (required aesthetics are in **bold**):
#'
#'   - **`x`**: The x-coordinate of the vector's starting point.
#'   - **`y`**: The y-coordinate of the vector's starting point.
#'   - **`fx`**: The horizontal component of the vector displacement.
#'   - **`fy`**: The vertical component of the vector displacement.
#'   - `color`: The color of the vector lines (default mapping in **geom_vector_field**).
#'   - `length`: The computed vector norm (default mapping in **geom_vector_field2**).
#'   - `linetype`: The type of the vector line (e.g., solid, dashed).
#'   - `linewidth`: The thickness of the vector line.
#'   - `alpha`: The transparency of the vector.
#'
#' @return A ggplot2 layer that computes and plots a vector field using
#'   streamlines.
#'
#' \describe{
#'   \item{norm}{Calculated as the Euclidean distance between the starting point
#'     (\code{x}, \code{y}) and the computed endpoint. Used to normalize the
#'     vector.}
#' }
#'
#' @seealso [geom_stream_field()]
#'
#' @examples
#'
#' f <- function(u) c(-u[2], u[1])
#' ggplot() + geom_vector_field(fun = f, xlim = c(-1,1), ylim = c(-1,1))
#'
#' # xlim and ylim default to (-1,1), so for ease of illustration we remove them
#'
#' ggplot() + geom_vector_field(fun = f)
#' ggplot() + geom_vector_field(fun = f, grid = "hex")
#'
#' ggplot() + geom_vector_field2(fun = f)
#' ggplot() + geom_vector_field2(fun = f, grid = "hex")
#'
#' f <- efield_maker()
#' ggplot() + geom_vector_field(fun = f, xlim = c(-2,2), ylim = c(-2,2))
#' ggplot() + geom_vector_field2(fun = f, xlim = c(-2,2), ylim = c(-2,2))
#'
#' @name geom_vector_field
#' @aliases geom_vector_field stat_vector_field geom_vector_field2
#'   stat_vector_field2
NULL

#' @rdname geom_vector_field
#' @export
geom_vector_field <- function(
  mapping = NULL,
  data = NULL,
  stat = StatStreamField,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 11,
  args = list(),
  center = TRUE,
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  grid = NULL,
  arrow = grid::arrow(angle = 30, length = unit(0.02, "npc"), type = "closed")
  ) {

  # Define default mapping for geom_vector_field
  default_mapping <- aes(color = after_stat(norm))

  # Merge user-provided mapping with default mapping
  # User mapping takes precedence
  if (!is.null(mapping)) {
    if (!"color" %in% names(mapping)) mapping <- modifyList(default_mapping, mapping)
  } else {
    mapping <- default_mapping
  }

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  layer(
    stat = stat,
    geom = GeomStream,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      args = args,
      method = "euler",
      na.rm = na.rm,
      max_it = 2,
      center = center,
      normalize = normalize,
      type = "vector",
      tail_point = tail_point,
      eval_point = eval_point,
      grid = grid,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname geom_vector_field
#' @export
stat_vector_field <- function(
  mapping = NULL,
  data = NULL,
  stat = StatStreamField,
  geom = GeomStream,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 11,
  args = list(),
  center = TRUE,
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  grid = NULL,
  arrow = grid::arrow(angle = 30, length = unit(0.02, "npc"), type = "closed")
  ) {

  # Define default mapping for geom_vector_field
  default_mapping <- aes(color = after_stat(norm))

  # Merge user-provided mapping with default mapping
  # User mapping takes precedence
  if (!is.null(mapping)) {
    if (!"color" %in% names(mapping)) mapping <- modifyList(default_mapping, mapping)
  } else {
    mapping <- default_mapping
  }

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  layer(
    stat = stat,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      args = args,
      method = "euler",
      na.rm = na.rm,
      center = center,
      normalize = normalize,
      type = "vector",
      tail_point = tail_point,
      eval_point = eval_point,
      grid = grid,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname geom_vector_field
#' @export
geom_vector_field2 <- function(
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
   args = list(),
   center = FALSE,
   tail_point = TRUE,
   eval_point = FALSE,
   grid = NULL,
   arrow = NULL) {

  # Define default mapping for stat_stream_field2
  default_mapping <- ggplot2::aes(color = after_stat(NULL), length = after_stat(norm))

  # Merge user-provided mapping with default mapping
  # User mapping takes precedence
  if (!is.null(mapping)) {
    if (!"color" %in% names(mapping)) mapping <- modifyList(default_mapping, mapping)
  } else {
    mapping <- default_mapping
  }

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  ggplot2::layer(
    stat = stat,
    geom = GeomStream,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      args = args,
      method = "euler",
      na.rm = na.rm,
      center = center,
      normalize = TRUE,
      type = "vector",
      tail_point = tail_point,
      eval_point = eval_point,
      T = 1,
      grid = grid,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname geom_vector_field
#' @export
stat_vector_field2 <- function(mapping = NULL, data = NULL,
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
   args = list(),
   center = FALSE,
   tail_point = TRUE,
   eval_point = FALSE,
   grid = NULL,
   arrow = NULL) {

  # Define default mapping for stat_stream_field2
  default_mapping <- ggplot2::aes(color = after_stat(NULL), length = after_stat(norm))

  # Merge user-provided mapping with default mapping
  # User mapping takes precedence
  if (!is.null(mapping)) {
    if (!"color" %in% names(mapping)) mapping <- modifyList(default_mapping, mapping)
  } else {
    mapping <- default_mapping
  }

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  ggplot2::layer(
    stat = StatStreamField,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      args = args,
      method = "euler",
      na.rm = na.rm,
      center = center,
      normalize = TRUE,
      type = "vector",
      tail_point = tail_point,
      eval_point = eval_point,
      T = 1,
      grid = grid,
      arrow = arrow,
      ...
    )
  )
}

