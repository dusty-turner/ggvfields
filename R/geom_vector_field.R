#' Create a Vector Field Plot Layer
#'
#' `geom_vector_field()` generates a vector field plot layer from a user-defined
#' function `fun` that computes vector displacements (`dx`, `dy`) over a specified domain.
#'
#' **How the Domain is Determined**:
#' - If you provide **data with `aes(x, y)`**, the domain is inferred from the data.
#' - If you also specify **`xlim` and `ylim`**, they override any domain inferred from the data.
#' - If you do **not provide data**, but supply **`fun`, `xlim`, and `ylim`**, then `geom_vector_field()`
#'   generates its own grid of points within these limits.
#' - If data is provided **without `aes(x, y)`** mapped, you must provide `xlim` and `ylim`
#'   so the domain can be determined.
#'
#' **Default Behavior**:
#' - The **magnitude of each vector (`norm`) is mapped to `color` by default**, emphasizing vector strength.
#' - **Vector lengths** are scaled to **90% of the grid spacing**.
#' - **Vectors are normalized to unit length** before scaling. To see original lengths, set `normalize = FALSE`.
#' - **Arrowheads** are included by default.
#'
#' @inheritParams ggplot2::geom_raster
#' @inheritParams ggplot2::stat_identity
#'
#' @param fun A function that takes a vector `(x, y)` and returns `(dx, dy)`, defining
#'   vector displacements.
#' @param xlim,ylim Numeric vectors of length 2 defining the domain limits on the x/y-axis.
#' @param n Integer, the number of grid points along each axis.
#' @param center Logical; if `TRUE`, centers vectors on their grid points.
#' @param normalize Logical; if `TRUE`, normalizes vectors before scaling.
#' @param tail_point Logical; if `TRUE`, adds a point at the tail of each vector.
#' @param arrow Arrow specification created by `grid::arrow()`.
#' @param linewidth Numeric; thickness of the vector lines. Defaults to 2.
#' @param args A named list of additional arguments to pass to `fun`. For example, if
#'   your function signature is `function(v, scale = 1)`, you can supply
#'   `args = list(scale = 2)` to change the `scale` parameter.
#' @param ... Additional arguments passed to `layer()`.
#'
#' @section Computed Variables:
#' \describe{
#'   \item{norm}{\eqn{\sqrt{dx^2 + dy^2}}: Magnitude of the vector.}
#'   \item{divergence}{\eqn{\frac{\partial f_1}{\partial x} + \frac{\partial f_2}{\partial y}}: Divergence.}
#'   \item{curl}{\eqn{\frac{\partial f_2}{\partial x} - \frac{\partial f_1}{\partial y}}: Curl.}
#' }
#'
#' @section Aesthetic mappings:
#' - `norm`, `divergence`, and `curl` can be mapped with `after_stat()`.
#'
#' @examples
#'
#' # Example user-defined vector field function with a small random error
#' f <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(x + y, y - x)
#' }
#'
#' set.seed(1234)
#' n <- 10
#' wind_data <- data.frame(
#'   lon = rnorm(n),
#'   lat = rnorm(n)
#' )
#'
#' # Apply function f to each row to compute dx and dy with error
#' wind_data[, c("dx", "dy")] <- t(apply(wind_data[, c("lon", "lat")], 1, f))
#'
#' ### 1. No data provided:
#' #    `fun`, `xlim`, and `ylim` define the domain.
#' #    The layer generates a grid of points within (-5,5) for x and y.
#' ggplot() +
#'   geom_vector_field(fun = f, xlim = c(-5, 5), ylim = c(-5, 5))
#'
#' ### 2. With data provided:
#' #    Create sample data and compute dx, dy using function f with error.
#' # Plot data's own vectors and add a computed vector field
#' ggplot(wind_data, aes(x = lon, y = lat)) +
#'   geom_vector(aes(dx = dx, dy = dy), color = "black") +
#'   geom_vector_field(fun = f)
#'
#' ### 3. With data provided but overriding with xlim and ylim:
#' #    Supply `xlim` and `ylim` so the domain is determined by these limits instead of the data.
#' ggplot(wind_data) +
#'   geom_vector(aes(x = lon, y = lat, dx = dx, dy = dy), color = "black", normalize = FALSE) +
#'   geom_vector_field(fun = f, xlim = c(-5, 5), ylim = c(-5, 5))
#'
#' ### 4. Passing additional arguments to `fun` using `args`:
#' #    Suppose we define a rotation-like field with a tunable scale:
#' f_scale <- function(v, scale = 1) {
#'   x <- v[1]; y <- v[2]
#'   scale * c(-y, x)
#' }
#'
#' #    Now we can pass 'scale' via args = list(scale = 10):
#' ggplot() +
#'   geom_vector_field(
#'     fun = f_scale,
#'     xlim = c(-5, 5), ylim = c(-5, 5),
#'     args = list(scale = 10), # <- pass scale to fun
#'     color = "blue"
#'   ) +
#'   coord_fixed()
#'
#' @export


geom_vector_field <- function(mapping = NULL, data = NULL,
                              stat = StatVector, geom = GeomVector,
                              ...,
                              position = "identity",
                              center = TRUE,
                              normalize = TRUE,
                              tail_point = FALSE,
                              arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed"),
                              fun = NULL,
                              args = list(),
                              xlim = NULL,
                              ylim = NULL,
                              n = NULL,
                              show.legend = NA,
                              linewidth = 2,
                              inherit.aes = TRUE) {

  # Check if x and y are in the mapping
  mapping_defines_xy <- !is.null(mapping) && all(c("x", "y") %in% names(mapping))

  # If no data and no x,y aesthetics are specified, but we have fun, xlim, ylim,
  # we need dummy data to trigger compute_group().
  if (is.null(data) && !mapping_defines_xy && !is.null(fun) && !is.null(xlim) && !is.null(ylim)) {
    data <- data.frame(x = NA_real_, y = NA_real_)
  }

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
      linewidth = linewidth,
      arrow = arrow,
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      args = args,
      n = n,
      ...
    )
  )
}




#' @rdname geom_vector_field
#' @export
stat_vector_field <- function(mapping = NULL, data = NULL,
                              geom = GeomVector, position = "identity",
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE,
                              fun = NULL,
                              xlim = c(-1, 1),
                              ylim = c(-1, 1),
                              args = list(),
                              n = 11,
                              center = TRUE,
                              normalize = TRUE,
                              tail_point = FALSE,
                              linewidth = 2,
                              arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")) {
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
      linewidth = linewidth,
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      args = args,
      n = n,
      ...
    )
  )
}


utils::globalVariables(c("x", "y", "dx", "dy"))
