#' Create a Complex Vector Field Plot Layer
#'
#' `stat_complex_vector_field` generates a vector field plot layer using a
#' user-defined function to compute the vector components. This is particularly
#' useful for visualizing vector fields in a two-dimensional space.
#'
#' @inheritParams ggplot2::geom_raster
#' @inheritParams ggplot2::stat_identity
#' @param fun A user-defined function that takes a complex number as input and
#'   returns a complex number.
#' @param relim A numeric vector of length 2 giving the real-axis limits.
#' @param imlim A numeric vector of length 2 giving the imaginary-axis limits.
#' @param n An integer specifying the number of grid points along each axis.
#'
#' @return A ggplot2 layer that can be added to a ggplot object to produce a
#'   vector field plot.
#' @export
#' @examples
#'
#' # Example user-defined function
#' f <- function(z) (z^2 + 1) / (z^2 - 1)
#'
#' # Create a ggplot with the vector field layer
#' ggplot() +
#'   stat_complex_function(fun = f, relim = c(-2, 2), imlim = c(-2, 2), n = 100)
#'

stat_complex_function <- function(mapping = NULL, data = NULL, geom = "raster",
                                      position = "identity", na.rm = FALSE,
                                      show.legend = NA, inherit.aes = TRUE,
                                      fun, relim, imlim, n = 10, ...) {

  if (is.null(data)) data <- ensure_nonempty_data

  layer(
    stat = StatComplexFunction,
    data = data,
    mapping = mapping,
    geom = "raster",
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      relim = relim,
      imlim = imlim,
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}

StatComplexFunction <- ggproto("StatComplexFunction", Stat,

  required_aes = c("relim", "imlim"),

  compute_group = function(data, scales, fun, relim, imlim, n = n) {
    # Create a sequence of real and imaginary values within the limits
    re_seq <- seq(relim[1], relim[2], length.out = n)
    im_seq <- seq(imlim[1], imlim[2], length.out = n)
    grid <- expand.grid(re = re_seq, im = im_seq)

    grid$z <- complex(real = grid$re, imaginary = grid$im)
    grid$fz <- fun(grid$z)
    grid$H <- rad2deg(Arg(grid$fz))
    grid$S <- 80
    grid$L <- 50 + times(times(atan(abs(grid$fz)),2/pi),50)
    grid$fill <- farver::encode_colour(cbind(grid$H, grid$S, grid$L), from = "hsl")

    grid$x <- grid$re
    grid$y <- grid$im

    data <- grid
  }
)

#' Create a Complex Vector Field Geom Layer
#'
#' `geom_complex_vector_field` generates a vector field plot layer using a user-defined function to compute the vector components. This is particularly useful for visualizing vector fields in a two-dimensional space.
#'
#' @inheritParams ggplot2::geom_raster
#' @param fun A user-defined function that takes a complex number as input and returns a complex number.
#' @param relim A numeric vector of length 2 giving the real-axis limits.
#' @param imlim A numeric vector of length 2 giving the imaginary-axis limits.
#' @param n An integer specifying the number of grid points along each axis.
#'
#' @return A ggplot2 layer that can be added to a ggplot object to produce a vector field plot.
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Example user-defined function
#' f <- function(z) (z^2 + 1) / (z^2 - 1)
#'
#' # Create a ggplot with the vector field layer
#' ggplot() +
#'   geom_complex_vector_field(fun = f, relim = c(-2, 2), imlim = c(-2, 2), n = 100) +
#'   labs(x = "Real", y = "Imaginary")
#'

geom_complex_function <- function(mapping = NULL, data = NULL,
  stat = "complex_function",
  position = "identity", na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE,
  fun, relim, imlim, n = 10, ...) {

  if (is.null(data)) data <- ensure_nonempty_data(data)

  layer(
    stat = StatComplexFunction,
    data = data,
    mapping = mapping,
    geom = GeomComplexFunction,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      relim = relim,
      imlim = imlim,
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_complex_function
#' @format NULL
#' @usage NULL
#' @export
GeomComplexFunction <- ggproto("GeomComplexFunction", GeomRaster)
