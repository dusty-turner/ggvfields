#' Create a Complex Vector Field Geom Layer
#'
#' [geom_complex_function()] generates a raster plot layer of a user-defined
#' complex function. The hue of each color corresponds to the argument (angle)
#' of the complex number resulting from the function, indicating the direction
#' of the vector. The lightness of the color reflects the magnitude of the
#' vector, providing a visual representation of both direction and intensity.
#'
#' @param fun A user-defined function that takes a complex number as input and
#'   returns a complex number.
#' @param relim A numeric vector of length 2 giving the real-axis limits.
#' @param imlim A numeric vector of length 2 giving the imaginary-axis limits.
#' @param n An integer specifying the number of grid points along each axis.
#' @return A ggplot2 layer that can be added to a ggplot object to produce a
#'   vector field plot.
#' @keywords internal
#'
#' @examples
#' library(ggplot2)
#' # Example user-defined function
#' f <- function(z) (z^2 + 1) / (z^2 - 1)
#'
#' # Create a ggplot with the vector field layer
#' ggplot() +
#'   geom_complex_function(fun = f, relim = c(-2, 2), imlim = c(-2, 2), n = 100) +
#'   labs(x = "Real", y = "Imaginary")
NULL

#' @keywords internal
geom_complex_function <- function(mapping = NULL, data = NULL,
                                  stat = "complex_function",
                                  position = "identity", na.rm = FALSE,
                                  show.legend = TRUE, inherit.aes = TRUE,
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

#' @keywords internal
GeomComplexFunction <- ggproto("GeomComplexFunction", GeomRaster)

#' @keywords internal
stat_complex_function <- function(mapping = NULL, data = NULL, geom = "raster",
                                  position = "identity", na.rm = FALSE,
                                  show.legend = TRUE, inherit.aes = TRUE,
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

#' @keywords internal
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

   grid$Angle <- grid$H
   grid$Magnitude <- grid$L

   data <- biscale::bi_class(grid, x = Angle, y = Magnitude, style = "quantile")
  }
)
