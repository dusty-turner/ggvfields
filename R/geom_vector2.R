#' Create a Vector Plot Layer with Norm Mapped to Length
#'
#' `geom_vector2` and `stat_vector2` are extensions of `geom_vector` and `stat_vector`.
#' These functions provide identical functionality but change the default behavior
#' to map the **vector norm (magnitude)** to the `length` aesthetic instead of color.
#'
#' This is useful when you want the vector's magnitude to be visually emphasized by
#' its length rather than its color. These functions also disable the default
#' color mapping, setting all vectors to black unless otherwise specified.
#'
#' These functions ensure that `length = after_stat(norm)` is applied by default,
#' while the `color` aesthetic is set to `NULL`. This gives users the ability to
#' scale vector length by magnitude without visual interference from color mapping.
#'
#' @inheritParams geom_vector
#' @param mapping Aesthetic mappings created by `aes()` or `aes_()`.
#'   These functions ensure that `length = after_stat(norm)` is mapped by default,
#'   and `color` is set to `NULL` unless otherwise specified.
#' @param ... Other arguments passed on to `geom_vector()` or `stat_vector()`.
#' @return A ggplot2 layer that can be added to a ggplot object.
#'
#' @examples
#' set.seed(1234)
#' n <- 10
#' wind_data <- data.frame(
#'   lon = rnorm(n),
#'   lat = rnorm(n),
#'   wind_dir = runif(n, -pi, pi),
#'   wind_spd = rchisq(n, df = 2),
#'   dx = rchisq(n, df = 2) * cos(runif(n, -pi, pi)),
#'   dy = rchisq(n, df = 2) * sin(runif(n, -pi, pi))
#' )
#'
#' ggplot(wind_data) +
#'   geom_vector2(aes(x = lon, y = lat, dx = dx, dy = dy))
#'
#' # Example with Polar Coordinates
#' ggplot(wind_data) +
#'   geom_vector2(aes(x = lon, y = lat, angle = wind_dir, distance = wind_spd))
#'
#' @seealso
#' Use [geom_vector()] if you prefer to map vector magnitude to color rather than length.
#'
#' @export
geom_vector2 <- function(
    mapping = NULL,
    data = NULL,
    stat = StatVector,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  mapping <- modifyList(aes(length = after_stat(norm), color = NULL), mapping)
  geom_vector(
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    ...,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}

#' @rdname geom_vector2
#' @export
stat_vector2 <- function(
    mapping = NULL,
    data = NULL,
    geom = GeomVector,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  mapping <- modifyList(aes(length = after_stat(norm), color = NULL), mapping)
  stat_vector(
    mapping = mapping,
    data = data,
    geom = geom,
    position = position,
    ...,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes
  )
}
