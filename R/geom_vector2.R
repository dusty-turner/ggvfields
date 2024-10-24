#' Create a Vector Plot Layer with Norm Mapped to Color
#'
#' `geom_vector2` and `stat_vector2` are extensions of `geom_vector` and `stat_vector`.
#' They provide identical functionality but map the **vector norm (magnitude)** to
#' the `color` aesthetic by default. This is useful when you want vector magnitude to
#' be emphasized visually through color.
#'
#' These functions are wrappers around `geom_vector()` and `stat_vector()`, ensuring
#' that `color = after_stat(norm)` and `length = after_stat(NA)` are applied by default.
#'
#' @inheritParams geom_vector
#' @param mapping Aesthetic mappings created by `aes()` or `aes_()`.
#'   These functions ensure that `color = after_stat(norm)` is mapped by default.
#' @param ... Other arguments passed on to `geom_vector()` or `stat_vector()`.
#' @return A ggplot2 layer that can be added to a ggplot object.
#' @examples
#'
#' # Example with Cartesian Data
#' set.seed(1234)
#' n <- 10
#' wind_data <- data.frame(
#'   lon = rnorm(n),
#'   lat = rnorm(n),
#'   wind_dir = runif(n, -pi, pi),
#'   wind_spd = rchisq(n, df = 2)
#' )
#' wind_data$dx <- wind_data$wind_spd * cos(wind_data$wind_dir)
#' wind_data$dy <- wind_data$wind_spd * sin(wind_data$wind_dir)
#'
#' ggplot(wind_data) +
#'   geom_vector2(aes(x = lon, y = lat, dx = dx, dy = dy)) +
#'   scale_color_viridis_c() +
#'   theme_minimal() +
#'   labs(color = "Vector Magnitude")
#'
#' # Example with Polar Coordinates
#' ggplot(wind_data) +
#'   geom_vector2(aes(x = lon, y = lat, angle = wind_dir, distance = wind_spd)) +
#'   scale_color_viridis_c() +
#'   theme_minimal() +
#'   labs(color = "Vector Magnitude")
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
  mapping <- modifyList(aes(color = after_stat(norm), length = after_stat(NA)), mapping)
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
  mapping <- modifyList(aes(color = after_stat(norm), length = after_stat(NA)), mapping)
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
