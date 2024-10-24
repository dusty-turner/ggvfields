#' Create a Vector Plot Layer with Norm Mapped to Color
#'
#' `geom_vector2` creates a ggplot2 layer that visualizes vectors as line segments with
#' optional arrowheads. By default, it maps the vector norm (magnitude) to the `color` aesthetic.
#'
#' This function is a wrapper around `geom_vector()` and behaves identically, except for
#' the default aesthetic mapping of `color = after_stat(norm)`.
#'
#' @inheritParams geom_vector
#' @param mapping Set of aesthetic mappings created by `aes()` or `aes_()`.
#'   This function ensures that `color = after_stat(norm)` is mapped by default.
#' @param ... Other arguments passed on to `geom_vector()`.
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a vector plot
#'   with `norm` mapped to `color`.
#' @examples
#'
#' # Example 1: Basic Cartesian Vector Plot with Magnitude Mapped to Color
#' set.seed(1234)
#' n <- 10
#' wind_data <- data.frame(
#'   lon = rnorm(n),                     # Longitude coordinates
#'   lat = rnorm(n),                     # Latitude coordinates
#'   wind_dir = runif(n, -pi, pi),       # Wind direction in radians
#'   wind_spd = rchisq(n, df = 2)        # Wind speed (magnitude)
#' )
#'
#' # Add Cartesian components (dx, dy)
#' wind_data$dx <- wind_data$wind_spd * cos(wind_data$wind_dir)  # x-component
#' wind_data$dy <- wind_data$wind_spd * sin(wind_data$wind_dir)  # y-component
#'
#' # Plot with geom_vector2 (color mapped to vector norm by default)
#' ggplot(wind_data) +
#'   geom_vector2(aes(x = lon, y = lat, dx = dx, dy = dy)) +
#'   scale_color_viridis_c() +
#'   theme_minimal() +
#'   labs(color = "Vector Magnitude")
#'
#' # Example 2: Polar Coordinates with Angle and Distance
#' ggplot(wind_data) +
#'   geom_vector2(aes(x = lon, y = lat, angle = wind_dir, distance = wind_spd)) +
#'   scale_color_viridis_c() +
#'   theme_minimal() +
#'   labs(color = "Vector Magnitude")
#'
#' # Example 3: Customizing Arrow Appearance
#' ggplot(wind_data) +
#'   geom_vector2(
#'     aes(x = lon, y = lat, dx = dx, dy = dy),
#'     arrow = grid::arrow(angle = 15, length = unit(0.02, "npc"), type = "closed")
#'   ) +
#'   scale_color_viridis_c() +
#'   theme_minimal() +
#'   labs(color = "Vector Magnitude")
#'
#' # Example 4: Disabling Length Mapping and Using Custom Lengths
#' wind_data$length_custom <- runif(n, 0.5, 1.5)  # Custom lengths for each vector
#' ggplot(wind_data) +
#'   geom_vector2(
#'     aes(x = lon, y = lat, dx = dx, dy = dy, length = length_custom)
#'   ) +
#'   scale_color_viridis_c() +
#'   theme_minimal() +
#'   labs(color = "Vector Magnitude")
#'
#' @export

# Wrapper around geom_vector to change the default mapping
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
  # Ensure norm is mapped to color by default
  mapping <- modifyList(aes(color = after_stat(norm), length = after_stat(NA)), mapping)

  # Call the original geom_vector with modified mapping
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

# Wrapper around stat_vector to change the default mapping
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
  # Ensure norm is mapped to color by default
  mapping <- modifyList(aes(color = after_stat(norm), length = after_stat(NA)), mapping)

  # Call the original stat_vector with modified mapping
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
