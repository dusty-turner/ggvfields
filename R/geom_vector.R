#' Create a Vector Plot Layer
#'
#' `geom_vector` generates a ggplot layer that visualizes vectors as line
#' segments with optional arrowheads. The vectors are defined by start (`x`,
#' `y`) and end (`xend`, `yend`) coordinates, which can be directly provided or
#' derived from angular (`angle`) and distance (`distance`) information.
#'
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::stat_identity
#' @param center Logical; if `TRUE`, centers the vector on the specified (`x`,
#'   `y`) location. If `FALSE`, the vector origin is at the specified (`x`, `y`)
#'   location. When centering is enabled, the vector's midpoint aligns with the
#'   original (`x`, `y`) location.
#' @param normalize Logical; if `TRUE`, normalizes each vector to a unit length
#'   before applying any scaling. This ensures that all vectors have the same
#'   length, which is then adjusted by `scale_length`. Normalization is useful
#'   for avoiding overplotting and ensuring visual consistency, especially in
#'   dense plots.
#' @param scale_length Numeric; a scaling factor applied to the vectors to
#'   adjust their length. Defaults to `1`. This value is applied after
#'   normalization (if `normalize = TRUE`), allowing you to control the final
#'   vector lengths.
#' @param arrow Arrow specification, as created by `grid::arrow()`. This
#'   controls the appearance of the arrowheads at the end of the vectors,
#'   including properties like angle, length, and type.
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a
#'   vector plot.
#' @name geom_vector
#' @rdname geom_vector
#' @examples
#'
#' # Example using Cartesian input: precomputed xend and yend
#' set.seed(1234)
#' n <- 10
#' wind_data_polar <- data.frame(
#'   lon = rnorm(n),
#'   lat = rnorm(n),
#'   wind_dir = runif(n, -pi, pi),
#'   wind_spd = rchisq(n, df = 2)
#' )
#'
#' wind_data_cartesian <- within(wind_data_polar, {
#'   wind_lon_comp <- wind_spd * cos(wind_dir)
#'   wind_lat_comp <- wind_spd * sin(wind_dir)
#'   xend <- lon + wind_lon_comp
#'   yend <- lat + wind_lat_comp
#' })
#'
#' ggplot(wind_data_cartesian) +
#'   geom_vector(aes(x = lon, y = lat, xend = xend, yend = yend)) +
#'   labs(title = "Wind Vectors (Cartesian Input)",
#'        x = "Longitude", y = "Latitude")
#'
#' # Example using Polar input: angle (wind_dir) and distance (wind_spd)
#' ggplot(wind_data_polar) +
#'   geom_vector(aes(x = lon, y = lat, angle = wind_dir * 180 / pi, distance = wind_spd)) +
#'   labs(title = "Wind Vectors (Polar Input)",
#'        x = "Longitude", y = "Latitude")
#'
#'
#' @section Computed variables:
#'
#' \describe{
#'   \item{norm}{The magnitude of each vector, calculated as \eqn{\|\mathbf{v}\| = \sqrt{(xend - x)^2 + (yend - y)^2}}.}
#' }
NULL

#' @rdname geom_vector
#' @export
geom_vector <- function(mapping = NULL, data = NULL, stat = "vector",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, center = TRUE, normalize = TRUE,
                        scale_length = 1,
                        arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                        ...) {

  layer(
    stat = StatVector,
    data = data,
    mapping = mapping,
    geom = GeomVector,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      center = center,
      normalize = normalize,
      scale_length = scale_length,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_vector
#' @export
stat_vector <- function(mapping = NULL, data = NULL, geom = "vector",
                        position = "identity", na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE,
                        center = TRUE, normalize = TRUE,
                        scale_length = 1, arrow_size = 1,
                        arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                        ...) {

  layer(
    stat = StatVector,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      center = center,
      normalize = normalize,
      scale_length = scale_length,
      arrow = arrow,
      arrow_size = arrow_size,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_vector
#' @export
StatVector <- ggproto("StatVector", Stat,

  required_aes = c("x", "y"),

  optional_aes = c("xend", "yend", "angle", "distance"),

  compute_group = function(data, scales, normalize = TRUE, scale_length = 1, center, ...) {

    # Check if angle and distance are provided and calculate xend and yend
    if (!("xend" %in% names(data) && "yend" %in% names(data))) {
      if ("angle" %in% names(data) && "distance" %in% names(data)) {
        data$angle <- data$angle * pi / 180  # Convert angle to radians
        data$xend <- data$x + data$distance * cos(data$angle)
        data$yend <- data$y + data$distance * sin(data$angle)
      } else {
        stop("Either xend/yend or angle/distance must be provided.")
      }
    }

    # Calculate the norm of the vectors
    data$norm <- sqrt((data$xend - data$x)^2 + (data$yend - data$y)^2)

    # Normalize if required
    if (normalize) {
      data$u <- data$xend - data$x
      data$v <- data$yend - data$y

      data$u <- data$u / data$norm
      data$v <- data$v / data$norm

      data$u <- data$u * scale_length
      data$v <- data$v * scale_length

      data$xend <- data$x + data$u
      data$yend <- data$y + data$v
    }

    return(data)
  }
)

#' @rdname geom_vector
#' @export
GeomVector <- ggproto("GeomVector", GeomSegment,

  required_aes = c("x", "y"),

  optional_aes = c("xend", "yend", "angle", "distance"),

  default_aes = aes(color = "black", linewidth = 0.5, linetype = 1, alpha = 1),

  draw_panel = function(data, panel_params, coord, arrow = NULL, arrow_size = 1, center) {

    # Handle centering if specified
    if (center) {
      half_u <- (data$xend - data$x) / 2
      half_v <- (data$yend - data$y) / 2

      data$x <- data$x - half_u
      data$y <- data$y - half_v
      data$xend <- data$xend - half_u
      data$yend <- data$yend - half_v
    }

    # Add vector length to the data
    data$vector_length <-  data$vector_length <- sqrt((data$xend - data$x)^2 + (data$yend - data$y)^2)

    # Scale arrow size based on vector length, with a sensible scaling factor
    max_arrow_size <- 0.05  # Maximum size cap for arrows
    scaling_factor <- 0.02  # Base scaling factor
    data$arrow_size <- pmin(scaling_factor * data$vector_length, max_arrow_size)

    arrow <- modifyList(arrow, list(length = unit(data$arrow_size, "npc")))

    # Call GeomSegment's draw_panel function
    GeomSegment$draw_panel(data, panel_params, coord, arrow = arrow)
  }
)
