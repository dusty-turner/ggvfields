#' Create a Vector Plot Layer
#'
#' `geom_vector` generates a ggplot layer that visualizes vectors as line
#' segments with optional arrowheads. The vectors are defined by start (`x`, `y`)
#' and end (`xend`, `yend`) coordinates, which can be directly provided or derived
#' from angular (`angle`) and distance (`distance`) information.
#'
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::stat_identity
#' @param fun A function used to calculate vector fields (curl/divergence).
#' @param size Size of the vector arrows.
#' @param center Logical; if `TRUE`, centers the vector on the specified (`x`, `y`) location.
#'   If `FALSE`, the vector origin is at the specified (`x`, `y`) location.
#'   When centering is enabled, the vector's midpoint aligns with the original (`x`, `y`) location.
#' @param normalize Logical; if `TRUE`, normalizes each vector to a unit length before
#'   applying any scaling. Normalization is useful for avoiding overplotting and ensuring
#'   visual consistency, especially in dense plots.
#' @param add_points Logical; if `TRUE`, adds a point at the start of each vector.
#' @param arrow Arrow specification for vector arrowheads, created by `grid::arrow()`.
#'   This controls the appearance of the arrowheads at the end of the vectors, including properties like angle, length, and type.
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a vector plot.
#' @name geom_vector
#' @rdname geom_vector
#' @export
#'
#' @section Aesthetics:
#' `geom_vector` understands the following aesthetics (required aesthetics are in bold):
#' - **`x`**: x-coordinate of the start point of the vector.
#' - **`y`**: y-coordinate of the start point of the vector.
#' - `xend`: x-coordinate of the end point of the vector (optional if `angle` and `distance` are provided).
#' - `yend`: y-coordinate of the end point of the vector (optional if `angle` and `distance` are provided).
#' - `angle`: The angle of the vector in degrees (optional, used with `distance`).
#' - `distance`: The distance/magnitude of the vector (optional, used with `angle`).
#' - `length`: The length of the vector (optional, overrides `distance` when mapped as an aesthetic).
#' - `color`: The color of the vector line.
#' - `fill`: The fill color of vector arrowheads and points.
#' - `linewidth`: The thickness of the vector line.
#' - `linetype`: The type of the vector line (solid, dashed, etc.).
#' - `alpha`: The transparency level of the vector.
#' - `arrow`: Specification for arrowheads at the end of the vector.
#'
#' @examples
#'
#' # Example using Cartesian input: precomputed dx and dy
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
#'   dx <- wind_lon_comp  # dx represents the change in x (longitude component)
#'   dy <- wind_lat_comp  # dy represents the change in y (latitude component)
#' })
#'
#' ggplot(wind_data_cartesian) +
#'   geom_vector(aes(x = lon, y = lat, dx = dx, dy = dy)) +
#'   labs(title = "Wind Vectors (Cartesian Input)",
#'        x = "Longitude", y = "Latitude")
#'
#' # Example using Polar input: angle (wind_dir) and distance (wind_spd)
#' ggplot(wind_data_polar) +
#'   geom_vector(aes(x = lon, y = lat, angle = wind_dir, distance = wind_spd)) +
#'   labs(title = "Wind Vectors (Polar Input)",
#'        x = "Longitude", y = "Latitude")
#'
#' @section Computed variables:
#' \describe{
#'   \item{norm}{The magnitude of each vector, calculated as \eqn{\|\mathbf{v}\| = \sqrt{(xend - x)^2 + (yend - y)^2}}.}
#' }
NULL

#' @rdname geom_vector
#' @export
geom_vector <- function(mapping = NULL, data = NULL, stat = StatVector,
                        position = "identity", ..., na.rm = FALSE, show.legend = NA,
                        arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                        inherit.aes = TRUE, center = TRUE, normalize = FALSE, add_points = FALSE,
                        fun = NULL) {
  layer(
    stat = StatVector, geom = GeomVector, mapping = mapping, data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arrow = arrow, center = center, normalize = normalize, add_points = add_points, fun = fun, ...)
  )
}


#' @rdname geom_vector
#' @export
stat_vector <- function(mapping = NULL, data = NULL, geom = GeomVector,
                        position = "identity", ..., na.rm = FALSE, show.legend = NA,
                        arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                        inherit.aes = TRUE, center = TRUE, normalize = FALSE, add_points = FALSE) {
  layer(
    stat = StatVector, geom = geom, mapping = mapping, data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arrow = arrow, center = center, normalize = normalize, add_points = add_points, ...)
  )
}

#' @rdname geom_vector
#' @export
StatVector <- ggproto("StatVector", Stat,
                      required_aes = c("x", "y"),
                      default_aes = aes(dx = NA, dy = NA, distance = NA, angle = NA, length = NA,
                                        color = "black", fill = "black", linewidth = 0.5, linetype = 1, alpha = 1),
                      compute_group = function(data, scales, center = FALSE, fun = NULL, ...) {

                        # If a function is provided, calculate curl and divergence
                        if (!is.null(fun)) {
                          # Calculate the gradient for each point
                          grad <- apply(data[, c("x", "y")], 1, function(v) numDeriv::grad(fun, v)) |> t()
                          grad_u <- grad[, 1]
                          grad_v <- grad[, 2]

                          # Calculate divergence and curl
                          data$divergence <- grad_u + grad_v
                          data$curl <- grad_v - grad_u
                        }

                        # If dx and dy are provided directly, use them
                        if (all(is.na(data$dx)) | all(is.na(data$dy))) {
                          if (!is.na(data$distance[1]) && !is.na(data$angle[1])) {
                            data$dx <- data$distance * cos(data$angle)
                            data$dy <- data$distance * sin(data$angle)
                          } else {
                            stop("Either dx/dy or distance/angle must be provided.")
                          }
                        }

                        data$xend <- data$x + data$dx
                        data$yend <- data$y + data$dy

                        data$norm <- sqrt(data$dx^2 + data$dy^2)

                        return(data)
                      }
)

#' @keywords internal
draw_panel_vector <- function(data, panel_params, coord, na.rm = FALSE, arrow = NULL, center = FALSE, normalize = FALSE, add_points = FALSE) {

  # If length is not mapped, normalize and center using the original data before transformation
  if (is.na(data$length[1])) {

    # Now transform the modified data into the coordinate system
    coords <- coord$transform(data, panel_params)

    # Create the vector grob
    vector_grob <- grid::segmentsGrob(
      x0 = unit(coords$x, "npc"), y0 = unit(coords$y, "npc"),
      x1 = unit(coords$xend, "npc"), y1 = unit(coords$yend, "npc"),
      gp = grid::gpar(col = coords$colour, fill = coords$fill, lwd = 2),  # Use mapped fill
      arrow = arrow  # Pass the arrow parameter here
    )

    points_grob <- NULL
    if (add_points) {
      points_grob <- grid::pointsGrob(
        x = unit(coords$x, "npc"),  # The starting point (adjusted by centering)
        y = unit(coords$y, "npc"),
        pch = 16,  # Solid circle
        size = unit(2, "mm"),
        gp = grid::gpar(col = coords$colour, fill = coords$fill)  # Use mapped fill
      )
    }

    # Combine vector and points grobs
    grobs <- list(vector_grob, points_grob)
    grobs <- Filter(Negate(is.null), grobs)  # Remove NULL entries
    return(grid::grobTree(do.call(grid::gList, grobs)))

  } else {
    # When length is mapped, proceed as usual
    # Display a message and ignore normalization if normalize = TRUE
    if (normalize) {
      message("Normalization is ignored because length is mapped using after_stat().")
    }

    # data$y <- data$y * 2
    # data$x <- data$x * 2
    # data$xend <- data$xend * 2
    # data$yend <- data$yend * 2

    # Transform data into the coordinate system
    coords <- coord$transform(data, panel_params)

    # Normalize dx and dy to unit vectors (for direction)
    norms <- sqrt(coords$dx^2 + coords$dy^2)
    norms[norms == 0] <- 1  # Avoid division by zero
    coords$dx <- coords$dx / norms
    coords$dy <- coords$dy / norms

    # Calculate the midpoint of the vector for centering
    half_dx <- (coords$length / 2) * coords$dx
    half_dy <- (coords$length / 2) * coords$dy

    # Handle centering
    if (center) {
      vector_grob <- grid::segmentsGrob(
        x0 = unit(coords$x, "npc") - unit(half_dx, "cm"),
        y0 = unit(coords$y, "npc") - unit(half_dy, "cm"),
        x1 = unit(coords$x, "npc") + unit(half_dx, "cm"),
        y1 = unit(coords$y, "npc") + unit(half_dy, "cm"),
        gp = grid::gpar(col = coords$colour, fill = coords$fill, lwd = 2),  # Use mapped fill
        arrow = arrow  # Pass the arrow parameter here
      )
      points_grob <- NULL
      if (add_points) {
        points_grob <- grid::pointsGrob(
          x = unit(coords$x, "npc") - unit(half_dx, "cm"),  # The starting point (adjusted by centering)
          y = unit(coords$y, "npc") - unit(half_dy, "cm"),
          pch = 16,  # Solid circle
          size = unit(2, "mm"),
          gp = grid::gpar(col = coords$colour, fill = coords$fill)  # Use mapped fill
        )
      }
    } else {
      vector_grob <- grid::segmentsGrob(
        x0 = unit(coords$x, "npc"), y0 = unit(coords$y, "npc"),
        x1 = unit(coords$x, "npc") + unit(coords$length * coords$dx, "cm"),
        y1 = unit(coords$y, "npc") + unit(coords$length * coords$dy, "cm"),
        gp = grid::gpar(col = coords$colour, fill = coords$fill, lwd = 2),  # Use mapped fill
        arrow = arrow  # Pass the arrow parameter here
      )
      points_grob <- NULL
      if (add_points) {
        points_grob <- grid::pointsGrob(
          x = unit(coords$x, "npc"),  # The starting point (adjusted by centering)
          y = unit(coords$y, "npc"),
          pch = 16,  # Solid circle
          size = unit(2, "mm"),
          gp = grid::gpar(col = coords$colour, fill = coords$fill)  # Use mapped fill
        )
      }
    }

    # Combine vector and points grobs
    grobs <- list(vector_grob, points_grob)
    grobs <- Filter(Negate(is.null), grobs)  # Remove NULL entries
    return(grid::grobTree(do.call(grid::gList, grobs)))
  }
}

#' @rdname geom_vector
#' @keywords internal
draw_key_vector <- function(data, params, size) {
  dx <- data$dx
  dy <- data$dy

  x0 <- unit(0.1, "npc")
  y0 <- unit(0.5, "npc")

  length_value <- data$length
  x1 <- x0 + unit(length_value, "cm")
  y1 <- y0

  grid::segmentsGrob(
    x0 = x0, y0 = y0,
    x1 = x1, y1 = y1,
    gp = grid::gpar(col = data$colour, lwd = 2)
  )
}

#' @rdname geom_vector
#' @export
scale_length_continuous <- function(...) {
  continuous_scale(
    aesthetics = "length",
    palette = scales::rescale_pal(c(0.1, .5)),
    ...
  )
}

#' @rdname geom_vector
#' @export
GeomVector <- ggproto(
  "GeomVector", Geom,

  setup_data = function(data, params){

  # if (is.na(data$length[1])) {
  if (is.null(data$length[1])) {
    # Normalize dx and dy to unit vectors if normalize is TRUE

    if (params$normalize) {
      norms <- sqrt(data$dx^2 + data$dy^2)
      norms[norms == 0] <- 1  # Avoid division by zero
      data$dx <- data$dx / norms
      data$dy <- data$dy / norms

      # Recalculate xend and yend after normalization
      data$xend <- data$x + data$dx
      data$yend <- data$y + data$dy
    }

    # Handle centering if requested (using the original data)
    if (params$center) {
      # Calculate midpoint for centering the vector (using data, not coords)
      half_dx <- (data$xend - data$x) / 2
      half_dy <- (data$yend - data$y) / 2

      # Adjust the original data to center the vector around its midpoint
      data$x <- data$x - half_dx
      data$y <- data$y - half_dy
      data$xend <- data$xend - half_dx
      data$yend <- data$yend - half_dy
    }
  } else {
     data$y <- data$y / 2
     data$x <- data$x / 2
     data$xend <- data$xend / 2
     data$yend <- data$yend / 2

  }


    return(data)
  },

  required_aes = c("x", "y"),
  default_aes = aes(colour = "black", fill = "black", size = 0.5, length = NA, linewidth = 0.5, linetype = 1, alpha = 1),
  draw_group = draw_panel_vector,
  draw_key = draw_key_vector
)
