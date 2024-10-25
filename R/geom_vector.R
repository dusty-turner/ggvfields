#' Create a Vector Plot Layer
#'
#' `geom_vector` generates a ggplot layer that visualizes vectors as line
#' segments with optional arrowheads. Vectors are defined by their start (`x`, `y`)
#' and directional components (`dx`, `dy`), which indicate the change in position.
#' Alternatively, vectors can be defined by angular (`angle`) and distance (`distance`) information.
#'
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::stat_identity
#' @param fun A function applied to vector data if additional transformations are needed.
#' @param center Logical; if `TRUE`, centers the vector on the specified (`x`, `y`) location.
#'   If `FALSE`, the vector starts at the specified (`x`, `y`) point. When centering is enabled,
#'   the vector's midpoint aligns with the original (`x`, `y`) location.
#' @param normalize Logical; if `TRUE`, normalizes each vector to a unit length before applying any scaling.
#'   This prevents overplotting and ensures consistent visual presentation in dense plots.
#' @param tail_point Logical; if `TRUE`, adds a point to mark the tail of each vector.
#' @param tail_point.size Integer; sets the size of the tail point if `tail_point = TRUE`.
#' @param arrow Arrow specification for vector arrowheads, created with `grid::arrow()`.
#'   Controls the appearance of arrowheads, including angle, length, and type.
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a vector plot.
#' @name geom_vector
#' @section Aesthetics:
#' `geom_vector` understands the following aesthetics (required aesthetics are in **bold**):
#' - **`x`**: x-coordinate of the start point of the vector.
#' - **`y`**: y-coordinate of the start point of the vector.
#' - **`dx`**: Change in the x-direction (length component along the x-axis).
#' - **`dy`**: Change in the y-direction (length component along the y-axis).
#' - `angle`: The angle of the vector in degrees (optional, used with `distance`).
#' - `distance`: The magnitude of the vector (optional, used with `angle`).
#' - `length`: Length of the vector (default: `after_stat(NA)`).
#'   - You can also manually scale vector length by using `aes(length = after_stat(norm))`.
#'   - Alternatively, use `geom_vector2()` to map vector magnitude (`norm`) to length automatically.
#' - `color`: **By default, `color = after_stat(norm)`** to reflect vector magnitude.
#' - `fill`: Fill color for arrowheads and points.
#' - `linewidth`: Thickness of the vector line.
#' - `linetype`: Type of line (solid, dashed, etc.).
#' - `alpha`: Transparency level of the vector.
#' - `arrow`: Arrowhead specification for the vector.
#'
#' @seealso
#' Use [geom_vector2()] for automatically mapping the vector's magnitude (`norm`) to length.
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
#'   geom_vector(aes(x = lon, y = lat, dx = dx, dy = dy))
#'
#' # Example using Polar coordinates: angle and distance
#' ggplot(wind_data) +
#'   geom_vector(aes(x = lon, y = lat, angle = wind_dir, distance = wind_spd))
#'
#' # To scale vector length by magnitude, use geom_vector2():
#' ggplot(wind_data) +
#'   geom_vector2(aes(x = lon, y = lat, dx = dx, dy = dy))
#'
#' # Or manually set length mapping with after_stat():
#' ggplot(wind_data) +
#'   geom_vector(aes(x = lon, y = lat, dx = dx, dy = dy, length = after_stat(norm)))
#'
#' @section Key Notes:
#' - **Default Color Mapping**:
#'   - The default behavior maps the vector's magnitude (`norm`) to the `color` aesthetic.
#'   - This ensures that vector strength is visually emphasized through color.
#'   - To override this behavior, you can set `aes(color = "black")` or another fixed color.
#'
#' - **Scaling by Length**:
#'   - By default, vector length is not scaled (`length = after_stat(NA)`).
#'   - You can manually enable length scaling by using `aes(length = after_stat(norm))`.
#'   - Alternatively, use the `geom_vector2()` function for automatic length scaling based on magnitude.
#'
#' @section Computed Variables:
#' \describe{
#'   \item{norm}{The magnitude of each vector, calculated as \eqn{\|\mathbf{v}\| = \sqrt{dx^2 + dy^2}}.}
#' }

#' @rdname geom_vector
#' @export
geom_vector <- function(
  mapping = NULL,
  data = NULL,
  stat = StatVector,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed"),
  inherit.aes = TRUE,
  center = TRUE,
  normalize = TRUE,
  tail_point = FALSE,
  tail_point.size = 2,
  fun = NULL
) {

  mapping <- modifyList(aes(color = after_stat(norm), length = after_stat(NA)), mapping)


  layer(
    stat = StatVector, geom = GeomVector, mapping = mapping, data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arrow = arrow, center = center, normalize = normalize,
                  tail_point = tail_point, tail_point.size = tail_point.size, fun = fun, ...)
  )
}


#' @rdname geom_vector
#' @export
stat_vector <- function(
  mapping = NULL,
  data = NULL,
  geom = GeomVector,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed"),
  inherit.aes = TRUE,
  center = TRUE,
  normalize = TRUE,
  tail_point = FALSE,
  tail_point.size = 2
) {

  mapping <- modifyList(aes(color = after_stat(norm), length = after_stat(NA)), mapping)


  layer(
    stat = StatVector, geom = geom, mapping = mapping, data = data,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, arrow = arrow, center = center, normalize = normalize,
                  tail_point = tail_point, tail_point.size = tail_point.size, ...)
  )
}


#' @rdname geom_vector
#' @export
StatVector <- ggproto(
  "StatVector",
  Stat,
  required_aes = c("x", "y"),
  default_aes = aes(dx = NA, dy = NA, distance = NA, angle = NA, length = 1,
                    color = "black", fill = "black", linewidth = 2, linetype = 1, alpha = 1),

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
draw_panel_vector <- function(
  data,
  panel_params,
  coord,
  na.rm = FALSE,
  arrow = NULL,
  center = TRUE,
  normalize = TRUE,
  tail_point = FALSE,
  tail_point.size = 2,
  linewidth = 2
) {

  # If length is not mapped, normalize and center using the original data before transformation
  if (is.na(data$length[1])) {

    # Now transform the modified data into the coordinate system
    coords <- coord$transform(data, panel_params)

    # Create the vector grob
    vector_grob <- grid::segmentsGrob(
      x0 = unit(coords$x, "npc"), y0 = unit(coords$y, "npc"),
      x1 = unit(coords$xend, "npc"), y1 = unit(coords$yend, "npc"),
      gp = grid::gpar(col = coords$colour, fill = coords$colour, lwd = linewidth),
      arrow = arrow
    )

    points_grob <- NULL
    if (tail_point) {
      points_grob <- grid::pointsGrob(
        x = unit(coords$x, "npc"),  # The starting point (adjusted by centering)
        y = unit(coords$y, "npc"),
        pch = 16,  # Solid circle
        size = unit(tail_point.size, "mm"),
        gp = grid::gpar(col = coords$colour, fill = coords$fill)
      )
    }

    # Combine vector and points grobs
    grobs <- list(vector_grob, points_grob)
    grobs <- Filter(Negate(is.null), grobs)  # Remove NULL entries
    return(grid::grobTree(do.call(grid::gList, grobs)))

  } else {
    # When length is mapped, proceed as usual
    # Display a message and ignore normalization if normalize = TRUE
    # if (normalize) {
    #   message("Note: `normalize = TRUE` does not affect `dx` and `dy` when the `length` aesthetic is mapped.\nEnsure your `length` values reflect the intended scaling.")
    # }

    #### Untransform the data here ####
    # Reverse the transformation done in setup_data to get the original data back

    # 1. Undo centering if it was applied
    if (center) {
      half_dx <- (data$xend - data$x) / 2
      half_dy <- (data$yend - data$y) / 2

      # Restore the original positions by adding back the half_dx and half_dy
      data$x <- data$x + half_dx
      data$y <- data$y + half_dy
      data$xend <- data$xend + half_dx
      data$yend <- data$yend + half_dy
    }

    # 2. Undo the length scaling
    data$dx <- data$xend - data$x  # Calculate dx from x and xend
    data$dy <- data$yend - data$y  # Calculate dy from y and yend

    # Normalize dx and dy (undoing the length scaling)
    norms <- sqrt(data$dx^2 + data$dy^2)
    norms[norms == 0] <- 1  # Avoid division by zero

    data$dx <- data$dx / data$length / .01  # Undo the multiplication by length
    data$dy <- data$dy / data$length / .01  # Undo the multiplication by length

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

      ## this determins if the user has adjusted the length of the arrow away from the default and makes it smaller
      ## if the user has altered the default then leave it alone
      if (!is.null(arrow) && round(grid::convertUnit(arrow$length, "npc", valueOnly = TRUE), 3) == 0.025) {
        arrow$length <- unit(0.015, "npc")
      }

      vector_grob <- grid::segmentsGrob(
        x0 = unit(coords$x, "npc") - unit(half_dx, "cm"),
        y0 = unit(coords$y, "npc") - unit(half_dy, "cm"),
        x1 = unit(coords$x, "npc") + unit(half_dx, "cm"),
        y1 = unit(coords$y, "npc") + unit(half_dy, "cm"),
        gp = grid::gpar(col = coords$colour, fill = coords$colour, lwd = linewidth),  # Use mapped fill
        arrow = arrow  # Pass the arrow parameter here
      )
      points_grob <- NULL
      if (tail_point) {
        points_grob <- grid::pointsGrob(
          x = unit(coords$x, "npc") - unit(half_dx, "cm"),  # The starting point (adjusted by centering)
          y = unit(coords$y, "npc") - unit(half_dy, "cm"),
          pch = 16,  # Solid circle
          size = unit(2, "mm"),
          gp = grid::gpar(col = coords$colour, fill = coords$fill)  # Use mapped fill
        )
      }
    } else {

      ## this determins if the user has adjusted the length of the arrow away from the default and makes it smaller
      ## if the user has altered the default then leave it alone
      if (!is.null(arrow) && round(grid::convertUnit(arrow$length, "npc", valueOnly = TRUE), 3) == 0.025) {
        arrow$length <- unit(0.015, "npc")
      }
      vector_grob <- grid::segmentsGrob(
        x0 = unit(coords$x, "npc"), y0 = unit(coords$y, "npc"),
        x1 = unit(coords$x, "npc") + unit(coords$length * coords$dx, "cm"),
        y1 = unit(coords$y, "npc") + unit(coords$length * coords$dy, "cm"),
        gp = grid::gpar(col = coords$colour, fill = coords$colour, lwd = linewidth),  # Use mapped fill
        arrow = arrow
      )
      points_grob <- NULL
      if (tail_point) {
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


#' @keywords internal
draw_key_vector <- function(data, params, size, linewidth) {
  x0 <- unit(0.1, "npc")
  y0 <- unit(0.5, "npc")

  length_value <- data$length
  x1 <- rev(x0 + unit(length_value, "cm"))
  y1 <- rev(y0)

  grid::segmentsGrob(
    x0 = x0, y0 = y0,
    x1 = x1, y1 = y1,
    gp = grid::gpar(col = data$colour, lwd = data$linewidth)
  )
}


#' @rdname geom_vector
#' @export
GeomVector <- ggproto(
  "GeomVector",
  Geom,
  required_aes = c("x", "y"),
  default_aes = aes(color = "black", fill = "black", size = 0.5, length = 1, linewidth = 2, linetype = 1, alpha = 1),

  setup_data = function(data, params){

    # print(params)
    #
    # inherits(params$length, "after_stat")
    #
    # if (!is.na(data$length[1]) && params$normalize) {
    #   message("Note: `normalize = TRUE` does not affect `dx` and `dy` when the `length` aesthetic is mapped.\nEnsure your `length` values reflect the intended scaling.")
    # }



  if (is.na(data$length[1])) {

    # Normalize dx and dy to unit vectors if normalize is TRUE

    if (params$normalize) {

      # norms <- sqrt(data$dx^2 + data$dy^2)
      # norms[norms == 0] <- 1  # Avoid division by zero
      # data$dx <- data$dx / norms
      # data$dy <- data$dy / norms
      #
      # # Recalculate xend and yend after normalization
      # data$xend <- data$x + data$dx
      # data$yend <- data$y + data$dy

      # Detect if the data forms a regular grid by checking unique x and y spacings
      x_spacing <- unique(diff(sort(unique(data$x))))
      y_spacing <- unique(diff(sort(unique(data$y))))

      # Calculate the minimum spacing or default to 1 if not a grid
      min_spacing <- if (all(abs(x_spacing - mean(x_spacing)) < 1e-6) &&
                         all(abs(y_spacing - mean(y_spacing)) < 1e-6)) {
        min(x_spacing, y_spacing) * .9
      } else {
        1  # No scaling for non-grid data
      }

      # Normalize the vectors to unit length and scale by the minimum spacing
      norms <- sqrt(data$dx^2 + data$dy^2)
      norms[norms == 0] <- 1  # Avoid division by zero
      data$dx <- (data$dx / norms) * min_spacing
      data$dy <- (data$dy / norms) * min_spacing

      # Recalculate xend and yend after normalization/scaling
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

    # If length aesthetic is mapped

    # 1. Normalize dx and dy to unit vectors (like in draw_panel)
    norms <- sqrt(data$dx^2 + data$dy^2)
    norms[norms == 0] <- 1  # Avoid division by zero
    data$dx <- data$dx / norms
    data$dy <- data$dy / norms

    # 2. Multiply dx and dy by the length aesthetic
    data$dx <- data$dx * data$length * .01
    data$dy <- data$dy * data$length * .01

    # 3. Recalculate xend and yend based on the new dx and dy
    data$xend <- data$x + data$dx
    data$yend <- data$y + data$dy

    # 4. Handle centering if requested
    if (params$center) {
      half_dx <- (data$xend - data$x) / 2
      half_dy <- (data$yend - data$y) / 2

      # Adjust the original data to center the vector around its midpoint
      data$x <- data$x - half_dx
      data$y <- data$y - half_dy
      data$xend <- data$xend - half_dx
      data$yend <- data$yend - half_dy
    }

  }
    return(data)
  },

  required_aes = c("x", "y"),
  draw_group = draw_panel_vector,
  draw_key = draw_key_vector
)


#' Create a Continuous Scale for Vector Length
#'
#' `scale_length_continuous` provides a continuous scale for controlling the length
#' aesthetic in a ggplot. This is particularly useful when working with vector plots
#' where vector lengths are mapped to a continuous scale.
#'
#' @param ... Other arguments passed to `continuous_scale()`.
#' @export
scale_length_continuous <- function(...) {
  continuous_scale(
    aesthetics = "length",
    palette = scales::rescale_pal(c(0.1, .5)),
    ...
  )
}

