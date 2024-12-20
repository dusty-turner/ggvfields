#' Create a Vector Plot Layer
#'
#' `geom_vector()` generates a ggplot layer that visualizes vectors as line segments
#' with optional arrowheads. Vectors are defined by their start coordinates (`x`, `y`)
#' and either directional components (`dx`, `dy`) or polar coordinates (`angle` and `distance`).
#'
#' This geom is designed for situations where the vector data is already available
#' (e.g., wind directions and speeds at known locations). If you need to generate
#' a vector field from a user-defined function over a specified grid (which can
#' compute `divergence` and `curl`), consider using [`geom_vector_field()`].
#'
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::stat_identity
#'
#' @param center Logical; if `TRUE`, the vector is centered on the (`x`, `y`) location.
#'   If `FALSE`, the vector originates at the (`x`, `y`) location. When centered, the
#'   midpoint of the vector aligns with the original (`x`, `y`) position.
#' @param normalize Logical; if `TRUE`, normalizes each vector to a unit length before
#'   applying any scaling. This can help prevent overplotting in dense plots and
#'   ensures consistent visual representation.
#' @param tail_point Logical; if `TRUE`, adds a point at the tail of each vector to mark
#'   the starting position more clearly.
#' @param tail_point.size Numeric value indicating the size of the tail point if
#'   `tail_point = TRUE`.
#' @param arrow Arrow specification for adding arrowheads to vectors, created with
#'   `grid::arrow()`. Controls arrowhead angle, length, and type.
#'
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a vector plot.
#'
#' @name geom_vector
#'
#' @section Aesthetics:
#' `geom_vector()` understands the following aesthetics (required aesthetics are in **bold**):
#' - **`x`**: The x-coordinate of the vector's start (or center) point.
#' - **`y`**: The y-coordinate of the vector's start (or center) point.
#' - **`dx`**: The vector's x-component of displacement.
#' - **`dy`**: The vector's y-component of displacement.
#' - `angle` (in radians): The angle of the vector, used with `distance`.
#' - `distance`: The magnitude of the vector, used with `angle`.
#' - `length`: The displayed length of the vector on the plot. By default, the vector
#'   is drawn at its actual data-defined size. To modify this, you can:
#'   - Assign a constant length: `aes(length = 0.5)` (for example).
#'   - Scale length by a computed statistic, such as `aes(length = after_stat(norm))` to
#'     represent the vector's magnitude.
#' - `color`: By default, `color = after_stat(norm)` to map the magnitude of each vector
#'   to its color, providing a visual cue of vector strength.
#' - `fill`: Fill color for arrowheads and tail points.
#' - `linewidth`: The thickness of the vector line.
#' - `linetype`: The type of line (e.g., solid, dashed).
#' - `alpha`: The transparency level of the vector.
#'
#' @section Key Notes:
#' - **Default Color Mapping**:
#'   - The default maps vector magnitude (`norm`) to `color`.
#'   - This makes stronger (longer) vectors more visually prominent.
#'   - To override this behavior, specify `aes(color = "black")` or another fixed color.
#'
#' - **Scaling by Length**:
#'   - By default, vectors are drawn at their data-defined size.
#'   - To scale by the vector's magnitude, use `aes(length = after_stat(norm))`.
#'   - For a shortcut, `geom_vector2()` automatically maps magnitude to length.
#'
#' @section Computed Variables:
#' `geom_vector()` computes:
#' - `norm`: The magnitude of each vector, \eqn{\sqrt{dx^2 + dy^2}}.
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
#' # Basic vector plot using dx and dy
#' ggplot(wind_data) +
#'   geom_vector(aes(x = lon, y = lat, dx = dx, dy = dy))
#'
#' # Using angle and distance instead of dx, dy
#' ggplot(wind_data) +
#'   geom_vector(aes(x = lon, y = lat, angle = wind_dir, distance = wind_spd))
#'
#' # To scale vector length by magnitude, use geom_vector2()
#' ggplot(wind_data) +
#'   geom_vector2(aes(x = lon, y = lat, dx = dx, dy = dy))
#'
#' # Manually map length to norm
#' ggplot(wind_data) +
#'   geom_vector(aes(x = lon, y = lat, dx = dx, dy = dy, length = after_stat(norm)))
#'
#' @rdname geom_vector
#' @export
geom_vector <- function(mapping = NULL, data = NULL,
                        stat = StatVector,position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed"),
                        center = TRUE,
                        normalize = TRUE,
                        tail_point = FALSE,
                        tail_point.size = 2) {

  if (is.null(mapping)) {
    mapping <- aes()
  } else if (!is.list(mapping)) {
    stop("mapping must be a list or NULL")
  }
  mapping <- modifyList(aes(color = after_stat(norm), length = after_stat(NA)), mapping)

  layer(
    stat = StatVector,
    geom = GeomVector,
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
      tail_point.size = tail_point.size,
      ...
    )
  )
}


#' @rdname geom_vector
#' @export
stat_vector <- function(mapping = NULL, data = NULL,
                        geom = GeomVector, position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed"),
                        inherit.aes = TRUE,
                        center = TRUE,
                        normalize = TRUE,
                        tail_point = FALSE,
                        tail_point.size = 2) {

  mapping <- modifyList(aes(color = after_stat(norm), length = after_stat(NA)), mapping)

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
      tail_point.size = tail_point.size,
      ...
    )
  )
}


#' @rdname geom_vector
#' @export
StatVector <- ggproto(
  "StatVector",
  Stat,
  # required_aes = character(0), # No required aesthetics to allow flexibility
  default_aes = aes(x = NA, y = NA,
    dx = NA, dy = NA, distance = NA, angle = NA, length = 1,
    color = "black", fill = "black", linewidth = 2, linetype = 1, alpha = 1
  ),

  compute_group = function(data, scales, center = FALSE, fun = NULL, xlim = NULL, ylim = NULL, n = NULL, ...) {

    # Scenario: Using a function to generate the vector field
    if (!is.null(fun)) {
      # If xlim and ylim provided, generate grid from those
      # If not provided, try to infer from data
      if (is.null(xlim) || is.null(ylim)) {
        if (nrow(data) > 0 && all(c("x", "y") %in% names(data))) {
          xlim <- xlim %||% range(data$x, na.rm = TRUE)
          ylim <- ylim %||% range(data$y, na.rm = TRUE)
        } else {
          stop("When using `fun` without specifying aes `x, y` from data, you must supply `xlim` and `ylim` or specify `x, y` using aes()")
        }
      }

      if (is.null(n)) {
        n <- 10
      }

      data <- expand.grid(
        x = seq(xlim[1], xlim[2], length.out = n),
        y = seq(ylim[1], ylim[2], length.out = n)
      )

      vectors <- vectorize(fun)(as.matrix(data))

      data$dx <- vectors[, 1]
      data$dy <- vectors[, 2]

      # Compute divergence and curl
      grad <- apply(data[, c("x", "y")], 1, function(v) numDeriv::grad(fun, v)) |> t()
      grad_u <- grad[, 1]
      grad_v <- grad[, 2]

      data$divergence <- grad_u + grad_v
      data$curl <- grad_v - grad_u

    } else {
      # fun is NULL, expecting user-provided data with x,y and dx,dy or angle/distance
      if (!all(c("x", "y") %in% names(data))) {
        stop("`stat_vector()` requires `x` and `y` aesthetics or a `fun` with `xlim`/`ylim`.")
      }

      # If dx/dy are missing, try angle/distance
      if ((all(is.na(data$dx)) || all(is.na(data$dy))) &&
          (!is.na(data$distance[1]) && !is.na(data$angle[1]))) {
        data$dx <- data$distance * cos(data$angle)
        data$dy <- data$distance * sin(data$angle)
      }

      # Check again if dx,dy are available
      if (all(is.na(data$dx)) | all(is.na(data$dy))) {
        stop("Either dx/dy or distance/angle must be provided.")
      }
    }

    data$xend <- data$x + data$dx
    data$yend <- data$y + data$dy
    data$norm <- sqrt(data$dx^2 + data$dy^2)

    data
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

    vector_coords <- coords[coords$norm!=0,] ## only plot vectors that have a norm (norm != 0)
    # still allows for the tail point if the user wants a tail point

    # Create the vector grob
    vector_grob <- grid::segmentsGrob(
      x0 = unit(vector_coords$x, "npc"), y0 = unit(vector_coords$y, "npc"),
      x1 = unit(vector_coords$xend, "npc"), y1 = unit(vector_coords$yend, "npc"),
      gp = grid::gpar(col = vector_coords$colour, fill = vector_coords$colour, lwd = linewidth, alpha = vector_coords$alpha),
      arrow = arrow
    )

    points_grob <- NULL
    if (tail_point) {
      points_grob <- grid::pointsGrob(
        x = unit(coords$x, "npc"),  # The starting point (adjusted by centering)
        y = unit(coords$y, "npc"),
        pch = 16,  # Solid circle
        size = unit(tail_point.size, "mm"),
        gp = grid::gpar(col = coords$colour, fill = coords$fill, alpha = coords$alpha)
      )
    }

    # Combine vector and points grobs
    grobs <- list(vector_grob, points_grob)
    grobs <- Filter(Negate(is.null), grobs)  # Remove NULL entries
    return(grid::grobTree(do.call(grid::gList, grobs)))

  } else { ## if length is mapped
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

    ## properly scale min vector length
    length_range <- range(norms)

    data$length[data$length == 0] <- length_range[1]/length_range[2]/5

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

    # non_zero_coords <- coords[coords$norm > .0001,] ## tried to do this to remove 0 length norms but it messes up the legend

      vector_grob <- grid::segmentsGrob(
        x0 = unit(coords$x, "npc") - unit(half_dx, "cm"),
        y0 = unit(coords$y, "npc") - unit(half_dy, "cm"),
        x1 = unit(coords$x, "npc") + unit(half_dx, "cm"),
        y1 = unit(coords$y, "npc") + unit(half_dy, "cm"),
        gp = grid::gpar(col = coords$colour, fill = coords$colour, lwd = linewidth),
        arrow = arrow
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

      ## this determines if the user has adjusted the length of the arrow away from the default and makes it smaller
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
draw_key_vector <- function(data, params, size) {

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
  # required_aes = NULL,
  # required_aes = c("x", "y"),
  default_aes = aes(
    color = "black",
    fill = "black",
    size = 0.5,
    length = 1,
    linewidth = 2,
    linetype = 1,
    alpha = 1
  ),

  setup_data = function(data, params) {
    if (!"length" %in% colnames(data) || all(is.na(data$length))) {
      # if (is.na(data$length[1])) {

      # Normalize dx and dy to unit vectors if normalize is TRUE

      if (params$normalize) {
        # Detect if the data forms a regular grid by checking unique x and y spacings
        x_spacing <- unique(diff(sort(unique(data$x))))
        y_spacing <- unique(diff(sort(unique(data$y))))


        # Calculate the minimum spacing or default to 1 if not a grid
        min_spacing <-
          if (length(x_spacing) == 0 || length(y_spacing) == 0) {
            1
          } else if (all(abs(x_spacing - mean(x_spacing)) < 1e-6) &&
                     all(abs(y_spacing - mean(y_spacing)) < 1e-6)) {
            min(x_spacing, y_spacing) * .9
          } else {
            1  # No scaling for non-grid data
          }

        # Normalize the vectors to unit length and scale by the minimum spacing
        norms <- sqrt(data$dx ^ 2 + data$dy ^ 2)
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
      norms <- sqrt(data$dx ^ 2 + data$dy ^ 2)
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

  draw_group = draw_panel_vector,
  draw_key = draw_key_vector
)


#' Create a Continuous Scale for Vector Length
#'
#' `scale_length_continuous` provides a continuous scale for controlling the length
#' aesthetic in a ggplot. This is particularly useful when working with vector plots
#' where vector lengths are mapped to a continuous scale.
#'
#' @param max_range The maximum value to which the input is rescaled. Numeric scalar
#'   specifying the upper bound of the output range. Should be between 0 and 1.
#' @param ... Other arguments passed to `continuous_scale()`.
#' @export
scale_length_continuous <- function(max_range = 0.5, ...) {
  continuous_scale(
    aesthetics = "length",
    palette = scales::rescale_pal(range = c(0, max_range)),
    ...
  )
}

