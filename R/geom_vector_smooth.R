#' Create a Smooth Vector Plot Layer
#'
#' `geom_vector_smooth` generates a ggplot layer that visualizes a smooth vector
#' field by taking in raw vector data and applying smoothing techniques to
#' estimate the underlying vector field. This is similar in concept to how
#' `geom_smooth()` in ggplot2 estimates a smooth line, but for vector data
#' instead of scalar data.
#'
#' @inheritParams geom_vector
#' @inheritParams ggplot2::stat_identity
#' @param n An integer vector specifying the number of grid points along each
#'   axis.
#' @param center Logical; if `TRUE`, centers the vector on the evaluated x/y
#'   location. If `FALSE`, the vector origin is at the evaluated x/y location.
#'   When centering is enabled, the vector's midpoint aligns with the original
#'   x/y location. This parameter is passed to the `GeomVector` object and is
#'   applied during the rendering of the vectors.
#' @param arrow Arrow specification, as created by `grid::arrow()`. This
#'   controls the appearance of the arrowheads at the end of the vectors,
#'   including properties like angle, length, and type.
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a
#'   smooth vector field plot.
#' @name geom_vector_smooth
#' @rdname geom_vector_smooth
#' @examples
#'
#' # Example data
#' set.seed(1234)
#' n <- 10
#' wind_data_polar <- data.frame(
#'   lon = rnorm(n),
#'   lat = rnorm(n),
#'   wind_dir = runif(n, -pi, pi),
#'   wind_spd = rchisq(n, df = 2)
#' )
#'
#' # Cartesian transformation
#' wind_data_cartesian <- within(wind_data_polar, {
#'   wind_lon_comp <- wind_spd * cos(wind_dir)
#'   wind_lat_comp <- wind_spd * sin(wind_dir)
#'   xend <- lon + wind_lon_comp
#'   yend <- lat + wind_lat_comp
#' })
#'
#' # Example using Cartesian coordinates
#' ggplot(wind_data_cartesian) +
#' geom_vector(aes(x = lon, y = lat, xend = xend, yend = yend),
#'                 arrow_size = .01, color = "black", center = FALSE) +
#' geom_vector_smooth(aes(x = lon, y = lat, xend = xend, yend = yend),
#'                        arrow_size = .01, alpha = .7, color = "red") +
#' geom_vector_smooth(aes(x = lon, y = lat, xend = xend, yend = yend),
#'                        arrow_size = .01, alpha = .7,
#'                        color = "darkgreen", center = FALSE)
#'
#' # Example using Polar coordinates
#' ggplot(wind_data_polar) +
#' geom_vector(aes(x = lon, y = lat, angle = wind_dir * 180 / pi, distance = wind_spd),
#'                 color = "black", arrow_size = .03) +
#' geom_vector_smooth(aes(x = lon, y = lat, angle = wind_dir * 180 / pi, distance = wind_spd),
#'                        arrow_size = .01, alpha = .7, color = "red") +
#' geom_vector_smooth(aes(x = lon, y = lat, angle = wind_dir * 180 / pi, distance = wind_spd),
#'                        arrow_size = .01, alpha = .7, color = "darkgreen", center = FALSE)
#'
#'
NULL

#' @rdname geom_vector_smooth
#' @export
StatVectorSmooth <- ggproto("StatVectorSmooth", Stat,

  required_aes = c("x", "y"),

  compute_group = function(data, scales, n, center, method, ...) {

    # Ensure that n has the correct length
    n <- ensure_length_two(n)

    # Convert angle and distance to xend and yend if necessary
    original_input <- "angle" %in% names(data) && "distance" %in% names(data)
    if (original_input) {
        data$angle <- data$angle * pi / 180  # Convert angle to radians
        data$xend <- data$x + data$distance * cos(data$angle)
        data$yend <- data$y + data$distance * sin(data$angle)
    }

    # Now, check that xend and yend exist
    if (!("xend" %in% names(data) && "yend" %in% names(data))) {
        stop("Either xend/yend or angle/distance must be provided.")
    }

    # Calculate the norm of the vectors
    data$norm <- sqrt((data$xend - data$x)^2 + (data$yend - data$y)^2)

    # Normalize the end points
    data$xend_norm <- data$x + (data$xend - data$x) / data$norm
    data$yend_norm <- data$y + (data$yend - data$y) / data$norm

    # Generate the grid for predictions
    x_seq <- seq(min(data$x), max(data$x), length.out = n[1])
    y_seq <- seq(min(data$y), max(data$y), length.out = n[2])
    grid <- expand.grid(x = x_seq, y = y_seq)

    # Fit the models for smoothing
    if (method == "lm") {
        fit_x <- lm(xend_norm ~ x + y, data = data)
        fit_y <- lm(yend_norm ~ x + y, data = data)
        message("`geom_vector_smooth()` using method = 'lm' and formula 'xend ~ x + y' and 'yend ~ x + y'")
        # Generate predictions
        grid$xend_pred <- predict(fit_x, newdata = grid)
        grid$yend_pred <- predict(fit_y, newdata = grid)
    } else if (method == "loess") {
        fit_x <- loess(xend_norm ~ x, data = data)
        fit_y <- loess(yend_norm ~ y, data = data)
        message("`geom_vector_smooth()` using method = 'loess' and formula 'xend ~ x' and 'yend ~ y'")
        # Generate predictions
        grid$xend_pred <- predict(fit_x, newdata = grid[,1])
        grid$yend_pred <- predict(fit_y, newdata = grid[,2])
    } else {
        stop("Unsupported method. Please use 'lm' or 'loess'.")
    }

    # Calculate the norm of the predicted values
    grid$norm_pred <- sqrt((grid$xend_pred - grid$x)^2 + (grid$yend_pred - grid$y)^2)

    # Normalize the predicted end points
    grid$xend_pred_norm <- grid$x + (grid$xend_pred - grid$x) / grid$norm_pred
    grid$yend_pred_norm <- grid$y + (grid$yend_pred - grid$y) / grid$norm_pred

    # If the original input was angle and distance, convert back
    if (original_input) {
        grid$distance <- sqrt((grid$xend_pred_norm - grid$x)^2 + (grid$yend_pred_norm - grid$y)^2)
        grid$angle <- atan2(grid$yend_pred_norm - grid$y, grid$xend_pred_norm - grid$x) * 180 / pi  # Convert back to degrees

        result <- data.frame(
          x = grid$x,
          y = grid$y,
          distance = grid$distance,
          angle = grid$angle
        )
    } else {
        # Return the predicted data in x/y/xend/yend format
        result <- data.frame(
          x = grid$x,
          y = grid$y,
          xend = grid$xend_pred_norm,
          yend = grid$yend_pred_norm
        )
    }


    result
  }
)


#' @rdname geom_vector_smooth
#' @export
geom_vector_smooth <- function(mapping = NULL, data = NULL,
                               stat = "vector_smooth",
                               position = "identity",
                               na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE,
                               n = c(11, 11),
                               center = TRUE,
                               method = "lm",
                               arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                               ...) {

  layer(
    stat = StatVectorSmooth,
    data = data,
    mapping = mapping,
    geom = GeomVectorSmooth,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      center = center,
      method = method,  # Pass the method argument to the params list
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_vector_smooth
#' @export
GeomVectorSmooth <- ggproto("GeomVectorSmooth", GeomVector,
                            required_aes = c("x", "y"),

                            optional_aes = c("xend", "yend", "angle", "distance"),

                            default_aes = aes(color = "black", linewidth = 0.5, linetype = 1, alpha = 1),

                            draw_panel = function(data, panel_params, coord, arrow = NULL, arrow_size = 1, center = TRUE) {

                              # Handle both xend/yend and angle/distance
                              if ("angle" %in% names(data) && "distance" %in% names(data)) {
                                data$angle <- data$angle * pi / 180  # Convert angle to radians
                                data$xend <- data$x + data$distance * cos(data$angle)
                                data$yend <- data$y + data$distance * sin(data$angle)
                              }

                              # Handle centering if specified
                              if (center) {
                                half_u <- (data$xend - data$x) / 2
                                half_v <- (data$yend - data$y) / 2

                                data$x <- data$x - half_u
                                data$y <- data$y - half_v
                                data$xend <- data$xend - half_u
                                data$yend <- data$yend - half_v
                              }

                              # Calculate the vector length
                              data$vector_length <- sqrt((data$xend - data$x)^2 + (data$yend - data$y)^2)

                              # Scale arrow size based on vector length, with a sensible scaling factor
                              max_arrow_size <- 0.05  # Maximum size cap for arrows
                              scaling_factor <- 0.02  # Base scaling factor
                              data$arrow_size <- pmin(scaling_factor * data$vector_length, max_arrow_size)

                              # Apply the calculated arrow size
                              arrow <- modifyList(arrow, list(length = unit(data$arrow_size, "npc")))

                              # Call the draw_panel from GeomSegment or GeomVector
                              GeomSegment$draw_panel(data, panel_params, coord, arrow = arrow)
                            }
)




#' @rdname geom_vector_smooth
#' @export
stat_vector_smooth <- function(mapping = NULL, data = NULL,
                               geom = "vector",
                               position = "identity",
                               na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE,
                               n = c(11, 11),
                               center = TRUE,
                               method = "lm",
                               arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                               ...) {

  layer(
    stat = StatVectorSmooth,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      n = n,
      center = center,
      method = method,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}


