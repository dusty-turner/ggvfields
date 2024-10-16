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
#' @param scale_length Numeric; scales the length of the vectors to a given
#'   value. Useful for ensuring consistent lengths for visualization.
#' @param center Logical; if `TRUE`, centers the vector on the evaluated x/y
#'   location. If `FALSE`, the vector origin is at the evaluated x/y location.
#'   When centering is enabled, the vector's midpoint aligns with the original
#'   x/y location.
#' @param normalize Logical; if `TRUE`, normalizes the vector's length to a unit
#'   length before applying other transformations like centering. If `FALSE`,
#'   vectors retain their original lengths.
#' @param method Character; specifies the smoothing method to be used. Accepts
#'   `"lm"` for linear modeling or `"loess"` for locally estimated scatterplot
#'   smoothing.
#' @param se Logical; if `TRUE`, plots the confidence intervals around the
#'   smoothed vectors.
#' @param se.circle Logical; if `TRUE`, draws circles around the origin of the
#'   vectors to represent the radius of the confidence interval. This is useful
#'   for visualizing the spread and variability of the confidence intervals when
#'   `se = TRUE`.
#' @param probs Numeric vector; specifies the prediction interval level(s) to be
#'   plotted when `se = TRUE`. For example, `probs = c(0.95, 0.68)` will plot
#'   95% and 68% prediction intervals. If only one value is provided (e.g.,
#'   `probs = 0.95`), a single prediction interval is drawn. If multiple values
#'   are provided, the function will plot multiple intervals (with smaller
#'   intervals nested inside larger ones).
#' @param arrow Arrow specification, as created by `grid::arrow()`. This
#'   controls the appearance of the arrowheads at the end of the vectors,
#'   including properties like angle, length, and type.
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a
#'   smooth vector field plot.
#' @importFrom stats qt
#' @name geom_vector_smooth
#' @rdname geom_vector_smooth
#'
#' @section Aesthetics: `geom_vector_smooth` understands the following
#'   aesthetics (required aesthetics are in bold):
#'
#' - **`x`**: x-coordinate of the start point of the vector.
#' - **`y`**: y-coordinate of the start point of the vector.
#' - `xend`: x-coordinate of the end point of the vector (optional if `angle` and `distance` are provided).
#' - `yend`: y-coordinate of the end point of the vector (optional if `angle` and `distance` are provided).
#' - `angle`: The angle of the vector in degrees (optional, used with `distance`).
#' - `distance`: The distance/magnitude of the vector (optional, used with `angle`).
#' - `color`: The color of the vector line.
#' - `linewidth`: The thickness of the vector line.
#' - `linetype`: The type of the vector line (solid, dashed, etc.).
#' - `alpha`: The transparency level of the vector.
#' - `arrow`: Specification for arrowheads at the end of the vector.
#'
#'   Additionally, when using smoothing:
#' - `norm`: A computed variable representing the magnitude of each smoothed vector.
#'
#' @examples
#' # Generate a function to create random vectors based on input (x, y) values
#' generate_vectors <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   # Return new x and y values with added random noise
#'   c(x + rnorm(1, 1, 1), y + rnorm(1, 1, 1))
#' }
#'
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Generate sample points for the vector field
#' sample_points <- data.frame(
#'   x = runif(10, min = -10, max = 10),
#'   y = runif(10, min = -10, max = 10)
#' )
#'
#' # Apply the generate_vectors function to each row
#' result <- t(apply(sample_points, 1, generate_vectors))
#'
#' # Create new columns for xend and yend coordinates
#' sample_points$xend <- result[, 1]
#' sample_points$yend <- result[, 2]
#'
#' # Calculate dx and dy (displacements)
#' sample_points$dx <- sample_points$xend - sample_points$x
#' sample_points$dy <- sample_points$yend - sample_points$y
#'
#' # Plot the original vectors and smoothed vectors using `ggplot2` from ggvfields
#' ggplot(sample_points, aes(x = x, y = y)) +
#'   geom_vector(aes(dx = dx, dy = dy), color = "red") +  # Original vectors in red
#'   geom_vector_smooth(aes(dx = dx, dy = dy), se = TRUE, se.circle = TRUE)  # Smoothed vectors
#'
NULL


calculate_bounds <- function(fit, se, probs) {
  if (!se) return(NULL)

  # Calculate critical t-value for the confidence level
  t_critical <- qt(1 - (1 - probs) / 2, df = fit$df)

  # Calculate upper and lower bounds
  lower_bound <- fit$fit - t_critical * fit$se.fit
  upper_bound <- fit$fit + t_critical * fit$se.fit

  # Return a list of bounds
  return(list(lower = lower_bound, upper = upper_bound))
}


#' @rdname geom_vector_smooth
#' @export
StatVectorSmooth <- ggproto(
  "StatVectorSmooth",
  Stat,
  required_aes = c("x", "y", "dx", "dy"),
  default_aes = aes(color = "#3366FF", linewidth = 0.5, linetype = 1, alpha = 1),

  compute_group = function(data, scales, n, center, method, normalize = TRUE, scale_length, se = TRUE, probs, ...) {

    # Ensure probs is a vector, even if a single value is provided
    if (length(probs) == 1) {
      probs <- c(probs, NA)  # Add NA to represent the missing inner interval
    }

    # Ensure that n has the correct length
    n <- ensure_length_two(n)

    # Calculate xend and yend using dx and dy
    data$xend <- data$x + data$dx
    data$yend <- data$y + data$dy

    # Calculate angle and distance using dx and dy
    data$distance <- sqrt(data$dx^2 + data$dy^2)
    data$angle <- atan2(data$dy, data$dx)

    # Generate the grid for predictions
    x_seq <- seq(min(data$x), max(data$x), length.out = n[1])
    y_seq <- seq(min(data$y), max(data$y), length.out = n[2])
    grid <- expand.grid(x = x_seq, y = y_seq)
    grid$id <- 1:nrow(grid)


    x_spacing <- diff(sort(unique(grid$x)))[1]  # Spacing between adjacent x-values
    y_spacing <- diff(sort(unique(grid$y)))[1]  # Spacing between adjacent y-values

    # Choose the radius as a fraction of the smaller spacing
    radius <- min(x_spacing, y_spacing) / 2.5


    if (method == "lm") {
      fit_sin <- lm(sin(angle) ~ x + y, data = data)
      fit_cos <- lm(cos(angle) ~ x + y, data = data)
      fit_distance <- lm(distance ~ x + y, data = data)

      # Predictions and confidence intervals using helper function
      pred_sin <- predict(fit_sin, newdata = grid, se.fit = se)
      pred_cos <- predict(fit_cos, newdata = grid, se.fit = se)
      pred_distance <- predict(fit_distance, newdata = grid, se.fit = se)

      if (se) {

        # Calculate confidence bounds for outer interval
        sin_bounds_outer <- calculate_bounds(fit = pred_sin, se = se, probs = probs[1])
        cos_bounds_outer <- calculate_bounds(pred_cos, se, probs[1])
        distance_bounds <- calculate_bounds(pred_distance, se, probs[1])

        # Reconstruct the angle and distance predictions
        grid$angle_pred <- atan2(pred_sin$fit, pred_cos$fit)
        grid$distance_pred <- pred_distance$fit

        # Calculate the angle prediction intervals for outer bounds
        grid$angle_lower_outer <- atan2(sin_bounds_outer$lower, cos_bounds_outer$upper)
        grid$angle_upper_outer <- atan2(sin_bounds_outer$upper, cos_bounds_outer$lower)

        # Calculate xend and yend for lower, upper, and main predictions
        grid$xend_pred <- grid$x + grid$distance_pred * cos(grid$angle_pred)
        grid$yend_pred <- grid$y + grid$distance_pred * sin(grid$angle_pred)
        grid$xend_lower_outer <- grid$x + grid$distance_pred * cos(grid$angle_lower_outer)
        grid$yend_lower_outer <- grid$y + grid$distance_pred * sin(grid$angle_lower_outer)
        grid$xend_upper_outer <- grid$x + grid$distance_pred * cos(grid$angle_upper_outer)
        grid$yend_upper_outer <- grid$y + grid$distance_pred * sin(grid$angle_upper_outer)

        # Handle inner interval only if probs[2] is not NA
        if (!is.na(probs[2])) {
          sin_bounds_inner <- calculate_bounds(fit = pred_sin, se = se, probs = probs[2])
          cos_bounds_inner <- calculate_bounds(pred_cos, se, probs[2])
          distance_bounds <- calculate_bounds(pred_distance, se, probs[2])

          # Calculate the angle prediction intervals for inner bounds
          grid$angle_lower_inner <- atan2(sin_bounds_inner$lower, cos_bounds_inner$upper)
          grid$angle_upper_inner <- atan2(sin_bounds_inner$upper, cos_bounds_inner$lower)

          # Calculate xend and yend for lower, upper, and main predictions
          grid$xend_lower_inner <- grid$x + grid$distance_pred * cos(grid$angle_lower_inner)
          grid$yend_lower_inner <- grid$y + grid$distance_pred * sin(grid$angle_lower_inner)
          grid$xend_upper_inner <- grid$x + grid$distance_pred * cos(grid$angle_upper_inner)
          grid$yend_upper_inner <- grid$y + grid$distance_pred * sin(grid$angle_upper_inner)
        }
      } else {
        grid$angle_pred <- atan2(pred_sin, pred_cos)
        grid$distance_pred <- pred_distance
        grid$xend_pred <- grid$x + grid$distance_pred * cos(grid$angle_pred)
        grid$yend_pred <- grid$y + grid$distance_pred * sin(grid$angle_pred)
      }
    }

    if (normalize) {
      grid$norm_pred <- sqrt((grid$xend_pred - grid$x)^2 + (grid$yend_pred - grid$y)^2)
      grid$xend_pred <- grid$x + (grid$xend_pred - grid$x) / grid$norm_pred * scale_length
      grid$yend_pred <- grid$y + (grid$yend_pred - grid$y) / grid$norm_pred * scale_length
    }

    if (center) {
      half_u <- (grid$xend_pred - grid$x) / 2
      half_v <- (grid$yend_pred - grid$y) / 2

      grid$x <- grid$x - half_u
      grid$y <- grid$y - half_v
      grid$xend_pred <- grid$xend_pred - half_u
      grid$yend_pred <- grid$yend_pred - half_v
    }

    # Include radius and original dx, dy in the result
    result <- data.frame(
      x = grid$x,
      y = grid$y,
      xend = grid$xend_pred,
      yend = grid$yend_pred,
      radius = rep(radius, nrow(grid)),
      dx = grid$xend_pred - grid$x,  # Include dx
      dy = grid$yend_pred - grid$y  # Include dy
      # distance = grid$distance_pred / max(grid$distance_pred)
    )

    # Append inner bounds only if they exist
    if (se) {
      result$xend_upper_outer <- grid$xend_upper_outer
      result$yend_upper_outer <- grid$yend_upper_outer
      result$xend_lower_outer <- grid$xend_lower_outer
      result$yend_lower_outer <- grid$yend_lower_outer

      if (!is.na(probs[2])) {
        result$xend_upper_inner <- grid$xend_upper_inner
        result$yend_upper_inner <- grid$yend_upper_inner
        result$xend_lower_inner <- grid$xend_lower_inner
        result$yend_lower_inner <- grid$yend_lower_inner
      }
    }


    return(result)
  }
)



#' @rdname geom_vector_smooth
#' @export
GeomVectorSmooth <- ggproto(
  "GeomVectorSmooth",
  GeomSegment,
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = aes(linewidth = 0.5, linetype = 1, alpha = 1, fill = "grey80", color = "#3366FF"),

  setup_data = function(data, params) {
    data$id <- seq_len(nrow(data))

    if (params$se) {
      original_length <- sqrt((data$xend - data$x)^2 + (data$yend - data$y)^2)
      scaling_factor <- data$radius / original_length

      data$xend <- data$x + (data$xend - data$x) * scaling_factor
      data$yend <- data$y + (data$yend - data$y) * scaling_factor
    }

    return(data)
  },

  draw_panel = function(data, panel_params, coord, arrow = NULL, se = TRUE, se.circle = TRUE) {
    circle_grob <- NULL
    wedge_grob_outer <- NULL
    wedge_grob_inner <- NULL
    all_circle_data <- NULL
    wedge_data_outer <- NULL
    wedge_data_inner <- NULL

    if (se) {
      if (se.circle) {
        # Create confidence interval circles if se.circle is TRUE
        all_circle_data <- do.call(rbind, lapply(1:nrow(data), function(i) {
          circle_data <- create_circle_data(
            x = data$x[i], y = data$y[i],
            # radius = data$distance[i]
            radius = data$radius[i]
          )
          circle_data$group <- i
          circle_data$linewidth <- data$linewidth[i]
          circle_data$alpha <- 0.4
          circle_data$fill <- NA
          circle_data$colour <- "grey60"
          return(circle_data)
        }))

        circle_grob <- GeomPolygon$draw_panel(
          data = all_circle_data,
          panel_params = panel_params,
          coord = coord
        )
      }

      # Outer wedge data
      wedge_data_outer <- do.call(rbind, lapply(1:nrow(data), function(i) {
        wedge <- create_wedge_data(
          x = data$x[i], y = data$y[i],
          xend_upper = data$xend_upper_outer[i], yend_upper = data$yend_upper_outer[i],
          xend_lower = data$xend_lower_outer[i], yend_lower = data$yend_lower_outer[i],
          xend = data$xend[i], yend = data$yend[i],
          # radius = data$distance[i],
          radius = data$radius[i],
          id = data$id[i],
          n_points = 50
        )

        wedge$linewidth <- data$linewidth[i]
        wedge$alpha <- 0.4
        wedge$fill <- data$fill[i]
        wedge$colour <- "grey60"
        return(wedge)
      }))

      wedge_grob_outer <- GeomPolygon$draw_panel(
        data = wedge_data_outer,
        panel_params = panel_params,
        coord = coord
      )

      # Draw inner wedge only if inner bounds exist (probs[2] was not NA)
      if (!is.null(data$xend_lower_inner)) {
        wedge_data_inner <- do.call(rbind, lapply(1:nrow(data), function(i) {
          wedge <- create_wedge_data(
            x = data$x[i], y = data$y[i],
            xend_upper = data$xend_upper_inner[i], yend_upper = data$yend_upper_inner[i],
            xend_lower = data$xend_lower_inner[i], yend_lower = data$yend_lower_inner[i],
            xend = data$xend[i], yend = data$yend[i],
            # radius = data$distance[i],
            radius = data$radius[i],
            id = data$id[i],
            n_points = 50
          )

          wedge$linewidth <- data$linewidth[i]
          wedge$alpha <- 0.4
          wedge$fill <- data$fill[i]
          wedge$colour <- "grey60"
          return(wedge)
        }))

        wedge_grob_inner <- GeomPolygon$draw_panel(
          data = wedge_data_inner,
          panel_params = panel_params,
          coord = coord
        )
      }
    }

    # Draw the vectors
    segments_grob <- GeomSegment$draw_panel(
      data = data,
      panel_params = panel_params,
      coord = coord,
      arrow = arrow
    )

    # Combine grobs for outer wedge, inner wedge (if any), and segments
    grobs <- list(circle_grob, wedge_grob_outer, wedge_grob_inner, segments_grob)
    grobs <- Filter(Negate(is.null), grobs)

    return(grid::grobTree(do.call(grid::gList, grobs)))
  },


  draw_key = draw_key_smooth
)



#' @rdname geom_vector_smooth
#' @export
stat_vector_smooth <- function(
  mapping = NULL,
  data = NULL,
  geom = "vector_smooth",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  n = c(11, 11),
  scale_length = 1,
  center = TRUE,
  normalize = TRUE,
  method = "lm",
  se = TRUE,
  se.circle = TRUE,
  probs = c(.95, NA),
  arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
  ...
) {

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
      normalize = normalize,
      scale_length = scale_length,
      method = method,
      se = se,
      se.circle = se.circle,
      probs = probs,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_vector_smooth
#' @export
geom_vector_smooth <- function(
  mapping = NULL,
  data = NULL,
  stat = "vector_smooth",
  position = "identity",
  na.rm = FALSE, show.legend = NA,
  inherit.aes = TRUE,
  n = c(11, 11), scale_length = 1,
  center = TRUE, normalize = TRUE,
  method = "lm",
  se = TRUE,
  se.circle = TRUE,
  probs = c(.95, NA),
  arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
  ...
) {

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
      normalize = normalize,
      scale_length = scale_length,
      method = method,
      se = se,
      se.circle = se.circle,
      probs = probs,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}
