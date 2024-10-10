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
#' @param n An integer vector specifying the number of grid points along each axis.
#' @param scale_length Numeric; scales the length of the vectors to a given value.
#'   Useful for ensuring consistent lengths for visualization.
#' @param center Logical; if `TRUE`, centers the vector on the evaluated x/y
#'   location. If `FALSE`, the vector origin is at the evaluated x/y location.
#'   When centering is enabled, the vector's midpoint aligns with the original
#'   x/y location.
#' @param normalize Logical; if `TRUE`, normalizes the vector's length to a unit
#'   length before applying other transformations like centering. If `FALSE`,
#'   vectors retain their original lengths.
#' @param method Character; specifies the smoothing method to be used.
#'   Accepts `"lm"` for linear modeling or `"loess"` for locally estimated
#'   scatterplot smoothing.
#' @param se Logical; if `TRUE`, plots the confidence intervals around the smoothed vectors.
#' @param se.circle Logical; if `TRUE`, draws circles around the origin of the vectors to represent
#'   the radius of the confidence interval. This is useful for visualizing the spread and variability
#'   of the confidence intervals when `se = TRUE`.
#' @param arrow Arrow specification, as created by `grid::arrow()`. This
#'   controls the appearance of the arrowheads at the end of the vectors,
#'   including properties like angle, length, and type.
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a
#'   smooth vector field plot.
#' @name geom_vector_smooth
#' @rdname geom_vector_smooth
#'
#' @section Aesthetics:
#' `geom_vector_smooth` understands the following aesthetics (required aesthetics are in bold):
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
#' Additionally, when using smoothing:
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


#' @rdname geom_vector_smooth
#' @export
StatVectorSmooth <- ggproto("StatVectorSmooth", Stat,
                            required_aes = c("x", "y", "dx", "dy"),
                            default_aes = aes(color = "#3366FF", linewidth = 0.5, linetype = 1, alpha = 1),

                            compute_group = function(data, scales, n, center, method, normalize = TRUE, scale_length, se = TRUE, level = .95, ...) {

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

                              # Fit models using regression on sin and cos for angle, and linear regression for distance
                              if (method == "lm") {
                                fit_sin <- lm(sin(angle) ~ x + y, data = data)
                                fit_cos <- lm(cos(angle) ~ x + y, data = data)
                                fit_distance <- lm(distance ~ x + y, data = data)

                                # Predict the sine and cosine values
                                pred_sin <- predict(fit_sin, newdata = grid, se.fit = se)
                                pred_cos <- predict(fit_cos, newdata = grid, se.fit = se)
                                pred_distance <- predict(fit_distance, newdata = grid, se.fit = se)

                                # Calculate residuals and covariance
                                sin_residuals <- residuals(fit_sin)
                                cos_residuals <- residuals(fit_cos)
                                cov_sin_cos <- cov(sin_residuals, cos_residuals)

                                # Calculate symmetric angle prediction intervals using Delta Method
                                if (se) {

                                  # Reconstruct the angle from sine and cosine predictions
                                  grid$angle_pred <- atan2(pred_sin$fit, pred_cos$fit)
                                  grid$distance_pred <- pred_distance$fit

                                  # Calculate the critical value for confidence intervals
                                  t_critical <- qt(1 - (1 - level) / 2, df = fit_distance$df.residual)

                                  # Calculate the variance and standard error of the angle using Delta Method
                                  sin_var <- pred_sin$se.fit^2
                                  cos_var <- pred_cos$se.fit^2

                                  # Compute gradient components
                                  partial_sin <- pred_cos$fit / (pred_sin$fit^2 + pred_cos$fit^2)
                                  partial_cos <- -pred_sin$fit / (pred_sin$fit^2 + pred_cos$fit^2)

                                  # Calculate angle variance using Delta Method with covariance
                                  grid$angle_var <- sin_var * partial_sin^2 + cos_var * partial_cos^2 + 2 * abs(cov_sin_cos * partial_sin * partial_cos)
                                  grid$angle_sd <- sqrt(grid$angle_var)

                                  # Construct symmetric prediction intervals
                                  grid$angle_lower <- grid$angle_pred - t_critical * grid$angle_sd
                                  grid$angle_upper <- grid$angle_pred + t_critical * grid$angle_sd

                                  # Calculate xend and yend for lower, upper, and main predictions
                                  grid$xend_pred <- grid$x + grid$distance_pred * cos(grid$angle_pred)
                                  grid$yend_pred <- grid$y + grid$distance_pred * sin(grid$angle_pred)
                                  grid$xend_lower <- grid$x + grid$distance_pred * cos(grid$angle_lower)
                                  grid$yend_lower <- grid$y + grid$distance_pred * sin(grid$angle_lower)
                                  grid$xend_upper <- grid$x + grid$distance_pred * cos(grid$angle_upper)
                                  grid$yend_upper <- grid$y + grid$distance_pred * sin(grid$angle_upper)
                                } else {
                                  grid$angle_pred <- atan2(pred_sin, pred_cos)
                                  grid$distance_pred <- pred_distance

                                  grid$xend_pred <- grid$x + grid$distance_pred * cos(grid$angle_pred)
                                  grid$yend_pred <- grid$y + grid$distance_pred * sin(grid$angle_pred)
                                }
                              } else if (method == "loess") {
                                fit_angle <- loess(angle ~ x + y, data = data)
                                fit_distance <- loess(distance ~ x + y, data = data)
                                grid$angle_pred <- predict(fit_angle, newdata = grid)
                                grid$distance_pred <- predict(fit_distance, newdata = grid)
                                grid$xend_pred <- grid$x + grid$distance_pred * cos(grid$angle_pred)
                                grid$yend_pred <- grid$y + grid$distance_pred * sin(grid$angle_pred)

                              } else {
                                stop("Unsupported method. Please use 'lm' or 'loess'.")
                              }

                              if (normalize) {
                                grid$norm_pred <- sqrt((grid$xend_pred - grid$x)^2 + (grid$yend_pred - grid$y)^2)
                                grid$xend_pred <- grid$x + (grid$xend_pred - grid$x) / grid$norm_pred * scale_length
                                grid$yend_pred <- grid$y + (grid$yend_pred - grid$y) / grid$norm_pred * scale_length

                                if (se) {
                                  grid$norm_lower <- sqrt((grid$xend_lower - grid$x)^2 + (grid$yend_lower - grid$y)^2)
                                  grid$norm_upper <- sqrt((grid$xend_upper - grid$x)^2 + (grid$yend_upper - grid$y)^2)

                                  grid$xend_lower <- grid$x + (grid$xend_lower - grid$x) / grid$norm_lower * scale_length
                                  grid$yend_lower <- grid$y + (grid$yend_lower - grid$y) / grid$norm_lower * scale_length
                                  grid$xend_upper <- grid$x + (grid$xend_upper - grid$x) / grid$norm_upper * scale_length
                                  grid$yend_upper <- grid$y + (grid$yend_upper - grid$y) / grid$norm_upper * scale_length
                                }
                              }

                              if (center) {
                                half_u <- (grid$xend_pred - grid$x) / 2
                                half_v <- (grid$yend_pred - grid$y) / 2

                                grid$x <- grid$x - half_u
                                grid$y <- grid$y - half_v
                                grid$xend_pred <- grid$xend_pred - half_u
                                grid$yend_pred <- grid$yend_pred - half_v

                                if (se) {
                                  grid$xend_lower <- grid$xend_lower - half_u
                                  grid$xend_upper <- grid$xend_upper - half_u
                                  grid$yend_lower <- grid$yend_lower - half_v
                                  grid$yend_upper <- grid$yend_upper - half_v
                                }
                              }

                              if (se) {
                                result <- data.frame(
                                  x = grid$x,
                                  y = grid$y,
                                  xend = grid$xend_pred,
                                  yend = grid$yend_pred,
                                  norm = grid$distance_pred,
                                  xend_upper = grid$xend_upper,
                                  yend_upper = grid$yend_upper,
                                  xend_lower = grid$xend_lower,
                                  yend_lower = grid$yend_lower,
                                  dx = grid$xend_pred - grid$x,
                                  dy = grid$yend_pred - grid$y,
                                  radius = rep(radius,nrow(grid))
                                )
                              } else {
                                result <- data.frame(
                                  x = grid$x,
                                  y = grid$y,
                                  xend = grid$xend_pred,
                                  yend = grid$yend_pred,
                                  norm = grid$distance_pred,
                                  dx = grid$xend_pred - grid$x,
                                  dy = grid$yend_pred - grid$y
                                )
                              }

                              return(result)
                            }
)

#' @rdname geom_vector_smooth
#' @export
GeomVectorSmooth <- ggproto("GeomVectorSmooth", GeomSegment,

                            required_aes = c("x", "y", "xend", "yend"),

                            default_aes = aes(linewidth = 0.5, linetype = 1, alpha = 1, fill = "grey80", color = "#3366FF"),

                            setup_data = function(data, params) {

                              data$id <- seq_len(nrow(data))

                              if(params$se){

                                original_length <- sqrt((data$xend - data$x)^2 + (data$yend - data$y)^2)

                                scaling_factor <- data$radius / original_length

                                data$xend <- data$x + (data$xend - data$x) * scaling_factor
                                data$yend <- data$y + (data$yend - data$y) * scaling_factor
                              }


                              return(data)
                            },

                            draw_panel = function(data, panel_params, coord, arrow = NULL, se = TRUE, se.circle) {


                              circle_grob <- NULL
                              wedge_grob <- NULL
                              all_circle_data <- NULL
                              wedge_data <- NULL

                              if (se) {
                                if(se.circle){

                                  all_circle_data <- do.call(rbind, lapply(1:nrow(data), function(i) {

                                    circle_data <- create_circle_data(
                                      x = data$x[i], y = data$y[i], radius = data$radius[i]
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


                                wedge_data <- do.call(rbind, lapply(1:nrow(data), function(i) {

                                  wedge <- create_wedge_data(
                                    x = data$x[i], y = data$y[i],
                                    xend_upper = data$xend_upper[i], yend_upper = data$yend_upper[i],
                                    xend_lower = data$xend_lower[i], yend_lower = data$yend_lower[i],
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


                                wedge_grob <- GeomPolygon$draw_panel(
                                  data = wedge_data,
                                  panel_params = panel_params,
                                  coord = coord
                                )
                              }


                              segments_grob <- GeomSegment$draw_panel(
                                data = data,
                                panel_params = panel_params,
                                coord = coord,
                                arrow = arrow
                              )

                              grobs <- list(circle_grob, wedge_grob, segments_grob)

                              grobs <- Filter(Negate(is.null), grobs)

                              return(grid::grobTree(do.call(grid::gList, grobs)))
                            },

                            draw_key = draw_key_smooth
)


#' @rdname geom_vector_smooth
#' @export
stat_vector_smooth <- function(mapping = NULL, data = NULL,
                               geom = "vector_smooth",
                               position = "identity",
                               na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE,
                               n = c(11, 11), scale_length = 1,
                               center = TRUE, normalize = TRUE,
                               method = "lm",
                               se = TRUE,
                               se.circle = TRUE,
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
      normalize = normalize,
      scale_length = scale_length,
      method = method,
      se = se,
      se.circle = se.circle,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_vector_smooth
#' @export
geom_vector_smooth <- function(mapping = NULL, data = NULL,
                               stat = "vector_smooth",
                               position = "identity",
                               na.rm = FALSE, show.legend = NA,
                               inherit.aes = TRUE,
                               n = c(11, 11), scale_length = 1,
                               center = TRUE, normalize = TRUE,
                               method = "lm",
                               se = TRUE,
                               se.circle = TRUE,
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
      normalize = normalize,
      scale_length = scale_length,
      method = method,
      se = se,
      se.circle = se.circle,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}
