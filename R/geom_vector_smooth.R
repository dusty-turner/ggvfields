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
#' @param scale_length more to follow
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
#'   geom_vector_smooth(aes(dx = dx, dy = dy))            # Smoothed vectors
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
                              grid <- expand.grid(x = x_seq, y = y_seq) |> mutate(id = row_number())

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

                                # Reconstruct the angle from sine and cosine predictions
                                grid$angle_pred <- atan2(pred_sin$fit, pred_cos$fit)
                                grid$distance_pred <- pred_distance$fit

                                # Calculate symmetric angle prediction intervals using Delta Method
                                if (se) {
                                  # Calculate the critical value for confidence intervals
                                  t_critical <- qt(1 - (1 - level) / 2, df = fit_distance$df.residual)

                                  # Calculate the variance and standard error of the angle using Delta Method
                                  sin_var <- pred_sin$se.fit^2
                                  cos_var <- pred_cos$se.fit^2

                                  # Compute gradient components
                                  partial_sin <- pred_cos$fit / (pred_sin$fit^2 + pred_cos$fit^2)
                                  partial_cos <- -pred_sin$fit / (pred_sin$fit^2 + pred_cos$fit^2)

                                  # Calculate angle variance using Delta Method with covariance
                                  grid$angle_var <- sin_var * partial_sin^2 + cos_var * partial_cos^2 + 2 * cov_sin_cos * partial_sin * partial_cos
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
                                  grid$angle_pred <- atan2(pred_sin$fit, pred_cos$fit)
                                  grid$distance_pred <- pred_distance$fit

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

                              # Normalize vector lengths if needed
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

                              # Center the vectors if required
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

                              # Prepare the final data to be returned based on se
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
                                  dy = grid$yend_pred - grid$y
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


create_circle_data <- function(x, y, radius, n = 100) {
  angle <- seq(0, 2 * pi, length.out = n)  # Generate angles from 0 to 2π
  data.frame(
    x = x + radius * cos(angle),  # X-coordinates for the circle
    y = y + radius * sin(angle),  # Y-coordinates for the circle
    group = 1  # Use a placeholder group ID; will be set uniquely for each circle
  )
}

#' @rdname geom_vector_smooth
#' @export
GeomVectorSmooth <- ggproto("GeomVectorSmooth", GeomSegment,

                            required_aes = c("x", "y", "xend", "yend"),

                            default_aes = aes(linewidth = 0.5, linetype = 1, alpha = 1, fill = "grey80", color = "#3366FF"),

                            draw_panel = function(data, panel_params, coord, arrow = NULL) {

                              # Reshape the data for polygons
                              data$id <- seq_len(nrow(data))

                              if(!is.null(data$xend_upper)){

                                radius <- sqrt((data$x[1] - data$xend[1])^2 + (data$y[1] - data$yend[1])^2)

                                all_circle_data <- do.call(rbind, lapply(1:nrow(data), function(i) {

                                  # Create circle data for the current point
                                  circle_data <- create_circle_data(
                                    x = data$x[i], y = data$y[i], radius = radius
                                  )

                                  # Set unique group for each circle
                                  circle_data$group <- i

                                  # Ensure aesthetics are replicated to match the length of circle_data
                                  # circle_data$colour <- rep(data$colour[i], nrow(circle_data))
                                  circle_data$linewidth <- rep(data$linewidth[i], nrow(circle_data))
                                  circle_data$alpha <- rep(0.4, nrow(circle_data))  # Set a fixed alpha value
                                  circle_data$fill <- NA  # Set fill color for the circle
                                  circle_data$colour <- rep("grey60", nrow(circle_data))  # Set fill color for the circle

                                  return(circle_data)
                                }))

                                # Draw all circles using GeomPolygon's draw_panel method to apply fill
                                circle_grob <- GeomPolygon$draw_panel(
                                  data = all_circle_data,
                                  panel_params = panel_params,
                                  coord = coord
                                )

                                polygon_data <- do.call(rbind, lapply(1:nrow(data), function(i) {

                                  data.frame(
                                    id = rep(data$id[i], 4),  # Repeat the ID to match the length of the other vectors
                                    x = c(data$x[i], data$xend_upper[i], data$xend[i], data$xend_lower[i]),  # Define the x-coordinates
                                    y = c(data$y[i], data$yend_upper[i], data$yend[i], data$yend_lower[i]),  # Define the y-coordinates
                                    group = rep(data$id[i], 4),  # Repeat group ID to match the length of the other vectors
                                    linewidth = rep(data$linewidth[i], 4),  # Inherit or repeat linewidth
                                    fill = rep(data$fill[i], 4),  # Inherit or repeat fill
                                    linetype = rep(data$linetype[i], 4),  # Inherit or repeat linetype
                                    alpha = rep(data$alpha[i], 4)  # Inherit or repeat alpha
                                  )
                                }))


                              } else {
                                polygon_data <- NULL
                              }

                              # Draw the polygon if polygon data exists
                              polygon_grob <- if (!is.null(polygon_data)) {

                                GeomPolygon$draw_panel(
                                  data = polygon_data,
                                  panel_params = panel_params,
                                  coord = coord
                                )
                              } else {
                                NULL
                              }

                              # Draw the vector lines
                              segments_grob <- GeomSegment$draw_panel(
                                data = data,
                                panel_params = panel_params,
                                coord = coord,
                                arrow = arrow
                              )

                              grid::gList(circle_grob, polygon_grob, segments_grob)

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
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}
