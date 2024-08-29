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
#' # Define the function
#' f <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(-1 - x^2 + y, 1 + x - y^2)
#' }
#'
#' # Generate sample points
#' set.seed(123)
#' sample_points <- data.frame(
#'   x = runif(10, min = -10, max = 10),
#'   y = runif(10, min = -10, max = 10)
#' )
#'
#' # Apply the function to each point
#' result <- t(apply(sample_points, 1, f))
#' sample_points$xend <- result[, 1]
#' sample_points$yend <- result[, 2]
#'
#' # Load ggplot2 library
#' library(ggplot2)
#'
#' # Plot the vectors
#' sample_points |>
#'   ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
#'     geom_vector_smooth(normalize = TRUE, center = TRUE, se = TRUE, arrow = grid::arrow()) +
#'     geom_vector(color = "red")
NULL

#' @rdname geom_vector_smooth
#' @export
StatVectorSmooth <- ggproto("StatVectorSmooth", Stat,

                            required_aes = c("x", "y"),

                            default_aes = aes(color = after_stat(norm), linewidth = 0.5, linetype = 1, alpha = 1),

                            compute_group = function(data, scales, n, center, method, normalize, scale_length, se = TRUE, level = .95, ...) {

                              # Ensure that n has the correct length
                              n <- ensure_length_two(n)

                              # Convert angle and distance to radians if they are provided
                              if ("angle" %in% names(data) && "distance" %in% names(data)) {
                                data$angle <- data$angle * pi / 180  # Convert angle to radians
                              } else if ("xend" %in% names(data) && "yend" %in% names(data)) {
                                # Convert xend/yend to angle/distance if not provided
                                data$u <- data$xend - data$x
                                data$v <- data$yend - data$y
                                data$distance <- sqrt(data$u^2 + data$v^2)
                                data$angle <- atan2(data$v, data$u)
                              } else {
                                stop("Either xend/yend or angle/distance must be provided.")
                              }

                              # Add sine and cosine of the angle for circular regression
                              data$sin_angle <- sin(data$angle)
                              data$cos_angle <- cos(data$angle)

                              # Generate the grid for predictions
                              x_seq <- seq(min(data$x), max(data$x), length.out = n[1])
                              y_seq <- seq(min(data$y), max(data$y), length.out = n[2])
                              grid <- expand.grid(x = x_seq, y = y_seq) |> mutate(id = row_number())

                              # Fit the models for smoothing using sine and cosine of the angle
                              if (method == "lm") {
                                fit_sin <- lm(sin_angle ~ x + y, data = data)
                                fit_cos <- lm(cos_angle ~ x + y, data = data)
                                fit_distance <- lm(distance ~ x + y, data = data)
                                message("`geom_vector_smooth()` using method = 'lm' and formula 'sin(angle) ~ x + y', 'cos(angle) ~ x + y', and 'distance ~ x + y'")

                                # Generate predictions
                                pred_sin <- predict(fit_sin, newdata = grid, se.fit = se)
                                pred_cos <- predict(fit_cos, newdata = grid, se.fit = se)
                                pred_distance <- predict(fit_distance, newdata = grid, se.fit = se)

                                if (!se) {
                                  grid$sin_angle_pred <- pred_sin
                                  grid$cos_angle_pred <- pred_cos
                                  grid$distance_pred <- pred_distance
                                } else {
                                  grid$sin_angle_pred <- pred_sin$fit
                                  grid$cos_angle_pred <- pred_cos$fit
                                  grid$distance_pred <- pred_distance$fit

                                  # Calculate the critical value for the t-distribution
                                  t_critical <- qt(1 - (1 - level) / 2, df = fit_sin$df.residual)

                                  # Adjust for the confidence level using the t-distribution
                                  grid$sin_angle_pred <- pred_sin$fit
                                  grid$cos_angle_pred <- pred_cos$fit
                                  grid$distance_pred <- pred_distance$fit

                                  grid$sin_angle_lower <- pred_sin$fit - t_critical * pred_sin$se.fit
                                  grid$sin_angle_upper <- pred_sin$fit + t_critical * pred_sin$se.fit
                                  grid$cos_angle_lower <- pred_cos$fit - t_critical * pred_cos$se.fit
                                  grid$cos_angle_upper <- pred_cos$fit + t_critical * pred_cos$se.fit

                                  # Convert predictions to angles
                                  grid$angle_pred <- atan2(grid$sin_angle_pred, grid$cos_angle_pred)
                                  grid$angle_lower <- atan2(grid$sin_angle_lower, grid$cos_angle_lower)
                                  grid$angle_upper <- atan2(grid$sin_angle_upper, grid$cos_angle_upper)

                                  # Normalize angles within [-pi, pi]
                                  grid$angle_pred <- normalize_angle(grid$angle_pred)
                                  grid$angle_lower <- normalize_angle(grid$angle_lower)
                                  grid$angle_upper <- normalize_angle(grid$angle_upper)

                                  # Handle wrapping cases
                                  if (any(grid$angle_upper < grid$angle_lower)) {
                                    grid$angle_upper[grid$angle_upper < grid$angle_lower] <- grid$angle_upper[grid$angle_upper < grid$angle_lower] + 2 * pi
                                  }

                                  # Adjust angle_pred if necessary to ensure it's within the interval
                                  grid$angle_pred <- ifelse(
                                    grid$angle_pred < grid$angle_lower, grid$angle_pred + 2 * pi,
                                    ifelse(grid$angle_pred > grid$angle_upper, grid$angle_pred - 2 * pi, grid$angle_pred)
                                  )
                                }
                              } else if (method == "loess") {
                                fit_sin <- loess(sin_angle ~ x + y, data = data)
                                fit_cos <- loess(cos_angle ~ x + y, data = data)
                                fit_distance <- loess(distance ~ x + y, data = data)
                                message("`geom_vector_smooth()` using method = 'loess' and formula 'sin(angle) ~ x + y', 'cos(angle) ~ x + y', and 'distance ~ x + y'")

                                # Generate predictions
                                grid$sin_angle_pred <- predict(fit_sin, newdata = grid)
                                grid$cos_angle_pred <- predict(fit_cos, newdata = grid)
                                grid$distance_pred <- predict(fit_distance, newdata = grid)

                                # Convert predictions to angles
                                grid$angle_pred <- atan2(grid$sin_angle_pred, grid$cos_angle_pred)

                                # Normalize angles within [-pi, pi]
                                grid$angle_pred <- normalize_angle(grid$angle_pred)
                              } else {
                                stop("Unsupported method. Please use 'lm' or 'loess'.")
                              }

                              # Convert predicted angle and distance back to xend and yend
                              grid$xend_pred <- grid$x + grid$distance_pred * cos(grid$angle_pred)
                              grid$yend_pred <- grid$y + grid$distance_pred * sin(grid$angle_pred)

                              if (se) {
                                # Convert the lower and upper angle bounds to xend/yend
                                grid$xend_lower <- grid$x + grid$distance_pred * cos(grid$angle_lower)
                                grid$yend_lower <- grid$y + grid$distance_pred * sin(grid$angle_lower)
                                grid$xend_upper <- grid$x + grid$distance_pred * cos(grid$angle_upper)
                                grid$yend_upper <- grid$y + grid$distance_pred * sin(grid$angle_upper)
                              }

                              # Calculate the norm of the predicted values
                              grid$norm_pred <- sqrt((grid$xend_pred - grid$x)^2 + (grid$yend_pred - grid$y)^2)

                              if (normalize) {
                                # Normalize the predicted vectors
                                grid$xend_pred <- grid$x + (grid$xend_pred - grid$x) / grid$norm_pred * scale_length
                                grid$yend_pred <- grid$y + (grid$yend_pred - grid$y) / grid$norm_pred * scale_length

                                if (se) {
                                  # Normalize the upper and lower bounds using their own norms
                                  grid$norm_lower <- sqrt((grid$xend_lower - grid$x)^2 + (grid$yend_lower - grid$y)^2)
                                  grid$norm_upper <- sqrt((grid$xend_upper - grid$x)^2 + (grid$yend_upper - grid$y)^2)

                                  grid$xend_lower <- grid$x + (grid$xend_lower - grid$x) / grid$norm_lower * scale_length
                                  grid$yend_lower <- grid$y + (grid$yend_lower - grid$y) / grid$norm_lower * scale_length

                                  grid$xend_upper <- grid$x + (grid$xend_upper - grid$x) / grid$norm_upper * scale_length
                                  grid$yend_upper <- grid$y + (grid$yend_upper - grid$y) / grid$norm_upper * scale_length
                                }
                              }

                              if (center) {
                                # Center the predicted vectors after normalization
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

                              # Check the lengths of the regular, upper, and lower vectors
                              grid$length_pred <- sqrt((grid$xend_pred - grid$x)^2 + (grid$yend_pred - grid$y)^2)
                              if (se) {
                                grid$length_lower <- sqrt((grid$xend_lower - grid$x)^2 + (grid$yend_lower - grid$y)^2)
                                grid$length_upper <- sqrt((grid$xend_upper - grid$x)^2 + (grid$yend_upper - grid$y)^2)

                                message("Length of predicted vector: ", round(mean(grid$length_pred), 4))
                                message("Length of lower bound vector: ", round(mean(grid$length_lower), 4))
                                message("Length of upper bound vector: ", round(mean(grid$length_upper), 4))
                              }

                              # Prepare the data to be returned
                              result <- data.frame(
                                x = grid$x,
                                y = grid$y,
                                xend = grid$xend_pred,
                                yend = grid$yend_pred,
                                norm = grid$norm_pred
                              )

                              if (se) {
                                result <- cbind(result, grid[, c("xend_lower", "xend_upper", "yend_lower", "yend_upper")])
                              }

                              return(result)
                            }
)



#' @rdname geom_vector_smooth
#' @export
GeomVectorSmooth <- ggproto("GeomVectorSmooth", GeomSegment,

                            required_aes = c("x", "y", "xend", "yend"),

                            default_aes = aes(linewidth = 0.5, linetype = 1, alpha = 1, fill = "grey80"),

                            draw_panel = function(data, panel_params, coord, arrow = NULL) {

                              # Reshape the data for polygons
                              data$id <- seq_len(nrow(data))

                              polygon_data <- do.call(rbind, lapply(1:nrow(data), function(i) {
                                data.frame(
                                  id = rep(data$id[i], 4),  # Repeat the ID to match the length of the other vectors
                                  x = c(data$x[i], data$xend_upper[i], data$xend[i], data$xend_lower[i]),
                                  y = c(data$y[i], data$yend_upper[i], data$yend[i], data$yend_lower[i]),
                                  group = rep(data$id[i], 4),  # Repeat group ID to match the length of the other vectors
                                  linewidth = rep(data$linewidth[i], 4),  # Inherit or repeat linewidth
                                  fill = rep(data$fill[i], 4),  # Inherit or repeat fill
                                  linetype = rep(data$linetype[i], 4),  # Inherit or repeat linetype
                                  alpha = rep(data$alpha[i], 4)  # Inherit or repeat alpha
                                )
                              }))

                              # Draw the polygon if polygon data exists
                              polygon_grob <- if (nrow(polygon_data) > 0) {
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

                              grid::gList(polygon_grob, segments_grob)
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
                               center = TRUE, normalize,
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
                               center = TRUE, normalize,
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
