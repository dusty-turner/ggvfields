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

                              # Generate the grid for predictions
                              x_seq <- seq(min(data$x), max(data$x), length.out = n[1])
                              y_seq <- seq(min(data$y), max(data$y), length.out = n[2])
                              grid <- expand.grid(x = x_seq, y = y_seq) |> mutate(id = row_number())

                              # Fit the models for smoothing
                              if (method == "lm") {
                                fit_x <- lm(xend ~ x + y, data = data)
                                fit_y <- lm(yend ~ x + y, data = data)
                                message("`geom_vector_smooth()` using method = 'lm' and formula 'xend ~ x + y' and 'yend ~ x + y'")
                                # Generate predictions
                                pred_x <- predict(fit_x, newdata = grid, se.fit = se)
                                pred_y <- predict(fit_y, newdata = grid, se.fit = se)

                                if(!se){
                                  grid$xend_pred <- pred_x
                                  grid$yend_pred <- pred_y
                                }

                                if (se) {

                                  grid$xend_pred <- pred_x$fit
                                  grid$yend_pred <- pred_y$fit

                                  grid <- grid %>%
                                    mutate(
                                      pred_x_lower = pred_x$fit - qt(1 - (1 - level) / 2, df = fit_x$df.residual) * pred_x$se.fit,
                                      pred_x_upper = pred_x$fit + qt(1 - (1 - level) / 2, df = fit_x$df.residual) * pred_x$se.fit,
                                      pred_y_lower = pred_y$fit - qt(1 - (1 - level) / 2, df = fit_y$df.residual) * pred_y$se.fit,
                                      pred_y_upper = pred_y$fit + qt(1 - (1 - level) / 2, df = fit_y$df.residual) * pred_y$se.fit
                                    )
                                }

                              } else if (method == "loess") {
                                fit_x <- loess(xend ~ x, data = data)
                                fit_y <- loess(yend ~ y, data = data)
                                message("`geom_vector_smooth()` using method = 'loess' and formula 'xend ~ x' and 'yend ~ y'")
                                # Generate predictions
                                grid$xend_pred <- predict(fit_x, newdata = grid[, 1])
                                grid$yend_pred <- predict(fit_y, newdata = grid[, 2])
                              } else {
                                stop("Unsupported method. Please use 'lm' or 'loess'.")
                              }

                              # Calculate the norm of the predicted values
                              grid$norm_pred <- sqrt((grid$xend_pred - grid$x)^2 + (grid$yend_pred - grid$y)^2)

                              if (normalize) {
                                # Normalize the predicted vectors
                                grid$xend_pred <- grid$x + (grid$xend_pred - grid$x) / grid$norm_pred * scale_length
                                grid$yend_pred <- grid$y + (grid$yend_pred - grid$y) / grid$norm_pred * scale_length

                                if (se) {
                                  # Normalize the upper and lower bounds
                                  grid$pred_x_lower <- grid$x + (grid$pred_x_lower - grid$x) / grid$norm_pred * scale_length
                                  grid$pred_x_upper <- grid$x + (grid$pred_x_upper - grid$x) / grid$norm_pred * scale_length
                                  grid$pred_y_lower <- grid$y + (grid$pred_y_lower - grid$y) / grid$norm_pred * scale_length
                                  grid$pred_y_upper <- grid$y + (grid$pred_y_upper - grid$y) / grid$norm_pred * scale_length
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
                                  grid$pred_x_lower <- grid$pred_x_lower - half_u
                                  grid$pred_x_upper <- grid$pred_x_upper - half_u
                                  grid$pred_y_lower <- grid$pred_y_lower - half_v
                                  grid$pred_y_upper <- grid$pred_y_upper - half_v
                                }
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
                                result <- cbind(result, grid[, c("pred_x_lower", "pred_x_upper", "pred_y_lower", "pred_y_upper")])
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

                              if ("pred_x_lower" %in% names(data) && "pred_x_upper" %in% names(data) &&
                                  "pred_y_lower" %in% names(data) && "pred_y_upper" %in% names(data)) {

                                # Reshape the data for polygons
                                data$id <- seq_len(nrow(data))
                                polygon_data <- do.call(rbind, lapply(1:nrow(data), function(i) {
                                  data.frame(
                                    id = data$id[i],
                                    x = c(data$x[i], data$pred_x_upper[i], data$xend[i], data$pred_x_lower[i]),
                                    y = c(data$y[i], data$pred_y_upper[i], data$yend[i], data$pred_y_lower[i]),
                                    group = data$id[i],
                                    linewidth = data$linewidth[i],  # Carry over any necessary aesthetics
                                    fill = data$fill[i],
                                    linetype = data$linetype[i],
                                    alpha = data$alpha[i]
                                  )
                                }))

                                # Draw the polygon if polygon data exists
                                polygon_grob <- if (nrow(polygon_data) > 0) {
                                  GeomPolygon$draw_panel(
                                    data = polygon_data,
                                    panel_params = panel_params,
                                    coord = coord)
                                } else {
                                  NULL
                                }
                              } else {
                                polygon_grob <- NULL
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
