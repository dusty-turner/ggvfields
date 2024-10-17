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
#' @param scale_factor Numeric; scales the length of the vectors to a given
#'   value. Useful for ensuring consistent lengths for visualization.
#' @param center Logical; if `TRUE`, centers the vector on the evaluated x/y
#'   location. If `FALSE`, the vector origin is at the evaluated x/y location.
#'   When centering is enabled, the vector's midpoint aligns with the original
#'   x/y location.
#' @param normalize Logical; if `TRUE`, normalizes the vector's length to a unit
#'   length before applying other transformations like centering. If `FALSE`,
#'   vectors retain their original lengths.
#' @param method Character; specifies the smoothing method to be used.
#'   Currently, only `"lm"` (linear modeling) is supported.
#' @param se Logical; if `TRUE`, plots the confidence intervals around the
#'   smoothed vectors.
#' @param se.circle Logical; if `TRUE`, draws circles around the origin of the
#'   vectors to represent the radius of the confidence interval. This is useful
#'   for visualizing the spread and variability of the confidence intervals when
#'   `se = TRUE`.
#' @param probs Numeric vector; specifies the prediction interval level(s) to be
#'   plotted when `se = TRUE`. **Default is `probs = c(0.95, NA)`**. If only one
#'   value is provided (e.g., `probs = 0.95`), a single prediction interval is
#'   drawn. If multiple values are provided, the function will plot multiple
#'   intervals (with smaller intervals nested inside larger ones).
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

#' @rdname geom_vector_smooth
#' @export
StatVectorSmooth <- ggproto(
  "StatVectorSmooth",
  Stat,
  required_aes = c("x", "y", "dx", "dy"),
  default_aes = aes(color = "#3366FF", linewidth = 0.5, linetype = 1, alpha = 1),

  compute_group = function(data, scales, n, center, method, normalize = TRUE, scale_factor, se = TRUE, probs, ...) {

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
      model <- lm(cbind(dx, dy) ~ x * y, data = data)

      # Extract the covariance matrix of the coefficients
      V <- vcov(model)

      # Compute the model matrix for the new data named grid
      X <- model.matrix(~ x * y, grid)

      # Initialize matrices to store prediction variances for each response
      n_preds <- ncol(model$coefficients)  # Number of responses (dx, dy)
      pred_var <- matrix(NA, nrow = nrow(X), ncol = n_preds)

      # Compute prediction variances for each response
      for (i in 1:n_preds) {
        idx <- ((i - 1) * ncol(X) + 1):(i * ncol(X))  # Extract block for response i
        V_i <- V[idx, idx]
        pred_var[, i] <- diag(X %*% V_i %*% t(X))  # Compute variances
      }

      # Compute standard errors for each response
      se_values <- sqrt(pred_var)

      # Get point predictions for all responses
      preds <- predict(model, grid)

      # Initialize a list to store interval data
      interval_data <- list()

      # Iterate over the probabilities and store appropriately named intervals
      for (i in seq_along(probs)) {
        # Determine the interval type (outer or inner)
        interval_type <- ifelse(i == 1, "outer", "inner")

        # Compute the critical value for the current probability
        t_value <- qt(1 - (1 - probs[i]) / 2, df = model$df.residual)

        # Compute the lower and upper prediction intervals
        lwr <- preds - t_value * se_values
        upr <- preds + t_value * se_values

        # Store the intervals with custom naming convention
        interval_data[[interval_type]] <- bind_cols(
          as_tibble(lwr) %>% rename_with(~paste0(c("xend", "yend"), "_lower_", interval_type)),
          as_tibble(upr) %>% rename_with(~paste0(c("xend", "yend"), "_upper_", interval_type))
        )

      }

      # Combine all interval data with the main grid and predictions
      grid <- bind_cols(
        grid,
        as_tibble(preds) %>% rename_with(~paste0("fit_", .)),
        bind_cols(interval_data)  # Flatten the list of intervals into columns
      )
    }

    # Optionally normalize vector lengths to a fixed scale
    if (normalize) {
      grid$norm_pred <- sqrt((grid$fit_dx - grid$x)^2 + (grid$fit_dy - grid$y)^2)
      grid$fit_dx <- grid$x + (grid$fit_dx - grid$x) / grid$norm_pred * scale_factor
      grid$fit_dy <- grid$y + (grid$fit_dy - grid$y) / grid$norm_pred * scale_factor
    }

    # Optionally center vectors around their midpoint
    if (center) {
      half_u <- (grid$fit_dx - grid$x) / 2
      half_v <- (grid$fit_dy - grid$y) / 2

      grid$x <- grid$x - half_u
      grid$y <- grid$y - half_v
      grid$fit_dx <- grid$fit_dx - half_u
      grid$fit_dy <- grid$fit_dy - half_v
    }

    # Prepare the result with relevant columns
    result <- data.frame(
      x = grid$x,
      y = grid$y,
      xend = grid$fit_dx,
      yend = grid$fit_dy,
      radius = rep(radius, nrow(grid)),
      dx = grid$fit_dx - grid$x,
      dy = grid$fit_dy - grid$y
    )

    # If confidence intervals are enabled, include them in the result
    if (se) {
      result <- bind_cols(
        result,
        grid %>% select(contains("end_"))
      )
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

      data$xend <- data$x + (data$xend - data$x) * params$scale_factor
      data$yend <- data$y + (data$yend - data$y) * params$scale_factor

    }

    return(data)
  },

  draw_panel = function(data, panel_params, coord, arrow = NULL, se = TRUE, se.circle = TRUE, scale_factor) {
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
          circle_data$alpha <- 0.6
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
        wedge$alpha <- 0.6
        wedge$fill <- "grey60"
        # wedge$fill <- data$fill[i]
        wedge$colour <- "grey60"
        return(wedge)
      }))

      wedge_grob_outer <- GeomPolygon$draw_panel(
        data = wedge_data_outer,
        panel_params = panel_params,
        coord = coord
      )

      # Draw inner wedge only if inner bounds exist (probs[2] was not NA)
      if (!is.na(data$xend_lower_inner[1])) {

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
          wedge$alpha <- 0.6
          wedge$fill <- "grey60"
          # wedge$fill <- data$fill[i]
          wedge$colour <- "grey60"
          return(wedge)
        }))

        print("here")

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
  scale_factor = 1,
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
      scale_factor = scale_factor,
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
  n = c(11, 11), scale_factor = 1,
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
      scale_factor = scale_factor,
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
