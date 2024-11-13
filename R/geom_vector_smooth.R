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
#' @param method Character; specifies the smoothing method to be used.
#'   Supported methods are `"lm"` (linear modeling) and `"boot"` (bootstrapping).
#'   `"boot"` generates smoother results by calculating angles with bootstrapping,
#'   and the prediction intervals are determined using quantiles.
#' @param se Logical; if `TRUE`, plots the confidence intervals around the
#'   smoothed vectors.
#' @param se.circle Logical; if `TRUE`, draws circles around the origin of the
#'   vectors to represent the radius of the confidence interval. This is useful
#'   for visualizing variability when `se = TRUE`.
#' @param probs Numeric vector; specifies the prediction interval levels to be
#'   plotted when `se = TRUE`. **Default is `probs = 0.95`**, but users can specify
#'   multiple levels (e.g., `probs = c(0.95, 0.68)`).
#' @param eval_points Number of points at which the function is evaluated for smoothing.
#' @param arrow Arrow specification, as created by `grid::arrow()`. This
#'   controls the appearance of the arrowheads at the end of the vectors,
#'   including properties like angle, length, and type.
#' @param default_formula A formula specifying the model to fit for smoothing.
#' Defaults to `cbind(dx, dy) ~ x * y`.
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a
#'   smooth vector field plot.
#' @importFrom stats qt
#' @name geom_vector_smooth
#' @rdname geom_vector_smooth
#'
#' @section Aesthetics:
#' `geom_vector_smooth` understands the following aesthetics (required aesthetics are in bold):
#'
#' - **`x`**: x-coordinate of the starting point of the vector.
#' - **`y`**: y-coordinate of the starting point of the vector.
#' - **`dx`**: x-displacement of the vector.
#' - **`dy`**: y-displacement of the vector.
#' - `angle`: The angle of the vector in radians (optional, used with `distance`).
#' - `distance`: The magnitude of the vector (optional, used with `angle`).
#' - `color`: The color of the vector line.
#' - `linewidth`: The thickness of the vector line.
#' - `linetype`: The type of the vector line (solid, dashed, etc.).
#' - `alpha`: The transparency level of the vector.
#' - `arrow`: Specification for arrowheads at the end of the vector.
#'
#'   Additionally, when using smoothing:
#' - `norm`: A computed variable representing the magnitude of the smoothed vector,
#'   available via `after_stat()`.
#'
#' @details
#' **Mathematics of Prediction and Prediction Intervals**:
#'
#'
#' This section explains the methods for computing predictions and prediction
#' intervals using the **x** and **y** coordinates.
#'
#' #### Linear Model (lm) Method
#'
#' The `"lm"` method fits a multivariate linear regression to predict the vector
#' displacements *dx* and *dy* based on **x** and **y**.
#'
#' **Model:**
#' ```
#' dx, dy = b_0 + b_1 * x + b_2 * y + b_3 * (x * y) + e
#' ```
#' - *b_0*, *b_1*, *b_2*, and *b_3* are the model coefficients.
#' - *e* is the residual error.
#'
#' **Prediction:**
#' For a new grid point, the predicted displacements are:
#' ```
#' Z_i = X_i * beta
#' ```
#' - *X_i* is the design matrix for the new grid point.
#' - *beta* is the vector of estimated coefficients.
#'
#' **Prediction Intervals:**
#' The standard error of the prediction is:
#' ```
#' SE(Z_i) = sqrt(diag(X_i * V * t(X_i)))
#' ```
#' - *V* is the covariance matrix of the coefficients.
#'
#' The prediction interval at confidence level (1 - alpha) is:
#' ```
#' Z_i ± t_(alpha / 2) * SE(Z_i)
#' ```
#'
#' #### Bootstrapping (boot) Method
#'
#' The `"boot"` method uses resampling to estimate angles and displacements.
#'
#' **Process:**
#' - Resample the original data with replacement.
#' - Fit models for the sine and cosine of the vector angle:
#'   ```
#'   sin(theta) = b_0 + b_1 * x + b_2 * y + e
#'   ```
#'   ```
#'   cos(theta) = b_0 + b_1 * x + b_2 * y + e
#'   ```
#' - Use the predicted sine and cosine to compute:
#'   ```
#'   theta = atan2(sin(theta), cos(theta))
#'   ```
#'
#' **Prediction Intervals:**
#' Based on quantiles from bootstrapped angles:
#' ```
#' Lower Bound = Q_(alpha / 2)(theta)
#' Upper Bound = Q_(1 - alpha / 2)(theta)
#' ```
#'
#' **Displacements:**
#' Using predicted angle *theta* and distance *d*:
#' ```
#' dx = d * cos(theta), dy = d * sin(theta)
#' ```
#'
#' @examples
#' library(ggvfields)
#'
#' # Function to generate random vectors based on (x, y) inputs
#' generate_vectors <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(sin(x) + sin(y) + rnorm(1, 5, 3), sin(x) - sin(y) + rnorm(1, 5, 3))
#' }
#'
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Generate sample data
#' sample_points <- data.frame(
#'   x = runif(50, min = -10, max = 10),
#'   y = runif(50, min = -10, max = 10)
#' )
#'
#' # Apply the generate_vectors function to each row
#' result <- t(apply(sample_points, 1, generate_vectors))
#'
#' # Create new columns for displacements (dx, dy) and polar coordinates
#' sample_points$xend <- result[, 1]
#' sample_points$yend <- result[, 2]
#' sample_points$dx <- sample_points$xend - sample_points$x
#' sample_points$dy <- sample_points$yend - sample_points$y
#' sample_points$distance <- sqrt(sample_points$dx^2 + sample_points$dy^2)
#' sample_points$angle <- atan2(sample_points$dy, sample_points$dx)
#'
#' # Example 1: Cartesian Coordinates with Linear Model (lm)
#' ggplot(sample_points, aes(x = x, y = y)) +
#'   geom_vector_smooth(aes(dx = dx, dy = dy), method = "lm", se = TRUE) +
#'   geom_vector(aes(dx = dx, dy = dy), color = "black") +
#'   ggtitle("Vector Smoothing with Linear Model (lm)")
#'
#' # Example 2: Polar Coordinates with Bootstrapping (boot)
#' ggplot(sample_points, aes(x = x, y = y, angle = angle, distance = distance)) +
#'   geom_vector_smooth(method = "boot", se = TRUE, probs = c(0.95, 0.68)) +
#'   geom_vector(aes(dx = dx, dy = dy), color = "black") +
#'   ggtitle("Vector Smoothing with Bootstrapping (boot)")
#'
NULL


#' @rdname geom_vector_smooth
#' @export
StatVectorSmooth <- ggproto(
  "StatVectorSmooth",
  Stat,
  required_aes = c("x", "y"),  # Supports both Cartesian and Polar coordinates
  dropped_aes = c("distance", "angle"),
  default_aes = aes(
    color = "#3366FF", linewidth = 0.5, linetype = 1, alpha = 1,
    angle = NA, distance = NA, dx = NA, dy = NA
  ),

  setup_data = function(data, params) {
    if (!all(is.na(data$dx)) && !all(is.na(data$angle))) {
      warning("Both Cartesian and Polar inputs provided. Using Cartesian by default.")
    }

    # Ensure 'dx' and 'dy' are present
    if (!("dx" %in% names(data) && "dy" %in% names(data))) {
      if ("angle" %in% names(data) && "distance" %in% names(data)) {
        data$dx <- data$distance * cos(data$angle)
        data$dy <- data$distance * sin(data$angle)
      } else {
        stop("Either 'dx' and 'dy' or 'angle' and 'distance' must be provided.")
      }
    }

    return(data)
  },

  compute_group = function(
    data, scales, n,
    method, scale_factor, se = TRUE, probs,
    eval_points = NULL, formula, ...
  ) {
    n <- ensure_length_two(n)

    # Create grid for evaluation
    if (!is.null(eval_points)) {
      if (!all(c("x", "y") %in% names(eval_points))) {
        stop("The 'eval_points' argument must contain 'x' and 'y' columns.")
      }
      grid <- eval_points

      if (nrow(grid) > 1) {
        min_distance <- euclidean_distances(grid)
        base_radius <- min_distance / 2.5
      } else {
        data_range_x <- diff(range(data$x))
        data_range_y <- diff(range(data$y))
        data_range <- min(data_range_x, data_range_y)
        base_radius <- data_range / 2.5
      }

    } else {
      x_seq <- seq(min(data$x), max(data$x), length.out = n[1])
      y_seq <- seq(min(data$y), max(data$y), length.out = n[2])
      grid <- expand.grid(x = x_seq, y = y_seq)
      x_spacing <- diff(sort(unique(grid$x)))[1]
      y_spacing <- diff(sort(unique(grid$y)))[1]
      base_radius <- min(x_spacing, y_spacing) / 2.5
    }

    grid$id <- 1:nrow(grid)

    # Ensure probs is a vector, even if a single value is provided
    if (length(probs) == 1) {
      probs <- c(probs, NA)
    }

    # Calculate xend and yend using dx and dy
    data$xend <- data$x + data$dx
    data$yend <- data$y + data$dy

    # Calculate angle and distance
    data$distance <- sqrt(data$dx^2 + data$dy^2)
    data$angle <- atan2(data$dy, data$dx)

    if (method == "lm") {
      # Fit the multivariate linear model
      angle_model <- lm(formula = formula, data = data)
      V <- vcov(angle_model)
      response_vars <- all.vars(formula[[2]])
      predictor_vars <- all.vars(formula[[3]])
      rhs_formula <- as.formula(paste("~", paste(predictor_vars, collapse = "+")))
      X <- model.matrix(rhs_formula, grid)
      n_preds <- length(response_vars)
      n_params <- ncol(X)
      pred_var <- matrix(NA, nrow = nrow(X), ncol = n_preds)

      # Compute prediction variances for each response
      for (i in 1:n_preds) {
        idx <- ((i - 1) * n_params + 1):(i * n_params)
        V_i <- V[idx, idx]
        pred_var[, i] <- diag(X %*% V_i %*% t(X))
      }

      se_values <- sqrt(pred_var)  # Standard errors for each response
      preds <- predict(angle_model, newdata = grid)  # Predicted dx/dy

      interval_data <- list()

      # Iterate over the probabilities and store appropriately named intervals
      for (i in seq_along(probs)) {
        interval_type <- ifelse(i == 1, "outer", "inner")
        p <- n_preds
        df_residual <- angle_model$df.residual

        # Critical value for Hotelling's T-squared
        T2_crit <- (df_residual) * p / (df_residual - p + 1) * qf(probs[i], p, df_residual - p + 1)
        scale_val <- sqrt(T2_crit)

        # Apply scale_val to get prediction intervals
        lwr <- preds - scale_val * se_values
        upr <- preds + scale_val * se_values

        # Store the intervals with custom naming convention for dx and dy
        interval_data[[interval_type]] <- cbind(
          setNames(data.frame(lwr), paste0(c("fit_dx", "fit_dy"), "_lower_", interval_type)),
          setNames(data.frame(upr), paste0(c("fit_dx", "fit_dy"), "_upper_", interval_type))
        )
      }
      grid <- cbind(
        grid,
        setNames(data.frame(preds), paste0("fit_", c("dx", "dy"))),
        do.call(cbind, unname(interval_data))
      )

    } else if (method == "boot") {
      grid <- perform_bootstrapping(data, grid, probs, se)
    }

    # Prepare the result with relevant columns
    result <- data.frame(
      x = grid$x,
      y = grid$y,
      dx = grid$fit_dx,
      dy = grid$fit_dy
    )

    if (se) {
      # Add dx and dy bounds
      result$dx_lower_outer <- grid$fit_dx_lower_outer
      result$dy_lower_outer <- grid$fit_dy_lower_outer
      result$dx_upper_outer <- grid$fit_dx_upper_outer
      result$dy_upper_outer <- grid$fit_dy_upper_outer

      # Add inner bounds if they exist
      if ("fit_dx_lower_inner" %in% colnames(grid)) {
        result$dx_lower_inner <- grid$fit_dx_lower_inner
        result$dy_lower_inner <- grid$fit_dy_lower_inner
        result$dx_upper_inner <- grid$fit_dx_upper_inner
        result$dy_upper_inner <- grid$fit_dy_upper_inner
      }
    }

    # Adjust vector lengths using scale_factor
    result$distance <- sqrt(result$dx^2 + result$dy^2)
    result$dx_norm <- result$dx / result$distance
    result$dy_norm <- result$dy / result$distance

    # Determine the scaling magnitude
    if (is.null(eval_points)) {
      est_magnitude <- rep(base_radius, nrow(result))
    } else {
      est_magnitude <- result$distance  # Or adjust as needed
    }

    # Apply scale_factor if provided
    est_magnitude <- est_magnitude * scale_factor

    # Scale dx and dy
    result$dx <- result$dx_norm * est_magnitude
    result$dy <- result$dy_norm * est_magnitude

    # Recompute xend and yend
    result$xend <- result$x + result$dx
    result$yend <- result$y + result$dy

    # Scale confidence intervals accordingly
    if (se) {
      # For outer bounds
      distance_lower_outer <- sqrt(result$dx_lower_outer^2 + result$dy_lower_outer^2)
      dx_lower_outer_norm <- result$dx_lower_outer / distance_lower_outer
      dy_lower_outer_norm <- result$dy_lower_outer / distance_lower_outer
      result$dx_lower_outer <- dx_lower_outer_norm * est_magnitude
      result$dy_lower_outer <- dy_lower_outer_norm * est_magnitude
      result$xend_lower_outer <- result$x + result$dx_lower_outer
      result$yend_lower_outer <- result$y + result$dy_lower_outer

      distance_upper_outer <- sqrt(result$dx_upper_outer^2 + result$dy_upper_outer^2)
      dx_upper_outer_norm <- result$dx_upper_outer / distance_upper_outer
      dy_upper_outer_norm <- result$dy_upper_outer / distance_upper_outer
      result$dx_upper_outer <- dx_upper_outer_norm * est_magnitude
      result$dy_upper_outer <- dy_upper_outer_norm * est_magnitude
      result$xend_upper_outer <- result$x + result$dx_upper_outer
      result$yend_upper_outer <- result$y + result$dy_upper_outer

      # For inner bounds, if they exist
      if ("dx_lower_inner" %in% names(result)) {
        distance_lower_inner <- sqrt(result$dx_lower_inner^2 + result$dy_lower_inner^2)
        dx_lower_inner_norm <- result$dx_lower_inner / distance_lower_inner
        dy_lower_inner_norm <- result$dy_lower_inner / distance_lower_inner
        result$dx_lower_inner <- dx_lower_inner_norm * est_magnitude
        result$dy_lower_inner <- dy_lower_inner_norm * est_magnitude
        result$xend_lower_inner <- result$x + result$dx_lower_inner
        result$yend_lower_inner <- result$y + result$dy_lower_inner

        distance_upper_inner <- sqrt(result$dx_upper_inner^2 + result$dy_upper_inner^2)
        dx_upper_inner_norm <- result$dx_upper_inner / distance_upper_inner
        dy_upper_inner_norm <- result$dy_upper_inner / distance_upper_inner
        result$dx_upper_inner <- dx_upper_inner_norm * est_magnitude
        result$dy_upper_inner <- dy_upper_inner_norm * est_magnitude
        result$xend_upper_inner <- result$x + result$dx_upper_inner
        result$yend_upper_inner <- result$y + result$dy_upper_inner
      }
    }

    # Remove temporary columns
    result$dx_norm <- NULL
    result$dy_norm <- NULL
    result$distance <- NULL

    # Include any other necessary columns
    result$est_magnitude <- est_magnitude
    result$id <- grid$id

    return(result)

  }
)




#' @rdname geom_vector_smooth
#' @export
GeomVectorSmooth <- ggproto(
  "GeomVectorSmooth",
  GeomSegment,
  required_aes = c("x", "y", "xend", "yend"),
  default_aes = aes(
    linewidth = 0.5, linetype = 1, alpha = 1,
    fill = "grey80", color = "#3366FF"
  ),

  setup_data = function(data, params) {
    data$id <- seq_len(nrow(data))
    return(data)
  },

  draw_panel = function(
    data, panel_params, coord,
    arrow = NULL, se = TRUE, se.circle = TRUE,
    scale_factor, eval_points
  ) {
    grobs <- list()
print(data)
    if (se) {
      # Draw wedges for outer confidence intervals using GeomPolygon
      if ("xend_upper_outer" %in% names(data) && "xend_lower_outer" %in% names(data)) {
        wedge_data_outer <- do.call(rbind, lapply(1:nrow(data), function(i) {
          wedge <- create_wedge_data(
            x = data$x[i], y = data$y[i],
            xend_upper = data$xend_upper_outer[i], yend_upper = data$yend_upper_outer[i],
            xend_lower = data$xend_lower_outer[i], yend_lower = data$yend_lower_outer[i],
            xend = data$xend[i], yend = data$yend[i],
            id = data$id[i],
            radius = data$est_magnitude[i],
            n_points = 50
          )
          wedge$linewidth <- data$linewidth[i]
          wedge$alpha <- .9
          # wedge$alpha <- data$alpha[i] * 0.4
          wedge$fill <- "grey60"
          wedge$fill <- data$fill[i]
          wedge$colour <- NA
          return(wedge)
        }))

        wedge_grob_outer <- GeomPolygon$draw_panel(
          data = wedge_data_outer,
          panel_params = panel_params,
          coord = coord
        )

        # Extract grobs from the grob tree
        grobs <- c(grobs, grid::gList(wedge_grob_outer))
      }

      # Draw wedges for inner confidence intervals if present
      if (!is.na(data$xend_upper_inner[1])) {
        wedge_data_inner <- do.call(rbind, lapply(1:nrow(data), function(i) {
          wedge <- create_wedge_data(
            x = data$x[i], y = data$y[i],
            xend_upper = data$xend_upper_inner[i], yend_upper = data$yend_upper_inner[i],
            xend_lower = data$xend_lower_inner[i], yend_lower = data$yend_lower_inner[i],
            xend = data$xend[i], yend = data$yend[i],
            id = data$id[i],
            radius = data$est_magnitude[i],
            n_points = 50
          )
          wedge$linewidth <- data$linewidth[i]
          wedge$alpha <- .4
          # wedge$alpha <- data$alpha[i] * 0.2
          wedge$fill <- "grey40"
          # wedge$fill <- data$fill[i]
          wedge$colour <- NA
          return(wedge)
        }))

        wedge_grob_inner <- GeomPolygon$draw_panel(
          data = wedge_data_inner,
          panel_params = panel_params,
          coord = coord
        )

        # Extract grobs from the grob tree
        grobs <- c(grobs, grid::gList(wedge_grob_inner))
      }

      # Draw circles if se.circle is TRUE
      if (se.circle) {
        all_circle_data <- do.call(rbind, lapply(1:nrow(data), function(i) {
          circle_data <- create_circle_data(
            x = data$x[i], y = data$y[i],
            radius = data$est_magnitude[i]
          )
          circle_data$group <- data$id[i]
          circle_data$linewidth <- data$linewidth[i]
          circle_data$alpha <- data$alpha[i] * 0.6
          circle_data$fill <- NA
          circle_data$colour <- "grey60"
          return(circle_data)
        }))

        circle_grob <- GeomPolygon$draw_panel(
          data = all_circle_data,
          panel_params = panel_params,
          coord = coord
        )

        # Extract grobs from the grob tree
        grobs <- c(grobs, grid::gList(circle_grob))
      }
    }

    # Draw the main vectors
    segments_grob <- GeomSegment$draw_panel(
      data, panel_params, coord, arrow = arrow
    )

    # Extract grobs from the grob tree
    grobs <- c(grobs, grid::gList(segments_grob))

    # Combine all grobs into a single grobTree
    combined_grob <- grid::grobTree(children = do.call(grid::gList, grobs))

    return(combined_grob)
  },

  draw_key = draw_key_smooth  # Ensure this function is defined elsewhere
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
  # center = TRUE,
  # normalize = TRUE,
  method = "lm",
  se = TRUE,
  se.circle = TRUE,
  probs = c(.95, NA),
  default_formula = cbind(dx, dy) ~ x * y,
  arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
  eval_points = NULL,
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
      # center = center,
      # normalize = normalize,
      scale_factor = scale_factor,
      method = method,
      se = se,
      se.circle = se.circle,
      probs = probs,
      arrow = arrow,
      eval_points = eval_points,
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
  # center = TRUE, normalize = TRUE,
  method = "lm",
  se = TRUE,
  se.circle = TRUE,
  probs = c(.95, NA),
  default_formula = cbind(dx, dy) ~ x * y,
  arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
  eval_points = NULL,
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
      # center = center,
      # normalize = normalize,
      scale_factor = scale_factor,
      method = method,
      se = se,
      se.circle = se.circle,
      probs = probs,
      arrow = arrow,
      eval_points = eval_points,
      formula = default_formula,
      na.rm = na.rm,
      ...
    )
  )
}
