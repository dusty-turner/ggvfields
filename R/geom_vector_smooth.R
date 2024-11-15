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

  compute_group = function(
    data, scales, n,
    method, scale_factor, se = TRUE, probs,
    eval_points = NULL, formula, ...
  ) {
    # Ensure 'n' is a numeric vector of length 2
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

    # Calculate xend and yend using dx and dy
    data$xend <- data$x + data$dx
    data$yend <- data$y + data$dy

    # Calculate angle and distance
    data$distance <- sqrt(data$dx^2 + data$dy^2)
    data$angle <- atan2(data$dy, data$dx)

    # ----------------------------
    # 2. Model Fitting and Prediction
    # ----------------------------

    # Fit the multivariate linear model
    model_mv <- lm(formula, data = data)

    # Predict dx and dy for the grid
    predictions_mv <- predict(model_mv, newdata = grid)

    # Extract predicted means (ensure correct column names)
    grid$dx <- predictions_mv[, "dx"]  # Predicted dx
    grid$dy <- predictions_mv[, "dy"]  # Predicted dy

    # ----------------------------
    # 3. Covariance Matrix Computation
    # ----------------------------

    # Extract the covariance matrix of the model coefficients
    cov_coef <- vcov(model_mv)

    # Create the design matrix for the grid
    design_matrix <- model.matrix(formula, data = grid)

    # Number of response variables (dx and dy)
    n_resp <- 2

    # Extract covariance matrices
    # Assuming the coefficients are ordered as:
    # (Intercept), x, y, x:y for dx, then same for dy
    # Adjust the indices if your model has a different order
    # Here, assuming that for two responses, coefficients are:
    # dx:(Intercept), dx:x, dx:y, dx:x:y, dy:(Intercept), dy:x, dy:y, dy:x:y

    # Number of coefficients per response
    coeffs_per_response <- length(coef(model_mv)[,1])

    # Check the number of coefficients
    total_coeffs <- ncol(cov_coef)
    expected_coeffs <- 4 * n_resp  # For formula cbind(dx, dy) ~ x * y
    if (total_coeffs != expected_coeffs) {
      stop("Unexpected number of coefficients in the model. Please verify the formula and data.")
    }

    # Extract covariance matrices
    cov_beta_dx <- cov_coef[1:4, 1:4]     # Covariance for dx coefficients
    cov_beta_dy <- cov_coef[5:8, 5:8]     # Covariance for dy coefficients
    cov_beta_dx_dy <- cov_coef[1:4, 5:8]  # Covariance between dx and dy coefficients

    # Compute variance for dx and dy predictions
    var_dx <- rowSums((design_matrix %*% cov_beta_dx) * design_matrix)
    var_dy <- rowSums((design_matrix %*% cov_beta_dy) * design_matrix)
    cov_dx_dy <- rowSums((design_matrix %*% cov_beta_dx_dy) * design_matrix)

    # Assemble the covariance matrix for (dx, dy) predictions
    cov_pred <- data.frame(var_dx, var_dy, cov_dx_dy)

    # ----------------------------
    # 4. Simulation for Prediction Intervals
    # ----------------------------

    # Number of simulations per grid point
    n_sim <- 1000  # Adjust as needed for accuracy vs performance

    # Initialize matrix to store theta simulations
    theta_sim_matrix <- matrix(NA, nrow = nrow(grid), ncol = n_sim)

    for (i in 1:nrow(grid)) {
      mu <- c(grid$dx[i], grid$dy[i])
      sigma <- matrix(c(cov_pred$var_dx[i], cov_pred$cov_dx_dy[i],
                        cov_pred$cov_dx_dy[i], cov_pred$var_dy[i]), nrow = 2)

      # Ensure the covariance matrix is positive definite
      if (!all(eigen(sigma)$values > 0)) {
        # Adjust the covariance matrix if necessary
        sigma <- sigma + diag(1e-6, 2)
      }

      # Simulate dx and dy
      simulations <- MASS::mvrnorm(n = n_sim, mu = mu, Sigma = sigma)
      sim_dx <- simulations[, 1]
      sim_dy <- simulations[, 2]
      sim_theta <- atan2(sim_dy, sim_dx)

      # Store simulated theta
      theta_sim_matrix[i, ] <- sim_theta
    }

    # ----------------------------
    # 5. Circular Statistics for Prediction Intervals
    # ----------------------------

    # Compute circular mean for each grid point
    theta_mean <- apply(theta_sim_matrix, 1, function(theta) {
     circular::mean.circular(circular::circular(theta, units = "radians", modulo = "2pi"))
    })

    # Compute theta_lower and theta_upper
    theta_lower <- mapply(
      compute_circular_quantile,
      theta = as.data.frame(t(theta_sim_matrix)),
      theta_mean = theta_mean,
      prob = 0.025
    ) |> as.numeric()

    theta_upper <- mapply(
      compute_circular_quantile,
      theta = as.data.frame(t(theta_sim_matrix)),
      theta_mean = theta_mean,
      prob = 0.975
    ) |> as.numeric()

    # ----------------------------
    # 6. Data Preparation for Geom
    # ----------------------------

    # Add theta values to grid
    grid <- grid %>%
      mutate(
        theta = theta_mean,
        theta_lower = theta_lower,
        theta_upper = theta_upper
      )

    # Calculate dx and dy for the mean theta
    grid <- grid %>%
      mutate(
        dx = scale_factor * cos(theta) * base_radius,
        dy = scale_factor * sin(theta) * base_radius,
        xend = x + dx,
        yend = y + dy,

        # Calculate dx and dy for theta_lower
        dx_lower = scale_factor * cos(theta_lower) * base_radius,
        dy_lower = scale_factor * sin(theta_lower) * base_radius,
        xend_lower = x + dx_lower,
        yend_lower = y + dy_lower,

        # Calculate dx and dy for theta_upper
        dx_upper = scale_factor * cos(theta_upper) * base_radius,
        dy_upper = scale_factor * sin(theta_upper) * base_radius,
        xend_upper = x + dx_upper,
        yend_upper = y + dy_upper
      )

    # ----------------------------
    # 7. Final Data Selection
    # ----------------------------

    # Select relevant columns to return
    result <- grid %>%
      select(
        x, y,
        dx, dy,
        xend, yend,
        dx_lower, dy_lower,
        xend_lower, yend_lower,
        dx_upper, dy_upper,
        xend_upper, yend_upper
      ) %>%
      mutate(
        r = sqrt(dx^2 + dy^2),         # Length of main vectors
        r_lower = sqrt(dx_lower^2 + dy_lower^2),  # Length of lower vectors
        r_upper = sqrt(dx_upper^2 + dy_upper^2)   # Length of upper vectors
      )

    return(result)
  }

)




#' @rdname geom_vector_smooth
#' @export
# Define the GeomVectorSmooth ggproto
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
    arrow = NULL, se = TRUE, se.circle = FALSE,
    scale_factor, eval_points
  ) {
    grobs <- list()
    # print(data)

    if (se) {
      # Draw wedges for confidence intervals using GeomPolygon

      wedge_data <- do.call(rbind, lapply(1:nrow(data), function(i) {
        create_wedge_data(
          x = data$x[i], y = data$y[i],
          xend_upper = data$xend_upper[i], yend_upper = data$yend_upper[i],
          xend_lower = data$xend_lower[i], yend_lower = data$yend_lower[i],
          xend = data$xend[i], yend = data$yend[i],
          id = data$id[i],
          n_points = 50,
          radius = data$r[i]  # Ensure 'r' is equal to base_radius
        )
      }))

      # Assign aesthetics for the wedge
      wedge_data <- wedge_data %>%
        mutate(
          linewidth = 0.5,          # Adjust as needed
          alpha = 1,              # Adjust transparency
          # alpha = 0.3,              # Adjust transparency
          fill = "green",          # Fill color for the wedge
          # fill = "grey60",          # Fill color for the wedge
          colour = NA               # No border color
        )

      # Draw the wedges
      wedge_grob <- GeomPolygon$draw_panel(
        wedge_data,
        panel_params = panel_params,
        coord = coord
      )

      grobs <- c(grobs, grid::gList(wedge_grob))
    }

    if (se.circle) {
      # Draw circles around each vector using GeomPolygon
      circle_data <- do.call(rbind, lapply(1:nrow(data), function(i) {
        create_circle_data(
          x = data$x[i], y = data$y[i],
          radius = data$r[i],
          n = 100,
          group = data$id[i]
        )
      }))

      # Assign aesthetics for the circles
      circle_data <- circle_data %>%
        mutate(
          linewidth = 0.5,          # Adjust as needed
          alpha = 0.2,              # Adjust transparency
          fill = NA,                # No fill for circles
          colour = "grey40"         # Border color for circles
        )

      # Draw the circles
      circle_grob <- GeomPolygon$draw_panel(
        circle_data,
        panel_params = panel_params,
        coord = coord
      )

      grobs <- c(grobs, grid::gList(circle_grob))
    }

    # Draw the main vectors
    # print(data)
    segments_grob <- GeomSegment$draw_panel(
      data, panel_params, coord, arrow = arrow
    )
    grobs <- c(grobs, grid::gList(segments_grob))

    # Combine all grobs into a single grobTree
    combined_grob <- grid::grobTree(children = do.call(grid::gList, grobs))

    return(combined_grob)
  },

  draw_key = draw_key_smooth  # Ensure this function is defined
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
