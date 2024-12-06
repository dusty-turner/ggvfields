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
#' @param method Character; specifies the smoothing method to be used.
#'   Supported methods are `"lm"` (linear modeling) and `"boot"` (bootstrapping).
#'   `"boot"` generates smoother results by calculating angles with bootstrapping,
#'   and the prediction intervals are determined using quantiles.
#' @param se Logical; if `TRUE`, plots the confidence intervals around the
#'   smoothed vectors.
#' @param se.circle Logical; if `TRUE`, draws circles around the origin of the
#'   vectors to represent the radius of the confidence interval. This is useful
#'   for visualizing variability when `se = TRUE`.
#' @param conf_level Numeric vector; specifies the prediction interval levels to be
#'   plotted when `se = TRUE`. **Default is `conf_level = 0.95`**.
#' @param eval_points Number of points at which the function is evaluated for
#'   smoothing.
#' @param pi_type Character; specifies the type of prediction interval to
#'   display around the smoothed vectors.
#'   - `"wedge"`: Represents prediction intervals as angular wedges, illustrating
#'   uncertainty in both the direction and magnitude of the vectors. This method
#'   visualizes variability by displaying a sector emanating from the vector's
#'   origin, capturing the range of possible vector directions and lengths.
#'   - `"ellipse"`: Depicts prediction intervals as ellipses surrounding the
#'   vectors, reflecting the covariance between the vector components (`dx` and
#'   `dy`). This approach provides a clear representation of the joint uncertainty
#'   in both directions, allowing for the visualization of correlated variability.
#'   **Default is `"wedge"`**. Note that if `pi_type` is set to `"ellipse"` but
#'   `eval_points` is `NULL`, the function will automatically switch `pi_type` to
#'   `"wedge"` to ensure appropriate interval representation.
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
#'   geom_vector(aes(dx = dx, dy = dy)) +
#'   ggtitle("Vector Smoothing with Linear Model (lm)")
#'
#' # Example 2: Polar Coordinates with Bootstrapping (boot)
#' ggplot(sample_points, aes(x = x, y = y, angle = angle, distance = distance)) +
#'   geom_vector_smooth(method = "boot", se = TRUE, conf_level = c(0.95, 0.68)) +
#'   geom_vector(aes(dx = dx, dy = dy)) +
#'   ggtitle("Vector Smoothing with Bootstrapping (boot)")
#'
#' # Example 3: Using Polar Coordinates with coord_polar
#' ggplot(sample_points, aes(x = x, y = y, angle = angle, distance = distance)) +
#'   geom_vector_smooth(method = "lm", se = TRUE, n = 4) +
#'   geom_vector(aes(dx = dx, dy = dy)) +
#'   ggtitle("Vector Smoothing in Polar Coordinates")
#'
NULL


#' @rdname geom_vector_smooth
#' @export
StatVectorSmooth <- ggproto(
  "StatVectorSmooth",
  Stat,
  required_aes = c("x", "y"),
  dropped_aes = c("distance", "angle"),
  default_aes = aes(
    color = "#3366FF", linewidth = 0.5, linetype = 1, alpha = 1,
    angle = NA, distance = NA, dx = NA, dy = NA
  ),

  compute_group = function(
    data, scales, n,
    method,
    se = TRUE, conf_level, pi_type,
    eval_points = NULL, formula, ...
  ) {

    # ----------------------------
    # 1. Initial Data Checks and Manipulation
    # ----------------------------
    if (pi_type == "ellipse" && is.null(eval_points)) {
      message("eval_points is NULL; changing pi_type from 'ellipse' to 'wedge'.")
      pi_type <- "wedge"
    }
    # Use helper function to validate input
    validation_result <- validate_aesthetics(data)

    # If 'angle' and 'distance' are provided, compute 'dx' and 'dy'
    if (!all(is.na(data$angle)) && !all(is.na(data$distance))) {
      data$dx <- data$distance * cos(data$angle)
      data$dy <- data$distance * sin(data$angle)
    } else if (all(!is.na(data$dx)) && all(!is.na(data$dy))) {
      # If 'dx' and 'dy' are provided, compute 'angle' and 'distance'
      data$distance <- sqrt(data$dx^2 + data$dy^2)
      data$angle <- atan2(data$dy, data$dx)
    }

    # Ensure 'n' is a numeric vector of length 2
    n <- ensure_length_two(n)

    alpha_levels <- 1 - conf_level

    # Two-tailed interval
    lower_probs <- alpha_levels / 2
    upper_probs <- 1 - (alpha_levels / 2)
    # Create grid for evaluation
    if (!is.null(eval_points)) {
      if (!all(c("x", "y") %in% names(eval_points))) {
        stop("The 'eval_points' argument must contain 'x' and 'y' columns.")
      }
      grid <- eval_points

      if (nrow(grid) > 1) {
        # min_distance <- euclidean_distances(grid)
        # base_radius <- min_distance / 2.5
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
    n_resp <- length(all.vars(formula[[2]]))

    # Number of coefficients per response
    coeffs_per_response <- length(coef(model_mv)[,1])

    # Check the number of coefficients
    total_coeffs <- ncol(cov_coef)

    # Extract residuals from the model
    residuals_mv <- resid(model_mv)

    # Compute covariance matrix of residuals
    Sigma <- cov(residuals_mv)

    # Extract correlation coefficient
    sigma_x <- sqrt(Sigma["dx", "dx"])
    sigma_y <- sqrt(Sigma["dy", "dy"])
    rho <- Sigma["dx", "dy"] / (sigma_x * sigma_y)

    expected_coeffs <- coeffs_per_response * n_resp
    if (total_coeffs != expected_coeffs) {
      stop("Unexpected number of coefficients in the model. Please verify the formula and data.")
    }

    # Extract covariance matrices
    cov_beta_dx <- cov_coef[1:coeffs_per_response, 1:coeffs_per_response]     # Covariance for dx coefficients
    cov_beta_dy <- cov_coef[(coeffs_per_response+1):(expected_coeffs), (coeffs_per_response+1):(expected_coeffs)]     # Covariance for dy coefficients
    cov_beta_dx_dy <- cov_coef[1:coeffs_per_response, (coeffs_per_response+1):(expected_coeffs)]  # Covariance between dx and dy coefficients

    # Compute variance for dx and dy predictions
    var_dx <- rowSums(design_matrix * (design_matrix %*% cov_beta_dx))
    var_dy <- rowSums(design_matrix * (design_matrix %*% cov_beta_dy))
    cov_dx_dy <- rowSums(design_matrix * (design_matrix %*% cov_beta_dx_dy))

    # Add residual variances and covariance
    var_dx <- var_dx + Sigma["dx", "dx"]
    var_dy <- var_dy + Sigma["dy", "dy"]
    cov_dx_dy <- cov_dx_dy + Sigma["dx", "dy"]

    # Assemble the covariance matrix for (dx, dy) predictions
    cov_pred <- data.frame(var_dx, var_dy, cov_dx_dy)

    # ----------------------------
    # 4. Compute Prediction Intervals
    # ----------------------------

    if (pi_type == "ellipse" | pi_type == "wedge") {
      # Compute ellipse parameters for each grid point
      ellipse_params_list <- mapply(
        compute_ellipse_params,
        var_dx = cov_pred$var_dx,
        var_dy = cov_pred$var_dy,
        cov_dx_dy = cov_pred$cov_dx_dy,
        MoreArgs = list(conf_level = conf_level[1]),
        SIMPLIFY = FALSE
      )
      # Extract ellipse parameters and add to grid
      grid$ellipse_width <- sapply(ellipse_params_list, `[[`, "width")
      grid$ellipse_height <- sapply(ellipse_params_list, `[[`, "height")
      grid$ellipse_angle <- sapply(ellipse_params_list, `[[`, "angle")

      # Compute mean dx, dy, xend, yend
      grid$xend <- grid$x + grid$dx
      grid$yend <- grid$y + grid$dy

      # Result data frame
      result <- data.frame(
        x = grid$x,
        y = grid$y,
        dx = grid$dx,
        dy = grid$dy,
        xend = grid$xend,
        yend = grid$yend,
        ellipse_width = grid$ellipse_width,
        ellipse_height = grid$ellipse_height,
        ellipse_angle = grid$ellipse_angle,
        id = grid$id
      )

  } else {
    stop("Invalid value for pi_type. Must be 'wedge' or 'ellipse'.")
  }

    if(pi_type == "wedge"){

      wedge_angles <- do.call(rbind, mapply(
        predict_theta_interval,
        x = grid$x,
        y = grid$y,
        mux = grid$dx,
        muy = grid$dy,
        MoreArgs = list(Sigma = Sigma, rho = rho),
        SIMPLIFY = FALSE
      ))

      # Combine with original dataframe
      result <- cbind(result, wedge_angles)

      result$r_upper <- sqrt((result$dx)^2 + (result$dy)^2)
      result$r_lower <- 0

      if (is.null(eval_points)) { ## rescales circle and arrow for grid shapes
        current_magnitudes <- sqrt(result$dx^2 + result$dy^2)

        # Avoid division by zero by setting zero magnitudes to one (vectors with no length)
        current_magnitudes[current_magnitudes == 0] <- 1

        # Calculate scaling factors to adjust magnitudes to base_radius
        scaling_factors <- base_radius / current_magnitudes

        # Scale dx and dy to have length base_radius while preserving direction
        result$dx <- result$dx * scaling_factors
        result$dy <- result$dy * scaling_factors

        # Recompute xend and yend based on scaled dx and dy
        result$xend <- result$x + result$dx
        result$yend <- result$y + result$dy

        result$r_upper <- base_radius
      }


      # Apply the function to each row using mapply
      prediction_results <- mapply(
        compute_prediction_endpoints,
        x = result$x,
        y = result$y,
        dx = result$dx,
        dy = result$dy,
        angle_lower = result$min_angle,
        angle_upper = result$max_angle,
        SIMPLIFY = FALSE
      )

      # Combine the list of dataframes into one dataframe
      prediction_df <- do.call(rbind, prediction_results)

      # View the prediction results

      result <- cbind(result, prediction_df)
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
  default_aes = aes(
    linewidth = 0.5, linetype = 1, alpha = 1,
    fill = NULL, color = "#3366FF"
  ),

  setup_data = function(data, params) {
    data$id <- seq_len(nrow(data))
    return(data)
  },

  draw_panel = function(
    data, panel_params, coord,
    arrow = NULL, se = TRUE, se.circle = FALSE,
    eval_points, pi_type
  ) {
    grobs <- list()

    if (pi_type == "ellipse" && is.null(eval_points)) {
      # message("eval_points is NULL; changing pi_type from 'ellipse' to 'wedge'.")
      pi_type <- "wedge"
    }

    if (se) {
      if (pi_type == "wedge") {

        # Initialize a list to store all wedge polygons
        wedge_polygons <- vector("list", nrow(data))

        for (i in 1:nrow(data)) {

          wedge_polygons[[i]] <- create_wedge_data(
            x = data$x[i], y = data$y[i],
            xend_upper = data$xend_upper[i], yend_upper = data$yend_upper[i],
            xend_lower = data$xend_lower[i], yend_lower = data$yend_lower[i],
            xend = data$xend[i], yend = data$yend[i],
            id = data$id[i],
            n_points = 50,
            outer_radius = data$r_lower[i],
            inner_radius = data$r_upper[i]
          )
        }

        # Combine all wedge data into a single data frame
        wedge_data <- do.call(rbind, wedge_polygons)

        # Assign aesthetics for the wedge
        wedge_data$linewidth <- 0.5
        wedge_data$alpha <- .4
        wedge_data$fill <- "grey60"
        wedge_data$colour <- NA

        # Draw the wedges using GeomPolygon
        wedge_grob <- GeomPolygon$draw_panel(
          wedge_data,
          panel_params = panel_params,
          coord = coord
        )

        grobs <- c(grobs, grid::gList(wedge_grob))

      } else if (pi_type == "ellipse") {
        # Draw ellipses
        ellipse_data_list <- lapply(1:nrow(data), function(i) {
          ellipse <- create_ellipse_data(
            x_center = data$xend[i],
            y_center = data$yend[i],
            width = data$ellipse_width[i],
            height = data$ellipse_height[i],
            angle = data$ellipse_angle[i],
            n_points = 100
          )
          ellipse$group <- data$id[i]
          ellipse
        })

        # Combine ellipse data
        ellipse_data <- do.call(rbind, ellipse_data_list)

        # Assign aesthetics
        ellipse_data$linewidth <- 0.5
        ellipse_data$alpha <- 0.4
        ellipse_data$fill <- "grey60"
        ellipse_data$colour <- NA

        # Draw the ellipses using GeomPolygon
        ellipse_grob <- GeomPolygon$draw_panel(
          ellipse_data,
          panel_params = panel_params,
          coord = coord
        )

        grobs <- c(grobs, grid::gList(ellipse_grob))
      } else {
        stop("Invalid value for pi_type. Must be 'wedge' or 'ellipse'.")
      }
    }

    if (se.circle) {
      # Initialize a list to store all circle polygons
      circle_polygons <- vector("list", nrow(data))

      for (i in 1:nrow(data)) {
        circle_polygons[[i]] <- create_circle_data(
          x = data$x[i], y = data$y[i],
          radius = sqrt(data$dx[i]^2 + data$dy[i]^2),
          n = 100,
          group = data$id[i]
        )
      }

      # Combine all circle data into a single data frame
      circle_data <- do.call(rbind, circle_polygons)

      # Assign aesthetics for the circles
      circle_data$linewidth <- 0.5
      circle_data$alpha <- 0.2
      circle_data$fill <- NA
      circle_data$colour <- "grey60"

      # Draw the circles using GeomPolygon
      circle_grob <- GeomPolygon$draw_panel(
        circle_data,
        panel_params = panel_params,
        coord = coord
      )

      grobs <- c(grobs, grid::gList(circle_grob))
    }

    # Draw the main vectors using GeomSegment
    segments_grob <- GeomSegment$draw_panel(
      data, panel_params, coord, arrow = arrow
    )
    grobs <- c(grobs, grid::gList(segments_grob))

    # Combine all grobs into a single grobTree
    combined_grob <- grid::grobTree(children = do.call(grid::gList, grobs))

    return(combined_grob)
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
    method = "lm",
    se = TRUE,
    se.circle = TRUE,
    conf_level = c(.95, NA),
    pi_type = "wedge",
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
      method = method,
      se = se,
      se.circle = se.circle,
      pi_type = pi_type,
      conf_level = conf_level,
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
    n = c(11, 11),
    method = "lm",
    se = TRUE,
    se.circle = TRUE,
    pi_type = "wedge",
    conf_level = c(.95, NA),
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
      method = method,
      se = se,
      se.circle = se.circle,
      pi_type = pi_type,
      conf_level = conf_level,
      arrow = arrow,
      eval_points = eval_points,
      formula = default_formula,
      na.rm = na.rm,
      ...
    )
  )
}
