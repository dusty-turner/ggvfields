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
  # required_aes = c("x", "y"),
  required_aes = c("x", "y"),  # Add angle/distance support
  dropped_aes = c("distance", "angle"),
  default_aes = aes(color = "#3366FF", linewidth = 0.5, linetype = 1, alpha = 1,
                    angle = NA, distance = NA, dx = NA, dy = NA),

  setup_data = function(data, params) {

    # print(data |> head())

    if (!all(is.na(data$dx)) && !all(is.na(data$angle))) {
      warning("Both Cartesian and Polar inputs provided. Using Cartesian by default.")
    }

    # Ensure angle and distance are retained in the transformed data
    if (!("dx" %in% names(data) && "dy" %in% names(data))) {
      if ("angle" %in% names(data) && "distance" %in% names(data)) {
        data$dx <- data$distance * cos(data$angle)
        data$dy <- data$distance * sin(data$angle)
      } else {
        stop("Either 'dx' and 'dy' or 'angle' and 'distance' must be provided.")
      }

    }

    # Retain all original aesthetics to prevent warnings
    required_aes <- c("x", "y", "angle", "distance", "dx", "dy")
    extra_aes <- setdiff(names(data), required_aes)
    data[extra_aes] <- lapply(extra_aes, function(a) data[[a]])

    # print(data |> head())

    return(data)
  },

  compute_group = function(data, scales, n, center, method, normalize = TRUE, scale_factor, se = TRUE, probs,
                           eval_points = NULL, formula, ...) {

    # print(eval_points)

    ## if eval points exist, then create the grid this way
    if (!is.null(eval_points)) {
      if (!all(c("x", "y") %in% names(eval_points))) {
        stop("The 'eval_points' argument must contain 'x' and 'y' columns.")
      }

      grid <- eval_points

      if (nrow(grid) > 1) {
        # Find the minimum Euclidean distance between points
        min_distance <- euclidean_distances(grid)
        # Choose radius as a fraction of the minimum distance
        base_radius <- min_distance / 2.5
      } else {
        # Use a fallback radius derived from the data range or ggplot's scales
        data_range_x <- diff(range(data$x))
        data_range_y <- diff(range(data$y))
        data_range <- min(data_range_x, data_range_y)
        base_radius <- data_range / 2.5  # A heuristic to derive a meaningful radius
      }

    } else { # Generate a regular grid if eval_points is not provided
      x_seq <- seq(min(data$x), max(data$x), length.out = n[1])
      y_seq <- seq(min(data$y), max(data$y), length.out = n[2])
      grid <- expand.grid(x = x_seq, y = y_seq)
      x_spacing <- diff(sort(unique(grid$x)))[1]
      y_spacing <- diff(sort(unique(grid$y)))[1]
      # Choose the radius as a fraction of the smaller spacing
      base_radius <- min(x_spacing, y_spacing) / 2.5

    }

    grid$id <- 1:nrow(grid)

    # print(grid)

    # Ensure probs is a vector, even if a single value is provided
    if (length(probs) == 1) {
      probs <- c(probs, NA)
    }

    # Ensure that n has the correct length
    n <- ensure_length_two(n)

    # Calculate xend and yend using dx and dy
    data$xend <- data$x + data$dx
    data$yend <- data$y + data$dy

    # Calculate angle and distance using dx and dy
    data$distance <- sqrt(data$dx^2 + data$dy^2)
    data$angle <- atan2(data$dy, data$dx)

    if (method == "lm") {

      ## for angle
      message(as.character(formula))

      model <- lm(formula = formula, data = data)

      # print(model)

      # Extract the covariance matrix of the coefficients
      V <- vcov(model)

      rhs_formula <- as.formula(paste("~", paste(all.vars(formula[[3]]), collapse = "+")))

      # Create the model matrix using only the independent variables

      X <- model.matrix(rhs_formula, grid)

      # Initialize matrices to store prediction variances for each response
      n_preds <- ncol(model$coefficients)  # Number of responses (dx, dy)
      pred_var <- matrix(NA, nrow = nrow(X), ncol = n_preds)


      # Compute prediction variances for each response
      for (i in 1:n_preds) {
        idx <- ((i - 1) * ncol(X) + 1):(i * ncol(X))
        V_i <- V[idx, idx]
        pred_var[, i] <- diag(X %*% V_i %*% t(X))
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
        interval_data[[interval_type]] <- cbind(
          setNames(data.frame(lwr), paste0(c("xend", "yend"), "_lower_", interval_type)),
          setNames(data.frame(upr), paste0(c("xend", "yend"), "_upper_", interval_type))
        )

        ## for distance
        # Distance prediction model
        fit_distance <- lm(sqrt(dx^2 + dy^2) ~ x + y, data = data)
        distance_pred <- predict(fit_distance, newdata = grid, interval = "prediction", level = 0.75)

        # Extract the 25% and 75% prediction intervals
        grid$distance_pred <- distance_pred[, "fit"]
        grid$distance_lower_25 <- distance_pred[, "lwr"]
        grid$distance_upper_75 <- distance_pred[, "upr"]

        # Use predicted distance and angle to calculate fit_dx and fit_dy
        grid$est_dx <- grid$x + grid$distance_pred * cos(preds[, 1])
        grid$est_dy <- grid$y + grid$distance_pred * sin(preds[, 1])

        grid$radius <- sqrt((grid$est_dx)^2 + (grid$est_dy)^2)

        # Optionally include the lower and upper bounds for plotting
        grid$est_dx_lower_25 <- grid$x + grid$distance_lower_25 * cos(preds[, 1])
        grid$est_dy_lower_25 <- grid$y + grid$distance_lower_25 * sin(preds[, 1])
        grid$est_dx_upper_75 <- grid$x + grid$distance_upper_75 * cos(preds[, 1])
        grid$est_dy_upper_75 <- grid$y + grid$distance_upper_75 * sin(preds[, 1])

      }

      # Combine all interval data with the main grid and predictions
      grid <- cbind(
        grid,
        setNames(data.frame(preds), paste0("fit_", c("dx", "dy"))),
        do.call(cbind, unname(interval_data))
      )
    } else if (method == "boot") {
      # Perform bootstrapping to predict angles
      boot_results <- boot::boot(data, function(d, i) {
        fit_sin <- lm(sin(atan2(d$dy, d$dx)) ~ x + y, data = d[i, ])
        fit_cos <- lm(cos(atan2(d$dy, d$dx)) ~ x + y, data = d[i, ])

        pred_sin <- predict(fit_sin, newdata = grid)
        pred_cos <- predict(fit_cos, newdata = grid)

        return(atan2(pred_sin, pred_cos))
      }, R = 500)

      # Extract predictions and use the median as the main angle prediction
      angle_preds <- boot_results$t
      grid$angle_pred <- apply(angle_preds, 2, median)

      # If SE enabled, calculate outer and inner bounds for prediction intervals
      if (se) {
        grid$angle_lower_outer <- apply(angle_preds, 2, function(x) quantile(x, probs = (1 - probs[1]) / 2))
        grid$angle_upper_outer <- apply(angle_preds, 2, function(x) quantile(x, probs = 1 - (1 - probs[1]) / 2))

        if (!is.na(probs[2])) {
          grid$angle_lower_inner <- apply(angle_preds, 2, function(x) quantile(x, probs = (1 - probs[2]) / 2))
          grid$angle_upper_inner <- apply(angle_preds, 2, function(x) quantile(x, probs = 1 - (1 - probs[2]) / 2))
        } else {
          # Fill inner bounds with NA if probs[2] is not provided
          grid$angle_lower_inner <- rep(NA, nrow(grid))
          grid$angle_upper_inner <- rep(NA, nrow(grid))
        }
      }

      # Predict distances
      fit_distance <- lm(sqrt(dx^2 + dy^2) ~ x + y, data = data)
      pred_distance <- predict(fit_distance, newdata = grid, se.fit = se)
      grid$distance_pred <- pred_distance$fit

      # Calculate xend and yend using the predicted angles and distances
      grid$fit_dx <- grid$x + grid$distance_pred * cos(grid$angle_pred)
      grid$fit_dy <- grid$y + grid$distance_pred * sin(grid$angle_pred)

      if (se) {
        # Calculate outer bounds for xend and yend
        grid$xend_upper_outer <- grid$x + grid$distance_pred * cos(grid$angle_upper_outer)
        grid$yend_upper_outer <- grid$y + grid$distance_pred * sin(grid$angle_upper_outer)
        grid$xend_lower_outer <- grid$x + grid$distance_pred * cos(grid$angle_lower_outer)
        grid$yend_lower_outer <- grid$y + grid$distance_pred * sin(grid$angle_lower_outer)

        # Calculate inner bounds or fill with NA for xend and yend
        if (!is.na(probs[2])) {
          grid[, c("xend_upper_inner", "yend_upper_inner",
                   "xend_lower_inner", "yend_lower_inner")] <- list(
                     grid$x + grid$distance_pred * cos(grid$angle_upper_inner),
                     grid$y + grid$distance_pred * sin(grid$angle_upper_inner),
                     grid$x + grid$distance_pred * cos(grid$angle_lower_inner),
                     grid$y + grid$distance_pred * sin(grid$angle_lower_inner)
                   )
        } else {
          grid[, c("xend_upper_inner", "yend_upper_inner",
                   "xend_lower_inner", "yend_lower_inner")] <- NA
        }
      }
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
      dx = grid$fit_dx - grid$x,
      dy = grid$fit_dy - grid$y
    )

    # If confidence intervals are enabled, include them in the result
    if (se) {
      ci_cols <- grep("end_", colnames(grid), value = TRUE)
      result <- cbind(result, grid[, ci_cols])
    }

    if(is.null(eval_points)) {
      result$radius <- rep(base_radius, nrow(result))
    } else {
      result$radius <- grid$radius
    }

    # print(result)

    return(result)
  }
)



#' @rdname geom_vector_smooth
#' @export
GeomVectorSmooth <- ggproto(
  "GeomVectorSmooth",
  GeomSegment,
  required_aes = c("x", "y"),  # Add angle/distance support
  dropped_aes = c("distance", "angle"),
  optional_aes = c("x", "y", "dx", "dy"),
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
        wedge$alpha <- 0.9
        wedge$fill <- "grey60"
        # wedge$fill <- data$fill[i]
        wedge$colour <- NA
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
          wedge$alpha <- 0.4
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
      }
    }

    data$distance <- sqrt(data$dx^2 + data$dy^2)  # Calculate vector magnitudes

    # Normalize dx and dy to unit length
    data$dx_norm <- data$dx / data$distance
    data$dy_norm <- data$dy / data$distance

    # Scale dx and dy to match the desired radius
    data$dx <- data$dx_norm * data$radius
    data$dy <- data$dy_norm * data$radius

    # Calculate new xend and yend based on the scaled dx and dy
    data$xend <- data$x + data$dx
    data$yend <- data$y + data$dy

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
      center = center,
      normalize = normalize,
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
  center = TRUE, normalize = TRUE,
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
      center = center,
      normalize = normalize,
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
