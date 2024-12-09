#' Create a Smooth Vector Plot Layer
#'
#' `geom_vector_smooth` generates a ggplot layer that visualizes a smooth vector
#' field. It uses raw vector data and applies smoothing techniques to estimate
#' the underlying vector field. This functionality is similar to `geom_smooth()`
#' in ggplot2, but is designed specifically for vector data rather than scalar data.
#'
#' @inheritParams geom_vector
#' @inheritParams ggplot2::stat_identity
#' @param n An integer vector specifying the number of grid points along each
#'   axis for smoothing.
#' @param method Character; specifies the smoothing method to use. The only
#'   supported method is `"lm"`, which fits a multivariate linear model to predict
#'   the vector displacements `dx` and `dy` based on the coordinates `x` and `y`.
#' @param se Logical; if `TRUE`, confidence intervals are plotted around the
#'   smoothed vectors.
#' @param se.circle Logical; if `TRUE`, circles are drawn around the origin of
#'   the vectors to represent the radius of the confidence interval. This feature
#'   is useful when `se = TRUE`.
#' @param conf_level Numeric vector; specifies the confidence levels for the
#'   prediction intervals when `se = TRUE`. **Default is `conf_level = 0.95`**.
#' @param eval_points Data frame of evaluation points, or `NULL`. When provided,
#'   it specifies the grid points where the smoothing model is evaluated. If
#'   `NULL`, the function generates a grid based on `n`.
#' @param pi_type Character; determines how prediction intervals are displayed
#'   around the smoothed vectors. Two options are available:
#'   - `"wedge"`: Displays angular wedges that indicate uncertainty in both the
#'     direction and magnitude of the vectors. Wedges show the range of possible
#'     vector orientations and lengths.
#'   - `"ellipse"`: Uses ellipses to depict prediction intervals, reflecting
#'     the covariance between the vector components (`dx` and `dy`). Ellipses
#'     provide a visual representation of joint uncertainty in vector directions.
#'   The default is `"wedge"`. If `pi_type` is set to `"ellipse"` and `eval_points`
#'   is `NULL`, the function switches `pi_type` to `"wedge"` to ensure proper
#'   interval representation.
#' @param arrow Arrow specification created by `grid::arrow()`. This parameter
#'   controls the appearance of arrowheads at the ends of vectors, including
#'   angle, length, and type.
#' @param formula A formula specifying the multivariate linear model used
#'   for smoothing. The default formula is `cbind(dx, dy) ~ x * y`.
#' @return A `ggplot2` layer that can be added to a ggplot object to create a
#'   smooth vector field plot.
#' @importFrom stats qt
#' @importFrom stats integrate
#' @name geom_vector_smooth
#' @rdname geom_vector_smooth
#'
#' @section Aesthetics:
#' `geom_vector_smooth` supports the following aesthetics (required aesthetics are in bold):
#'
#' - **`x`**: The x-coordinate of the vector's starting point.
#' - **`y`**: The y-coordinate of the vector's starting point.
#' - **`dx`**: The vector's displacement along the x-axis.
#' - **`dy`**: The vector's displacement along the y-axis.
#' - `color`: The color of the vector line.
#' - `linewidth`: The thickness of the vector line.
#' - `linetype`: The type of the vector line (e.g., solid or dashed).
#' - `alpha`: The transparency level of the vector.
#' - `arrow`: Specifies arrowheads for the vectors.
#'
#' @details
#' **Multivariate Linear Model**:
#'
#' The `"lm"` method fits a multivariate linear model to predict vector displacements
#' `dx` and `dy` based on the input coordinates `x` and `y`. This model includes
#' interaction terms (`x * y`) to capture more complex relationships in the vector field.
#'
#' **Prediction Intervals**:
#'
#' Two types of prediction intervals are supported:
#'
#' - **Ellipse**: Ellipses are used to represent the covariance of predicted `dx`
#'   and `dy` values. The size and orientation of the ellipses illustrate both
#'   the uncertainty in vector magnitude and the correlation between vector components.
#' - **Wedge**: Wedges are angular sectors that indicate the range of possible
#'   directions and lengths for the vectors. This type of prediction interval
#'   provides an intuitive visualization of uncertainty in vector orientation.
#'
#' The intervals are computed using the confidence level specified by the `conf_level`
#' parameter.
#'
#' @examples
#' # Function to generate vectors
#' generate_vectors <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(
#'     sin(x) + sin(y) + rnorm(1, 5, 1),
#'     sin(x) - sin(y) - rnorm(1, 5, 1)
#'   )
#' }
#'
#' # Set seed for reproducibility
#' set.seed(123)
#'
#' # Create sample points and compute vectors
#' sample_points <- data.frame(
#'   x = runif(30, 0, 10),
#'   y = runif(30, 0, 10)
#' )
#'
#' result <- t(apply(sample_points, 1, generate_vectors))
#'
#' sample_points$xend <- result[, 1]
#' sample_points$yend <- result[, 2]
#' sample_points$dx <- sample_points$xend - sample_points$x
#' sample_points$dy <- sample_points$yend - sample_points$y
#' sample_points$distance <- sqrt(sample_points$dx^2 + sample_points$dy^2)
#' sample_points$angle <- atan2(sample_points$dy, sample_points$dx)
#'
#' # Define evaluation points
#' eval_points <- data.frame(
#'   x = c(0, 7.5),
#'   y = c(10, 5)
#' )
#'
#' # Example 1:
#' ggplot(sample_points, aes(x = x, y = y)) +
#'   geom_vector(aes(dx = dx, dy = dy, color = NULL), center = FALSE, alpha = 0.2) +
#'   geom_vector_smooth(aes(dx = dx, dy = dy), n = 5) +
#'   ggtitle("Smoothed Vector Field")
#'
#' # Example 2: Ellipse with eval_points
#' ggplot(sample_points, aes(x = x, y = y)) +
#'   geom_vector(aes(dx = dx, dy = dy, color = NULL), center = FALSE, alpha = 0.2) +
#'   geom_vector_smooth(aes(dx = dx, dy = dy), eval_points = eval_points, conf_level = c(0.9)) +
#'   ggtitle("Smoothed Vector Field with Ellipse Intervals")
#'
#' # Example 3: Wedge with eval_points
#' ggplot(sample_points, aes(x = x, y = y)) +
#'   geom_vector(aes(dx = dx, dy = dy, color = NULL), center = FALSE, alpha = 0.2) +
#'   geom_vector_smooth(aes(dx = dx, dy = dy), eval_points = eval_points, pi_type = "ellipse") +
#'   ggtitle("Smoothed Vector Field with Wedge Intervals")
#'
NULL

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
    pi_type = "ellipse",
    formula = cbind(dx, dy) ~ x * y,
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
    pi_type = "ellipse",
    conf_level = c(.95, NA),
    formula = cbind(dx, dy) ~ x * y,
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
      formula = formula,
      na.rm = na.rm,
      ...
    )
  )
}

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

    # Create grid for evaluation
    if (!is.null(eval_points)) {
      # Validate eval_points contains necessary columns
      if (!all(c("x", "y") %in% names(eval_points))) {
        stop("The 'eval_points' argument must contain 'x' and 'y' columns.")
      }

      grid <- eval_points

      # Compute base_radius based on data range
      data_range <- min(diff(range(data$x)), diff(range(data$y)))
      base_radius <- data_range / 2.5
    } else {
      # Generate grid when eval_points is not provided
      x_seq <- seq(min(data$x), max(data$x), length.out = n[1])
      y_seq <- seq(min(data$y), max(data$y), length.out = n[2])
      grid <- expand.grid(x = x_seq, y = y_seq)

      # Calculate grid spacing and base radius
      x_spacing <- diff(sort(unique(grid$x)))[1]
      y_spacing <- diff(sort(unique(grid$y)))[1]
      base_radius <- min(x_spacing, y_spacing) / 2.5
    }

    grid$id <- seq_len(nrow(grid))


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

    if (pi_type == "ellipse" || pi_type == "wedge") {
      # Compute prediction intervals based on pi_type
      if (pi_type == "ellipse") {
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
      }

      if (pi_type == "wedge") {
        wedge_angles <- do.call(rbind, mapply(
          predict_theta_interval,
          x = grid$x,
          y = grid$y,
          mux = grid$dx,
          muy = grid$dy,
          MoreArgs = list(Sigma = Sigma, rho = rho, conf_level = conf_level[1]),
          SIMPLIFY = FALSE
        ))
        grid <- cbind(grid, wedge_angles)
        grid$r_upper <- sqrt(grid$dx^2 + grid$dy^2)
        grid$r_lower <- 0

        # Adjust scale if eval_points is NULL
        if (is.null(eval_points)) {
          current_magnitudes <- sqrt(grid$dx^2 + grid$dy^2)
          current_magnitudes[current_magnitudes == 0] <- 1  # Avoid division by zero
          scaling_factors <- base_radius / current_magnitudes
          grid$dx <- grid$dx * scaling_factors
          grid$dy <- grid$dy * scaling_factors
          grid$xend <- grid$x + grid$dx
          grid$yend <- grid$y + grid$dy
          grid$r_upper <- base_radius
        }

        # Compute prediction endpoints
        prediction_results <- mapply(
          compute_prediction_endpoints,
          x = grid$x,
          y = grid$y,
          dx = grid$dx,
          dy = grid$dy,
          angle_lower = grid$min_angle,
          angle_upper = grid$max_angle,
          SIMPLIFY = FALSE
        )
        prediction_df <- do.call(rbind, prediction_results)
        grid <- cbind(grid, prediction_df)
      }

      # Finalize result
      result <- grid
      result$xend <- result$x + result$dx
      result$yend <- result$y + result$dy
    } else {
      stop("Invalid value for pi_type. Must be 'wedge' or 'ellipse'.")
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
