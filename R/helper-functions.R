#' @importFrom stats lm median predict quantile qnorm
# List of non-expored and non-documented functions

ensure_nonempty_data <- function(data) {
  if (empty(data)) {
    tibble0(group = 1, .size = 1)
  }
  else {
    data
  }
}

ensure_length_two <- function(n) {
  if (length(n) == 1) n <- rep(n, 2)
  if (length(n) != 2) stop("Length of 'n' must be 2")
  return(n)
}

times <- `*`

rad2deg <- function(rad, rotate = 0) {
  ((rad + rotate) * 360/(2*pi)) %% 360
}


tibble0 <- function(...) {
  tibble::tibble(..., .name_repair = "minimal")
}

empty <- function(df) {
  is.null(df) || nrow(df) == 0 || ncol(df) == 0 || inherits(df, "waiver")
}

# Utility function to replace %||%
`%||%` <- function(a, b) if (!is.null(a)) a else b

norm <- function(u) sqrt(sum(u^2))

normalize <- function(u) u / norm(u)

## -----------geom_vector_smooth--------------------
validate_aesthetics <- function(data) {
  # Helper function to check if all specified columns exist and are non-NA
  columns_valid <- function(df, cols) {
    all(cols %in% names(df)) && all(!is.na(df[[cols[1]]])) && all(!is.na(df[[cols[2]]]))
  }

  # Check if both 'angle' and 'distance' are provided and fully populated
  has_angle_distance <- columns_valid(data, c("angle", "distance"))

  # Check if both 'dx' and 'dy' are provided and fully populated
  has_fx_fy <- columns_valid(data, c("fx", "fy"))

  # If neither pair is fully provided, throw an error
  if (!(has_angle_distance || has_fx_fy)) {
    stop("You must provide either both 'fx' and 'fy' or both 'angle' and 'distance' aesthetics.")
  }

  # Return a list indicating which pair is present
  return(list(
    has_angle_distance = has_angle_distance,
    has_fx_fy = has_fx_fy
  ))
}

# Compute ellipse parameters based on covariance - geom_vector_smooth
compute_ellipse_params <- function(var_fx, var_fy, cov_fx_fy, conf_level = 0.95) {
  # Construct the covariance matrix
  cov_matrix <- matrix(c(var_fx, cov_fx_fy, cov_fx_fy, var_fy), nrow = 2)

  # Eigen decomposition
  eig <- eigen(cov_matrix)

  # Eigenvalues and eigenvectors
  eigenvalues <- eig$values
  eigenvectors <- eig$vectors

  # Compute the angle of rotation in degrees
  angle <- atan2(eigenvectors[2,1], eigenvectors[1,1]) * (180 / pi)

  # Compute the scaling factor based on the desired confidence level
  # For a 95% confidence ellipse in 2D, the scaling factor is sqrt(5.991)
  # This comes from the chi-square distribution with 2 degrees of freedom
  chi_sq_val <- stats::qchisq(conf_level, df = 2)
  scale_factor <- sqrt(chi_sq_val)

  # Width and height of the ellipse (2 * axis lengths)
  width <- 2 * scale_factor * sqrt(eigenvalues[1])
  height <- 2 * scale_factor * sqrt(eigenvalues[2])

  return(list(width = width, height = height, angle = angle))
}


# Define a helper function to compute wedge endpoints (unchanged) - geom_vector_smooth
compute_prediction_endpoints <- function(x, y, fx, fy, angle_lower, angle_upper) {
  # Validate inputs
  if (!is.numeric(x) || length(x) != 1) {
    stop("Input 'x' must be a single numeric value.")
  }
  if (!is.numeric(y) || length(y) != 1) {
    stop("Input 'y' must be a single numeric value.")
  }
  if (!is.numeric(fx) || length(fx) != 1) {
    stop("Input 'fx' must be a single numeric value.")
  }
  if (!is.numeric(fy) || length(fy) != 1) {
    stop("Input 'fy' must be a single numeric value.")
  }
  if (!is.numeric(angle_lower) || length(angle_lower) != 1) {
    stop("Input 'angle_lower' must be a single numeric value in radians.")
  }
  if (!is.numeric(angle_upper) || length(angle_upper) != 1) {
    stop("Input 'angle_upper' must be a single numeric value in radians.")
  }

  # Calculate the magnitude of the predicted vector
  magnitude <- sqrt(fx^2 + fy^2)

  # Compute the endpoints based on absolute angles
  xend_lower <- x + magnitude * cos(angle_lower)
  yend_lower <- y + magnitude * sin(angle_lower)

  xend_upper <- x + magnitude * cos(angle_upper)
  yend_upper <- y + magnitude * sin(angle_upper)

  # Create and return the output dataframe
  result_df <- data.frame(
    xend_lower = xend_lower,
    yend_lower = yend_lower,
    xend_upper = xend_upper,
    yend_upper = yend_upper
  )

  return(result_df)
}

# create circles in geom_vector_smooth
create_circle_data <- function(x, y, radius, n = 100, group) {
  angle <- seq(0, 2 * pi, length.out = n)
  data.frame(
    x = x + radius * cos(angle),
    y = y + radius * sin(angle),
    group = group
  )
}

# create circles in geom_vector_smooth
create_wedge_data <- function(
    x, y, xend_upper, yend_upper, xend_lower, yend_lower,
    xend, yend, id, n_points = 100,
    outer_radius = NULL,
    inner_radius = 0
) {
  # Calculate angles using atan2 for upper, lower, and midpoint
  angle_upper <- atan2(yend_upper - y, xend_upper - x) + 2*pi#%% (2 * pi)
  angle_lower <- atan2(yend_lower - y, xend_lower - x) + 2*pi#%% (2 * pi)
  midpoint_angle <- atan2(yend - y, xend - x) + 2*pi#%% (2 * pi)

  if(angle_upper < angle_lower){
    angle_upper <- angle_upper + 2*pi
  }

  # Calculate the shift to bring the midpoint to 0 radians
  shift <- -midpoint_angle

  # Shift all angles by the same amount
  shifted_upper <- (angle_upper + shift) #%% (2 * pi)
  shifted_lower <- (angle_lower + shift) #%% (2 * pi)

  # Generate arc points from upper (positive) to lower (negative)
  arc_angles <- seq(shifted_upper, shifted_lower, length.out = n_points)

  # Create outer arc
  arc_x_outer <- outer_radius * cos(arc_angles)
  arc_y_outer <- outer_radius * sin(arc_angles)

  # Rotate the arc points back to the original coordinate system
  final_x_outer <- x + arc_x_outer * cos(-shift) - arc_y_outer * sin(-shift)
  final_y_outer <- y + arc_x_outer * sin(-shift) + arc_y_outer * cos(-shift)

  if (inner_radius > 0) {
    # if (inner_radius > 0 && !is.null(inner_radius)) {
    # Create inner arc
    arc_x_inner <- inner_radius * cos(arc_angles)
    arc_y_inner <- inner_radius * sin(arc_angles)

    # Rotate the inner arc points back to the original coordinate system
    final_x_inner <- x + arc_x_inner * cos(-shift) - arc_y_inner * sin(-shift)
    final_y_inner <- y + arc_x_inner * sin(-shift) + arc_y_inner * cos(-shift)

    # Combine outer and inner arcs to form an annular wedge
    wedge_data <- data.frame(
      x = c(final_x_outer, rev(final_x_inner)),
      y = c(final_y_outer, rev(final_y_inner)),
      group = rep(id, each = length(final_x_outer) + length(final_x_inner)),
      id = rep(id, each = length(final_x_outer) + length(final_x_inner))
    )

  } else {
    # Create regular wedge by connecting outer arc to center
    wedge_data <- data.frame(
      x = c(x, final_x_outer, x),
      y = c(y, final_y_outer, y),
      group = rep(id, length.out = n_points + 2),
      id = rep(id, length.out = n_points + 2)
    )
  }

  return(wedge_data)
}

# Helper function to create ellipse polygon data
create_ellipse_data <- function(x_center, y_center, width, height, angle, n_points = 50) {
  theta <- seq(0, 2 * pi, length.out = n_points)
  ellipse <- data.frame(
    theta = theta,
    x = width / 2 * cos(theta),
    y = height / 2 * sin(theta)
  )

  # Rotation matrix
  rotation_matrix <- matrix(c(cos(angle * pi / 180), -sin(angle * pi / 180),
                              sin(angle * pi / 180),  cos(angle * pi / 180)),
                            nrow = 2)

  rotated <- as.matrix(ellipse[, c("x", "y")]) %*% rotation_matrix
  ellipse$x <- rotated[,1] + x_center
  ellipse$y <- rotated[,2] + y_center

  return(ellipse)
}

predict_theta_interval <- function(x, y, mux, muy, Sigma, rho = NULL, conf_level) {

  # Validate inputs
  if (!is.numeric(x) || length(x) != 1) {
    stop("Input 'x' must be a single numeric value.")
  }
  if (!is.numeric(y) || length(y) != 1) {
    stop("Input 'y' must be a single numeric value.")
  }
  if (!is.numeric(mux) || length(mux) != 1) {
    stop("Input 'mux' must be a single numeric value.")
  }
  if (!is.numeric(muy) || length(muy) != 1) {
    stop("Input 'muy' must be a single numeric value.")
  }
  if (!is.matrix(Sigma) || any(dim(Sigma) != c(2, 2))) {
    stop("Input 'Sigma' must be a 2x2 covariance matrix.")
  }
  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("Input 'conf_level' must be a numeric value between 0 and 1.")
  }

  # If rho is not provided, calculate it (not used by delta method but for compatibility)
  if (is.null(rho)) {
    sigma_x <- sqrt(Sigma[1, 1])
    sigma_y <- sqrt(Sigma[2, 2])
    rho <- Sigma[1, 2] / (sigma_x * sigma_y)
  }

  # Compute the predicted vector's angle (theta_pred) in [0, 2*pi)
  theta_pred <- atan2(muy, mux)
  if (theta_pred < 0) theta_pred <- theta_pred + 2*pi

  # Delta method: compute the variance of theta = arctan2(muy, mux)
  denom <- mux^2 + muy^2
  if (denom == 0) {
    return(data.frame(min_angle = NA, max_angle = NA))
  }
  grad <- c(-muy/denom, mux/denom)
  var_theta <- as.numeric(t(grad) %*% Sigma %*% grad)
  se_theta <- sqrt(var_theta)

  # Get the z-value for the desired confidence level (e.g., ~1.96 for 95%)
  z_val <- qnorm(1 - (1 - conf_level) / 2)

  # Compute the raw (unwrapped) confidence interval for theta
  raw_lower <- theta_pred - z_val * se_theta
  raw_upper <- theta_pred + z_val * se_theta

  # If the raw interval width is >= 2*pi, cap the interval at the full circle.
  if ((raw_upper - raw_lower) >= 2*pi) {
    # cat("Delta method interval exceeds full circle; capping at [0, 2*pi].\n")
    return(data.frame(min_angle = .001, max_angle = 2*pi))
  }

  # Unwrap the interval so it is continuous:
  if (raw_lower < 0) {
    lower_unwrapped <- raw_lower + 2*pi
    upper_unwrapped <- raw_upper + 2*pi
  } else if (raw_upper >= 2*pi) {
    lower_unwrapped <- raw_lower - 2*pi
    upper_unwrapped <- raw_upper - 2*pi
  } else {
    lower_unwrapped <- raw_lower
    upper_unwrapped <- raw_upper
  }

  data.frame(min_angle = lower_unwrapped, max_angle = upper_unwrapped)
}






# predict_theta_interval <- function(x, y, mux, muy, Sigma, rho = NULL, conf_level) {
#   # Validate inputs
#   if (!is.numeric(x) || length(x) != 1) {
#     stop("Input 'x' must be a single numeric value.")
#   }
#   if (!is.numeric(y) || length(y) != 1) {
#     stop("Input 'y' must be a single numeric value.")
#   }
#   if (!is.numeric(mux) || length(mux) != 1) {
#     stop("Input 'mux' must be a single numeric value.")
#   }
#   if (!is.numeric(muy) || length(muy) != 1) {
#     stop("Input 'muy' must be a single numeric value.")
#   }
#   if (!is.matrix(Sigma) || any(dim(Sigma) != c(2, 2))) {
#     stop("Input 'Sigma' must be a 2x2 covariance matrix.")
#   }
#   if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
#     stop("Input 'conf_level' must be a numeric value between 0 and 1.")
#   }
#
#   # If rho is not provided, calculate it from Sigma
#   if (is.null(rho)) {
#     sigma_x <- sqrt(Sigma[1, 1])
#     sigma_y <- sqrt(Sigma[2, 2])
#     rho <- Sigma[1, 2] / (sigma_x * sigma_y)
#   }
#
#   # Compute the predicted vector's angle (in radians)
#   theta_pred <- atan2(muy, mux)
#
#   # Define the bivariate normal PDF in polar coordinates for the error distribution.
#   # Here the error distribution is assumed to be centered at (0,0)
#   bivariate_normal <- function(r, theta, Sigma) {
#     x_val <- r * cos(theta)
#     y_val <- r * sin(theta)
#     inv_Sigma <- solve(Sigma)
#     det_Sigma <- det(Sigma)
#     exponent <- -0.5 * (inv_Sigma[1, 1] * x_val^2 +
#                           2 * inv_Sigma[1, 2] * x_val * y_val +
#                           inv_Sigma[2, 2] * y_val^2)
#     return((r / (2 * pi * sqrt(det_Sigma))) * exp(exponent))
#   }
#
#   # Define the marginal density for theta by integrating out r
#   marginal_theta <- function(theta) {
#     integrate(
#       f = function(r) { bivariate_normal(r, theta, Sigma) },
#       lower = 0, upper = Inf,
#       rel.tol = 1e-8
#     )$value
#   }
#
#   # Compute the angular interval over a theta grid
#   compute_interval <- function(theta_vals, conf_level) {
#     pdf_vals <- sapply(theta_vals, marginal_theta)
#     delta_theta <- diff(theta_vals)[1]
#     pdf_vals <- pdf_vals / sum(pdf_vals * delta_theta)  # Normalize PDF
#     cdf_vals <- cumsum(pdf_vals * delta_theta)
#
#     alpha <- 1 - conf_level
#     lower_bound <- alpha / 2
#     upper_bound <- 1 - lower_bound
#
#     lower_index <- which(cdf_vals >= lower_bound)[1]
#     upper_index <- which(cdf_vals >= upper_bound)[1]
#
#     theta_lower <- theta_vals[lower_index]
#     theta_upper <- theta_vals[upper_index]
#
#     list(theta_lower = theta_lower, theta_upper = theta_upper, cdf_vals = cdf_vals)
#   }
#
#   # Use integration over -pi to pi
#   theta_vals <- seq(-pi, pi, length.out = 200)
#   interval <- compute_interval(theta_vals, conf_level)
#   theta_lower <- interval$theta_lower
#   theta_upper <- interval$theta_upper
#
#   # Shift the computed wedge by the predicted angle and wrap into [0, 2*pi)
#   min_angle <- (theta_lower + theta_pred) %% (2 * pi)
#   max_angle <- (theta_upper + theta_pred) %% (2 * pi)
#
#   data.frame(min_angle = min_angle, max_angle = max_angle)
# }


# geom_stream_field
matrix_to_df_with_names <- function(mat, col_names = NULL) {
  # Check if the input is a matrix
  if (!is.matrix(mat)) stop("Input must be a matrix.")

  # Convert to a data frame
  df <- as.data.frame(mat)

  # Set column names if provided
  if (!is.null(col_names)) {
    if (length(col_names) != ncol(df)) {
      stop("`col_names` must have the same length as the number of columns in the matrix.")
    }
    names(df) <- col_names
  }

  return(df)
}

utils::globalVariables(c("potential", "fun", "f", "l", "max_t", "avg_spd", "x", "y", "fx", "fy", "normalize", "z"))
