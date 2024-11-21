#' @importFrom stats lm median predict quantile
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

# Function to normalize angles to the range [-pi, pi]
normalize_angle <- function(angle) {
  angle <- angle %% (2 * pi)
  angle[angle > pi] <- angle[angle > pi] - 2 * pi
  return(angle)
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

vectorize <- function(f, drop = TRUE) {
  function(v) {
    stopifnot(is.numeric(v))
    if (is.vector(v)) v <- matrix(v, nrow = 1)
    out <- list(nrow(v))
    for (i in 1:nrow(v)) out[[i]] <- f(v[i,])
    out <- t(simplify2array(out))
    if ((nrow(out) == 1L) && drop) out[1,] else out
  }
}

## helper functions for hession / laplacian
extract_component_function <- function(fun, index) {
  function(v) fun(v)[index]
}

## helper functions for hession / laplacian
compute_laplacian <- function(func, v) {
  hessian_matrix <- numDeriv::hessian(func, v)
  sum(diag(hessian_matrix))
}

# Custom draw key function for length aesthetic
draw_key_length <- function(data, params, size) {
  rel_length <- data$length %||% 1
  grid::linesGrob(
    x = c(0.5 - 0.4 * rel_length, 0.5 + 0.4 * rel_length), y = c(0.5, 0.5),  # Adjust the length of the line
    gp = grid::gpar(
      col = alpha(data$colour %||% "black", data$alpha %||% NA),
      lwd = 1  # Constant line width for the legend key
    )
  )
}

# Utility function to replace %||%
`%||%` <- function(a, b) if (!is.null(a)) a else b

# # Helper function to extract expression inside after_stat
# extract_after_stat <- function(mapping, aes_name) {
#   if (!is.null(mapping[[aes_name]])) {
#     aes_expr <- as_label(mapping[[aes_name]])
#
#     pattern <- "after_stat\\(([^)]+)\\)"
#     match <- regexec(pattern, aes_expr)
#
#     if (length(match[[1]]) > 1) {
#       extracted <- regmatches(aes_expr, match)[[1]][2]
#       return(extracted)
#     }
#   }
#   return(NULL)
# }

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
  angle_upper <- atan2(yend_upper - y, xend_upper - x) %% (2 * pi)
  angle_lower <- atan2(yend_lower - y, xend_lower - x) %% (2 * pi)
  midpoint_angle <- atan2(yend - y, xend - x) %% (2 * pi)

  # Calculate the shift to bring the midpoint to 0 radians
  shift <- -midpoint_angle

  # Shift all angles by the same amount
  shifted_upper <- (angle_upper + shift) %% (2 * pi)
  shifted_lower <- (angle_lower + shift) %% (2 * pi)

  # Adjust shifted angles to ensure they are relative to the midpoint:
  # Upper should be positive, lower should be negative
  if (shifted_upper > pi) shifted_upper <- shifted_upper - 2 * pi
  if (shifted_lower > pi) shifted_lower <- shifted_lower - 2 * pi

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



calculate_bounds <- function(fit, se, probs) {
  if (!se) return(NULL)

  # Calculate critical t-value for the confidence level
  t_critical <- qt(1 - (1 - probs) / 2, df = fit$df)

  # Calculate upper and lower bounds
  lower_bound <- fit$fit - t_critical * fit$se.fit
  upper_bound <- fit$fit + t_critical * fit$se.fit

  # Return a list of bounds
  return(list(lower = lower_bound, upper = upper_bound))
}


# Compute Euclidean distances only between nearest neighbors
euclidean_distances <- function(points) {
  n <- nrow(points)
  min_distance <- Inf

  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      # Only consider direct neighbors (1-step difference in x or y)
      if (abs(points$x[i] - points$x[j]) <= min(diff(sort(unique(points$x)))) &&
          abs(points$y[i] - points$y[j]) <= min(diff(sort(unique(points$y))))) {
        dist <- sqrt((points$x[i] - points$x[j])^2 + (points$y[i] - points$y[j])^2)
        min_distance <- min(min_distance, dist)
      }
    }
  }
  return(min_distance)
}

# Function to compute circular quantiles based on angular differences
compute_circular_quantile <- function(theta, theta_mean, prob) {
  # Compute angular differences, wrapped to [-pi, pi]
  angular_diff <- (theta - theta_mean + pi) %% (2 * pi) - pi
  # Compute the desired quantile
  quantile_diff <- quantile(angular_diff, probs = prob, na.rm = TRUE)
  # Add back to the mean and wrap to [0, 2pi)
  theta_quantile <- (theta_mean + quantile_diff + 2 * pi) %% (2 * pi)
  return(theta_quantile)
}

validate_aesthetics <- function(data) {
  # Helper function to check if all specified columns exist and are non-NA
  columns_valid <- function(df, cols) {
    all(cols %in% names(df)) && all(!is.na(df[[cols[1]]])) && all(!is.na(df[[cols[2]]]))
  }

  # Check if both 'angle' and 'distance' are provided and fully populated
  has_angle_distance <- columns_valid(data, c("angle", "distance"))

  # Check if both 'dx' and 'dy' are provided and fully populated
  has_dx_dy <- columns_valid(data, c("dx", "dy"))

  # If neither pair is fully provided, throw an error
  if (!(has_angle_distance || has_dx_dy)) {
    stop("You must provide either both 'dx' and 'dy' or both 'angle' and 'distance' aesthetics.")
  }

  # Return a list indicating which pair is present
  return(list(
    has_angle_distance = has_angle_distance,
    has_dx_dy = has_dx_dy
  ))
}

# Compute ellipse parameters based on covariance
compute_ellipse_params <- function(var_dx, var_dy, cov_dx_dy, conf_level = 0.95) {
  # Construct the covariance matrix
  cov_matrix <- matrix(c(var_dx, cov_dx_dy, cov_dx_dy, var_dy), nrow = 2)

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

