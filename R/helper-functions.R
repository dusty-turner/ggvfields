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
  function(v, ...) {
    stopifnot(is.numeric(v))
    if (is.vector(v)) v <- matrix(v, nrow = 1)
    out <- vector("list", nrow(v))

    for (i in seq_len(nrow(v))) {
      # Pass ... along to f()
      out[[i]] <- f(v[i, ], ...)
    }

    out <- t(simplify2array(out))
    if ((nrow(out) == 1L) && drop) out[1, ] else out
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

# Compute ellipse parameters based on covariance
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


generate_starting_points_flow <- function(mask_shape, build_type = "spiral_traversal") {
  # Extract dimensions from mask_shape
  nx <- mask_shape[2]
  ny <- mask_shape[1]

  points <- list()

  if (build_type == "spiral_traversal") {
    # Spiral Traversal Logic
    x_min <- 1
    x_max <- nx
    y_min <- 1
    y_max <- ny

    while (x_min <= x_max && y_min <= y_max) {
      # Top row (left to right)
      for (x in x_min:x_max) {
        points <- append(points, list(c(x, y_max)))
      }
      y_max <- y_max - 1

      # Right column (top to bottom)
      for (y in y_max:y_min) {
        points <- append(points, list(c(x_max, y)))
      }
      x_max <- x_max - 1

      # Bottom row (right to left)
      if (y_min <= y_max) {
        for (x in rev(x_min:x_max)) {
          points <- append(points, list(c(x, y_min)))
        }
        y_min <- y_min + 1
      }

      # Left column (bottom to top)
      if (x_min <= x_max) {
        for (y in rev(y_max:y_min)) {
          points <- append(points, list(c(x_min, y)))
        }
        x_min <- x_min + 1
      }
    }

  } else if (build_type == "layer") {
    # Layer by Layer (Top to Bottom, Left to Right) Logic
    for (y in ny:1) {  # Start from the top row and move downward
      for (x in 1:nx) {  # Traverse each row from left to right
        points <- append(points, list(c(x, y)))
      }
    }
  } else {
    stop("Invalid build_type provided. Use 'spiral_traversal' or 'layer'.")
  }

  # Ensure unique points (removing any accidental duplicates)
  points <- unique(points)

  return(points)
}






# Function to solve the ODE using deSolve with boundary checking
#' @importFrom deSolve ode
# solve_flow <- function(initial_state, fun, times, xlim, ylim, method) {
#   parameters <- list(fun = fun, xlim = xlim, ylim = ylim)
#
#   result <- ode(y = initial_state, times = times, func = flow_ode, parms = parameters, method = method)
#
#   # Drop rows with NA values, since these indicate where the solver stopped
#   result <- as.data.frame(na.omit(result))
#
#   return(result)
# }

flow_ode <- function(time, state, parameters) {
  x <- state[1]
  y <- state[2]
  xlim <- parameters$xlim
  ylim <- parameters$ylim

  # print(time)
  # print(c(x,y))
  # If x or y is NA, return NA to stop the solver
  if (is.na(x) || is.na(y)) {
    return(list(c(NA, NA)))
  }

  # Check if x or y is outside the boundary
  if (x < xlim[1] || x > xlim[2] || y < ylim[1] || y > ylim[2]) {
    return(list(c(NA, NA)))  # Returning NA will stop the solver
  }

  # Calculate the derivatives
  dx <- parameters$fun(c(x, y))[1]
  dy <- parameters$fun(c(x, y))[2]
  # print(c(dx,dy))
  return(list(c(dx, dy)))
}

# Function to calculate the grid indices based on real-world coordinates
calculate_indices <- function(x, y, xlim, ylim, n) {
  xi <- min(floor((x - xlim[1]) / ((xlim[2] - xlim[1]) / n[2])) + 1, n[2])
  yi <- min(floor((y - ylim[1]) / ((ylim[2] - ylim[1]) / n[1])) + 1, n[1])
  if(xi <= 0) xi <- 1
  if(yi <= 0) yi <- 1
  return(c(xi, yi))
}

# Function to flip the y-index to match the mask's orientation
flip_y_index <- function(yi, n) {
  return(n[1] - yi + 1)
}

# Function to check if a point is within a masked area
is_masked_area <- function(xi, yi, n, mask) {
  xi_index <- xi
  yi_index <- flip_y_index(yi, n)
  return(mask[yi_index, xi] == 1)
}

# Function to update the mask for a square shape
update_mask_square <- function(mask, x, y, xlim, ylim, n) {
  indices <- calculate_indices(x, y, xlim, ylim, n)
  xi_index <- indices[1]
  yi_index <- indices[2]
  yi_index <- flip_y_index(yi_index, n)
  mask[yi_index, xi_index] <- 1
  return(mask)
}

# ---------------------------------------------------------------------
# Diamond Mask Functions

# Function to check if a point is within a diamond shape
is_within_diamond <- function(xi, yi, x_center, y_center, half_diagonal) {
  manhattan_distance <- abs(xi - x_center) + abs(yi - y_center)
  return(manhattan_distance <= half_diagonal)
}

# Function to update the mask for a diamond shape
update_mask_diamond <- function(mask, x, y, xlim, ylim, n) {
  indices <- calculate_indices(x, y, xlim, ylim, n)
  xi_index <- indices[1]
  yi_index <- indices[2]

  x_center <- xlim[1] + (xi_index - 0.5) * ((xlim[2] - xlim[1]) / n[1])
  y_center <- ylim[1] + (yi_index - 0.5) * ((ylim[2] - ylim[1]) / n[2])
  half_diagonal <- min(c((xlim[2] - xlim[1]) / n[1], (ylim[2] - ylim[1]) / n[2])) / 2

  if (is_within_diamond(x, y, x_center, y_center, half_diagonal)) {
    yi_index <- flip_y_index(yi_index, n)
    mask[yi_index, xi_index] <- 1
  }

  return(mask)
}

# ---------------------------------------------------------------------
# Inset Square Mask Functions

# Function to check if a point is within an inset square
is_within_inset_square <- function(xi, yi, x_center, y_center, inset_side) {
  x_min <- x_center - inset_side / 2
  x_max <- x_center + inset_side / 2
  y_min <- y_center - inset_side / 2
  y_max <- y_center + inset_side / 2
  return(xi >= x_min && xi <= x_max && yi >= y_min && yi <= y_max)
}

# Function to update the mask for an inset square
update_mask_inset_square <- function(mask, x, y, xlim, ylim, n, inset_fraction = 0.5) {
  indices <- calculate_indices(x, y, xlim, ylim, n)
  xi_index <- indices[1]
  yi_index <- indices[2]

  x_center <- xlim[1] + (xi_index - 0.5) * ((xlim[2] - xlim[1]) / n[1])
  y_center <- ylim[1] + (yi_index - 0.5) * ((ylim[2] - ylim[1]) / n[2])
  inset_side <- min(c((xlim[2] - xlim[1]) / n[1], (ylim[2] - ylim[1]) / n[2])) * inset_fraction

  if (is_within_inset_square(x, y, x_center, y_center, inset_side)) {
    yi_index <- flip_y_index(yi_index, n)
    mask[yi_index, xi_index] <- 1
  }

  return(mask)
}

# ---------------------------------------------------------------------
# Circle Mask Functions

# Function to check if a point is within a circle
is_within_circle <- function(xi, yi, x_center, y_center, radius) {
  return(sqrt((xi - x_center)^2 + (yi - y_center)^2) <= radius)
}

# Function to check if a point is too close to a circular masked area
is_too_close_circle <- function(xi, yi, mask, xlim, ylim, n, circle_fraction = 0.5) {
  xi_index <- floor((xi - xlim[1]) / ((xlim[2] - xlim[1]) / n[1])) + 1
  yi_index <- floor((yi - ylim[1]) / ((ylim[2] - ylim[1]) / n[2])) + 1

  if (xi_index < 1 || yi_index < 1 || xi_index > ncol(mask) || yi_index > nrow(mask)) {
    return(TRUE)
  }

  x_center <- xlim[1] + (xi_index - 0.5) * ((xlim[2] - xlim[1]) / n[1])
  y_center <- ylim[1] + (yi_index - 0.5) * ((ylim[2] - ylim[1]) / n[2])
  radius <- min(c((xlim[2] - xlim[1]) / n[1], (ylim[2] - ylim[1]) / n[2])) * circle_fraction

  return(mask[flip_y_index(yi_index, n), xi_index] == 1 && is_within_circle(xi, yi, x_center, y_center, radius))
}

# Function to update the mask for a circular shape
update_mask_circle <- function(mask, xi, yi, xlim, ylim, n, circle_fraction = 0.5) {
  indices <- calculate_indices(xi, yi, xlim, ylim, n)
  xi_index <- indices[1]
  yi_index <- indices[2]

  x_center <- xlim[1] + (xi_index - 0.5) * ((xlim[2] - xlim[1]) / n[1])
  y_center <- ylim[1] + (yi_index - 0.5) * ((ylim[2] - ylim[1]) / n[2])
  radius <- min(c((xlim[2] - xlim[1]) / n[1], (ylim[2] - ylim[1]) / n[2])) * circle_fraction

  if (is_within_circle(xi, yi, x_center, y_center, radius)) {
    mask[flip_y_index(yi_index, n), xi_index] <- 1
  }

  return(mask)
}

# ---------------------------------------------------------------------
# Generate Starting Points Function

# Function to generate starting points for the stream plot
generate_starting_points <- function(mask_shape) {
  points <- list()
  nx <- mask_shape[2]
  ny <- mask_shape[1]
  layer <- 0

  while (layer < ceiling(min(nx, ny) / 2)) {
    # Top row, left to right
    for (x in (1 + layer):(nx - layer)) {
      points <- append(points, list(c(x, ny - layer)))
    }

    # Right column, top to bottom
    for (y in (ny - 1 - layer):(1 + layer + 1)) {
      points <- append(points, list(c(nx - layer, y)))
    }

    # Bottom row, right to left
    for (x in (nx - 1 - layer):(1 + layer)) {
      points <- append(points, list(c(x, 1 + layer)))
    }

    # Left column, bottom to top
    for (y in (2 + layer):(ny - 1 - layer)) {
      points <- append(points, list(c(1 + layer, y)))
    }

    layer <- layer + 1
  }

  return(points)
}

# ---------------------------------------------------------------------
# Solve Flow and ODE Functions

# Function to solve the flow using the ODE solver
#' @importFrom deSolve ode
solve_flow <- function(initial_state, fun, times, xlim, ylim, method = "rk4") {
  parameters <- list(fun = fun, xlim = xlim, ylim = ylim)
  result <- ode(y = initial_state, times = times, func = flow_ode, parms = parameters, method = method)
  result <- as.data.frame(na.omit(result))  # Remove rows with NA values
  return(result)
}

# Function that defines the flow ODE system
flow_ode <- function(time, state, parameters) {
  x <- state[1]
  y <- state[2]
  xlim <- parameters$xlim
  ylim <- parameters$ylim

  if (is.na(x) || is.na(y) || x < xlim[1] || x > xlim[2] || y < ylim[1] || y > ylim[2]) {
    return(list(c(NA, NA)))  # Stop the solver if outside boundaries or NA values
  }

  dx <- parameters$fun(c(x, y))[1]
  dy <- parameters$fun(c(x, y))[2]
  return(list(c(dx, dy)))
}

# Function to calculate wedge angles from (x, y) to ellipse perimeter
calculate_wedge_angles <- function(x, y, xend, yend, a, b, angle_deg) {
  # Convert angle to radians
  phi <- angle_deg * pi / 180

  # Generate points around the ellipse perimeter
  theta <- seq(0, 2 * pi, length.out = 360)
  ellipse_x <- a * cos(theta)
  ellipse_y <- b * sin(theta)

  # Rotate the ellipse
  rotated_x <- ellipse_x * cos(phi) - ellipse_y * sin(phi)
  rotated_y <- ellipse_x * sin(phi) + ellipse_y * cos(phi)

  # Shift the ellipse to (xend, yend)
  ellipse_coords_x <- rotated_x + xend
  ellipse_coords_y <- rotated_y + yend

  # Calculate angles from (x, y) to each point on the ellipse
  angles <- atan2(ellipse_coords_y - y, ellipse_coords_x - x) * 180 / pi
  angles_deg <- (angles + 360) %% 360

  # Identify the angles corresponding to the upper and lower extremes
  # For simplicity, we'll take the min and max angles
  min_angle <- min(angles_deg)
  max_angle <- max(angles_deg)

  # Handle wrap-around if the span is greater than 180 degrees
  angle_span <- max_angle - min_angle
  if (angle_span > 180) {
    # Choose the smaller arc
    min_angle_new <- max_angle
    max_angle_new <- min_angle + 360
    return(data.frame(min_angle = min_angle_new %% 360, max_angle = max_angle_new %% 360))
  } else {
    return(data.frame(min_angle = min_angle, max_angle = max_angle))
  }
}

# Define a helper function to compute wedge endpoints (unchanged)
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

  # If rho is not provided, calculate it from Sigma
  if (is.null(rho)) {
    sigma_x <- sqrt(Sigma[1, 1])
    sigma_y <- sqrt(Sigma[2, 2])
    rho <- Sigma[1, 2] / (sigma_x * sigma_y)
  }

  # Define the bivariate normal PDF in polar coordinates
  bivariate_normal <- function(r, theta, mux, muy, Sigma) {
    x_val <- r * cos(theta)
    y_val <- r * sin(theta)

    z1 <- x_val - mux
    z2 <- y_val - muy

    inv_Sigma <- solve(Sigma)
    det_Sigma <- det(Sigma)

    # Compute the exponent term using matrix notation
    exponent <- -0.5 * (inv_Sigma[1, 1] * z1^2 +
                          2 * inv_Sigma[1, 2] * z1 * z2 +
                          inv_Sigma[2, 2] * z2^2)

    exp_term <- exp(exponent)

    # Return the PDF value
    return((r / (2 * pi * sqrt(det_Sigma))) * exp_term)
  }

  # Define the marginal_theta function by integrating out r
  marginal_theta <- function(theta) {
    integrate(
      f = function(r) {
        bivariate_normal(r, theta, mux, muy, Sigma)
      },
      lower = 0, upper = Inf,
      rel.tol = 1e-8 # Increase precision if needed
    )$value
  }

  # Helper function to compute interval
  compute_interval <- function(theta_vals, conf_level) {
    # Compute the marginal PDF for each theta
    pdf_vals <- sapply(theta_vals, marginal_theta)

    # Normalize the PDF to ensure it integrates to 1
    delta_theta <- diff(theta_vals)[1]
    pdf_vals <- pdf_vals / sum(pdf_vals * delta_theta)

    # Compute the cumulative distribution function (CDF)
    cdf_vals <- cumsum(pdf_vals * delta_theta)

    # Calculate lower and upper percentiles based on conf_level
    alpha <- 1 - conf_level
    lower_bound <- alpha / 2
    upper_bound <- 1 - lower_bound

    # Determine the 2.5th and 97.5th percentiles for the 95% prediction interval
    lower_index <- which(cdf_vals >= lower_bound)[1]
    upper_index <- which(cdf_vals >= upper_bound)[1]

    theta_lower <- theta_vals[lower_index]
    theta_upper <- theta_vals[upper_index]

    return(list(theta_lower = theta_lower, theta_upper = theta_upper, cdf_vals = cdf_vals))
  }

  # First attempt: integrate from -pi to pi
  theta_vals_initial <- seq(-pi, pi, length.out = 200)
  interval_initial <- compute_interval(theta_vals_initial, conf_level = conf_level)
  theta_lower_initial <- interval_initial$theta_lower
  theta_upper_initial <- interval_initial$theta_upper

  # Check if theta_lower is approximately equal to -theta_upper
  # Define a small threshold for numerical precision
  epsilon <- 1e-6
  is_invalid <- abs(theta_lower_initial + theta_upper_initial) < epsilon

  if (!is_invalid) {
    # Valid interval found with -pi to pi integration
    result_df <- data.frame(
      min_angle = theta_lower_initial,
      max_angle = theta_upper_initial
    )
    # print(result_df)
    return(result_df)
  } else {
    # Invalid interval detected, redo integration from 0 to 2pi
    theta_vals_new <- seq(0, 2 * pi, length.out = 200)
    interval_new <- compute_interval(theta_vals_new, conf_level = conf_level)
    theta_lower_new <- interval_new$theta_lower
    theta_upper_new <- interval_new$theta_upper

    # print("c(x,y)")
    # print(c(x,y))
    # Create the output dataframe
    result_df <- data.frame(
      min_angle = theta_lower_new,
      max_angle = theta_upper_new
    )

    return(result_df)
  }
}

## potential helpers
# Define the numerical potential computation function with proper vectorization
compute_potential <- function(point, fun, x0, y0) {
  x <- point[1]
  y <- point[2]

  # Path 1: Integrate F_x from x0 to x with y = y0
  F_x_func <- function(s) {
    sapply(s, function(si) fun(c(si, y0))[1])
  }

  # Perform the integration for F_x
  integral_x <- integrate(F_x_func, lower = x0, upper = x)$value

  # Path 2: Integrate F_y from y0 to y with x = x
  F_y_func <- function(t) {
    sapply(t, function(ti) fun(c(x, ti))[2])
  }

  # Perform the integration for F_y
  integral_y <- integrate(F_y_func, lower = y0, upper = y)$value

  # Sum the integrals to get the potential
  f_xy <- integral_x + integral_y

  return(f_xy)
}

# Verify if the vector field is conservative
verify_potential <- function(point, fun, tolerance) {
  # Compute the Jacobian matrix numerically

  jacobian_matrix <- numDeriv::jacobian(fun, point)

  # Extract partial derivatives
  df1_dy <- jacobian_matrix[1, 2]  # ∂F_x/∂y
  df2_dx <- jacobian_matrix[2, 1]  # ∂F_y/∂x

  # Check if df1_dy is approximately equal to df2_dx
  symmetric <- abs(df1_dy - df2_dx) <= tolerance

  return(symmetric)
}

## for gradient field
gradient_fun <- function(fun) {
  # This returned function is what geom_vector_field() will actually use
  function(v) {
    # Ensure v is a numeric vector (x, y)
    if (!is.numeric(v) || length(v) != 2) {
      stop("Input to the gradient function must be a numeric vector of length 2 (x, y).")
    }
    # Compute the gradient using numDeriv
    grad_val <- numDeriv::grad(func = fun, x = v)
    return(grad_val)
  }
}

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


norm <- function(u) sqrt(sum(u^2))

normalize <- function(u) u / norm(u)

ip <- function(u, v) sum(u*v)

angle <- function(u, v) {
  u <- normalize(u)
  v <- normalize(v)
  ip_uv <- ip(u, v)
  if (ip_uv < -1) ip_uv <- -1
  if (ip_uv >  1) ip_uv <-  1
  acos( ip_uv )
}


utils::globalVariables(c("Potential", "fun", "f", "l", "max_t", "avg_spd", "x", "y", "fx", "fy"))


