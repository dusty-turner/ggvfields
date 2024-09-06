# Function to calculate the grid indices based on real-world coordinates
calculate_indices <- function(x, y, xlim, ylim, n) {
  xi <- min(floor((x - xlim[1]) / ((xlim[2] - xlim[1]) / n[2])) + 1, n[2])
  yi <- min(floor((y - ylim[1]) / ((ylim[2] - ylim[1]) / n[1])) + 1, n[1])
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
solve_flow <- function(initial_state, fun, times, xlim, ylim) {
  parameters <- list(fun = fun, xlim = xlim, ylim = ylim)
  result <- ode(y = initial_state, times = times, func = flow_ode, parms = parameters, method = "rk4")
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
