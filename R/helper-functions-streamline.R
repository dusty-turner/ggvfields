calculate_indices <- function(x, y, xlim, ylim, n) {
  xi <- min(floor((x - xlim[1]) / ((xlim[2] - xlim[1]) / n[2])) + 1, n[2])
  yi <- min(floor((y - ylim[1]) / ((ylim[2] - ylim[1]) / n[1])) + 1, n[1])
  return(c(xi, yi))
}

flip_y_index <- function(yi, n) {
  return(n[1] - yi + 1)
}

update_mask_square <- function(mask, xi, yi, n) {
  xi_index <- xi
  yi_index <- flip_y_index(yi, n = n)  # Flip the y-index here if necessary for mask

  mask[yi_index, xi_index] <- 1
  return(mask)
}

is_too_close_square <- function(xi, yi, n, mask) {
  xi_index <- xi
  yi_index <- flip_y_index(yi, n)

    return(mask[yi_index, xi] == 1)
}


# Diamond mask
is_within_diamond <- function(xi, yi, x_center, y_center, half_diagonal) {
  return(abs(xi - x_center) + abs(yi - y_center) <= half_diagonal)
}

is_too_close_diamond <- function(xi, yi, mask, xlim, ylim, n) {
  xi_index <- floor((xi - xlim[1]) / ((xlim[2] - xlim[1]) / n[1])) + 1
  yi_index <- floor((yi - ylim[1]) / ((ylim[2] - ylim[1]) / n[2])) + 1

  if (any(xi_index < 1) || any(yi_index < 1) || any(xi_index > ncol(mask)) || any(yi_index > nrow(mask))) {
    return(TRUE)
  }

  x_center <- xlim[1] + (xi_index - 0.5) * ((xlim[2] - xlim[1]) / n[1])
  y_center <- ylim[1] + (yi_index - 0.5) * ((ylim[2] - ylim[1]) / n[2])
  half_diagonal <- min(c((xlim[2] - xlim[1]) / n[1], (ylim[2] - ylim[1]) / n[2])) / 2

  return(mask[yi_index, xi_index] == 1 && is_within_diamond(xi, yi, x_center, y_center, half_diagonal))
}

update_mask_diamond <- function(mask, xi, yi, xlim, ylim, n) {
  xi_index <- floor((xi - xlim[1]) / ((xlim[2] - xlim[1]) / n[1])) + 1
  yi_index <- floor((yi - ylim[1]) / ((ylim[2] - ylim[1]) / n[2])) + 1

  if (xi_index >= 1 && yi_index >= 1 && xi_index <= ncol(mask) && yi_index <= nrow(mask)) {
    x_center <- xlim[1] + (xi_index - 0.5) * ((xlim[2] - xlim[1]) / n[1])
    y_center <- ylim[1] + (yi_index - 0.5) * ((ylim[2] - ylim[1]) / n[2])
    half_diagonal <- min(c((xlim[2] - xlim[1]) / n[1], (ylim[2] - ylim[1]) / n[2])) / 2

    if (is_within_diamond(xi, yi, x_center, y_center, half_diagonal)) {
      mask[yi_index, xi_index] <- 1
    }
  }
  return(mask)
}

# Inset square mask
is_within_inset_square <- function(xi, yi, x_center, y_center, inset_side) {
  x_min <- x_center - inset_side / 1.5
  x_max <- x_center + inset_side / 1.5
  y_min <- y_center - inset_side / 1.5
  y_max <- y_center + inset_side / 1.5

  return(xi >= x_min && xi <= x_max && yi >= y_min && yi <= y_max)
}

is_too_close_inset_square <- function(xi, yi, mask, xlim, ylim, n, inset_fraction = 0.5) {
  xi_index <- floor((xi - xlim[1]) / ((xlim[2] - xlim[1]) / n[1])) + 1
  yi_index <- floor((yi - ylim[1]) / ((ylim[2] - ylim[1]) / n[2])) + 1

  if (any(xi_index < 1) || any(yi_index < 1) || any(xi_index > ncol(mask)) || any(yi_index > nrow(mask))) {
    return(TRUE)
  }

  x_center <- xlim[1] + (xi_index - 0.5) * ((xlim[2] - xlim[1]) / n[1])
  y_center <- ylim[1] + (yi_index - 0.5) * ((ylim[2] - ylim[1]) / n[2])
  inset_side <- min(c((xlim[2] - xlim[1]) / n[1], (ylim[2] - ylim[1]) / n[2])) * inset_fraction

  return(mask[yi_index, xi_index] == 1 && is_within_inset_square(xi, yi, x_center, y_center, inset_side))
}

update_mask_inset_square <- function(mask, xi, yi, xlim, ylim, n, inset_fraction = 0.5) {
  xi_index <- floor((xi - xlim[1]) / ((xlim[2] - xlim[1]) / n[1])) + 1
  yi_index <- floor((yi - ylim[1]) / ((ylim[2] - ylim[1]) / n[2])) + 1

  if (xi_index >= 1 && yi_index >= 1 && xi_index <= ncol(mask) && yi_index <= nrow(mask)) {
    x_center <- xlim[1] + (xi_index - 0.5) * ((xlim[2] - xlim[1]) / n[1])
    y_center <- ylim[1] + (yi_index - 0.5) * ((ylim[2] - ylim[1]) / n[2])
    inset_side <- min(c((xlim[2] - xlim[1]) / n[1], (ylim[2] - ylim[1]) / n[2])) * inset_fraction

    if (is_within_inset_square(xi, yi, x_center, y_center, inset_side)) {
      mask[yi_index, xi_index] <- 1
    }
  }
  return(mask)
}

# Circle mask
is_within_circle <- function(xi, yi, x_center, y_center, radius) {
  return(sqrt((xi - x_center)^2 + (yi - y_center)^2) <= radius)
}

is_too_close_circle <- function(xi, yi, mask, xlim, ylim, n, circle_fraction = 0.5) {
  xi_index <- floor((xi - xlim[1]) / ((xlim[2] - xlim[1]) / n[1])) + 1
  yi_index <- floor((yi - ylim[1]) / ((ylim[2] - ylim[1]) / n[2])) + 1

  if (any(xi_index < 1) || any(yi_index < 1) || any(xi_index > ncol(mask)) || any(yi_index > nrow(mask))) {
    return(TRUE)
  }

  x_center <- xlim[1] + (xi_index - 0.5) * ((xlim[2] - xlim[1]) / n[1])
  y_center <- ylim[1] + (yi_index - 0.5) * ((ylim[2] - ylim[1]) / n[2])
  radius <- min(c((xlim[2] - xlim[1]) / n[1], (ylim[2] - ylim[1]) / n[2])) * circle_fraction

  return(mask[yi_index, xi_index] == 1 && is_within_circle(xi, yi, x_center, y_center, radius))
}

update_mask_circle <- function(mask, xi, yi, xlim, ylim, n, circle_fraction = 0.5) {
  xi_index <- floor((xi - xlim[1]) / ((xlim[2] - xlim[1]) / n[1])) + 1
  yi_index <- floor((yi - ylim[1]) / ((ylim[2] - ylim[1]) / n[2])) + 1

  if (xi_index >= 1 && yi_index >= 1 && xi_index <= ncol(mask) && yi_index <= nrow(mask)) {
    x_center <- xlim[1] + (xi_index - 0.5) * ((xlim[2] - xlim[1]) / n[1])
    y_center <- ylim[1] + (yi_index - 0.5) * ((ylim[2] - ylim[1]) / n[2])
    radius <- min(c((xlim[2] - xlim[1]) / n[1], (ylim[2] - ylim[1]) / n[2])) * circle_fraction

    if (is_within_circle(xi, yi, x_center, y_center, radius)) {
      mask[yi_index, xi_index] <- 1
    }
  }
  return(mask)
}
# Combined function for bidirectional integration using Euler's method


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

    # Right column, top to bottom (excluding the corner we've already added)
    for (y in (ny - 1 - layer):(1 + layer + 1)) {
      points <- append(points, list(c(nx - layer, y)))
    }

    # Bottom row, right to left (excluding the corners we've already added)
    for (x in (nx - 1 - layer):(1 + layer)) {
      points <- append(points, list(c(x, 1 + layer)))
    }

    # Left column, bottom to top (excluding the corners we've already added)
    for (y in (2 + layer):(ny - 1 - layer)) {
      points <- append(points, list(c(1 + layer, y)))
    }

    layer <- layer + 1
  }

  return(points)
}





solve_flow <- function(initial_state, fun, times, xlim, ylim) {
  parameters <- list(fun = fun, xlim = xlim, ylim = ylim)

  result <- ode(y = initial_state, times = times, func = flow_ode, parms = parameters, method = "rk4")

  # Drop rows with NA values, since these indicate where the solver stopped
  result <- as.data.frame(na.omit(result))

  return(result)
}


flow_ode <- function(time, state, parameters) {
  x <- state[1]
  y <- state[2]
  xlim <- parameters$xlim
  ylim <- parameters$ylim

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

  return(list(c(dx, dy)))
}
