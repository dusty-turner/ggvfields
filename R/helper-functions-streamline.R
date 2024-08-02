ensure_length_two <- function(n) {
  if (length(n) == 1) n <- rep(n, 2)
  if (length(n) != 2) stop("Length of 'n' must be 2")
  return(n)
}

# Functions for different mask shapes

# No inset shape (square mask)
is_too_close_square <- function(xi, yi, mask, xlim, ylim, n) {
  xi_index <- floor((xi - xlim[1]) / ((xlim[2] - xlim[1]) / n[1])) + 1
  yi_index <- floor((yi - ylim[1]) / ((ylim[2] - ylim[1]) / n[2])) + 1

  if (any(xi_index < 1) || any(yi_index < 1) || any(xi_index > ncol(mask)) || any(yi_index > nrow(mask))) {
    return(TRUE)
  }

  return(mask[yi_index, xi_index] == 1)
}

update_mask_square <- function(mask, xi, yi, xlim, ylim, n) {
  xi_index <- floor((xi - xlim[1]) / ((xlim[2] - xlim[1]) / n[1])) + 1
  yi_index <- floor((yi - ylim[1]) / ((ylim[2] - ylim[1]) / n[2])) + 1

  if (xi_index >= 1 && yi_index >= 1 && xi_index <= ncol(mask) && yi_index <= nrow(mask)) {
    mask[yi_index, xi_index] <- 1
  }
  return(mask)
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

# Combined function for bidirectional integration using Euler's method
euler_integrate_bidirectional <- function(xi, yi, f, ds, max_length, max_steps, n, mask, xlim, ylim, is_too_close, update_mask) {
  integrate_fixed <- function(xi, yi, f, ds, max_length, max_steps, n, mask, xlim, ylim, is_too_close) {
    xy_traj <- list(c(xi, yi))
    steps <- 0
    length_traveled <- 0

    while (length_traveled < max_length && steps < max_steps) {
      steps <- steps + 1
      uv <- f(c(xi, yi))
      u <- uv[1]
      v <- uv[2]
      norm <- sqrt(u^2 + v^2)
      if (norm == 0) break

      dx <- ds * (u / norm)
      dy <- ds * (v / norm)

      xi <- xi + dx
      yi <- yi + dy
      length_traveled <- length_traveled + sqrt(dx^2 + dy^2)

      if (is_too_close(xi, yi, mask, xlim, ylim, n)) break

      xy_traj <- append(xy_traj, list(c(xi, yi)))
    }

    return(list(traj = xy_traj))
  }

  forward_result <- integrate_fixed(xi, yi, f, ds, max_length, max_steps, n, mask, xlim, ylim, is_too_close)
  reverse_f <- function(v) -f(v)
  backward_result <- integrate_fixed(xi, yi, reverse_f, ds, max_length, max_steps, n, mask, xlim, ylim, is_too_close)

  combined_traj <- c(rev(backward_result$traj[-1]), forward_result$traj)

  for (point in combined_traj) {
    mask <- update_mask(mask, point[1], point[2], xlim, ylim, n)
  }

  return(list(traj = combined_traj, mask = mask))
}

# Function to generate starting points for streamlines
generate_starting_points <- function(mask_shape) {
  points <- list()
  nx <- mask_shape[2]
  ny <- mask_shape[1]

  for (x in 1:nx) points <- append(points, list(c(x, ny)))
  for (y in (ny-1):1) points <- append(points, list(c(nx, y)))
  for (x in (nx-1):1) points <- append(points, list(c(x, 1)))
  for (y in 2:(ny-1)) points <- append(points, list(c(1, y)))

  layer <- 1
  while (layer <= floor(min(nx, ny) / 2)) {
    for (x in layer:(nx - layer)) points <- append(points, list(c(x, ny - layer + 1)))
    for (y in (ny - layer):(layer + 1)) points <- append(points, list(c(nx - layer + 1, y)))
    for (x in (nx - layer + 1):(layer + 1)) points <- append(points, list(c(x, layer)))
    for (y in (layer + 1):(ny - layer)) points <- append(points, list(c(layer, y)))

    layer <- layer + 1
  }

  return(points)
}

# Main function for generating stream plots
streamplot <- function(f, xlim, ylim, n, max_length, max_steps, ds, mask_shape_type = "square") {

  mask_shape <- c(n[2], n[1])
  mask <- matrix(0, nrow = mask_shape[1], ncol = mask_shape[2])

  trajectories <- list()
  traj_id <- 1

  if (mask_shape_type == "inset_square") {
    ## this inset_fraction is not currently available to the user
    is_too_close <- function(xi, yi, mask, xlim, ylim, n) is_too_close_inset_square(xi, yi, mask, xlim, ylim, n, inset_fraction = 0.5)
    update_mask <- function(mask, xi, yi, xlim, ylim, n) update_mask_inset_square(mask, xi, yi, xlim, ylim, n, inset_fraction = 0.5)
  } else {
    is_too_close <- switch(mask_shape_type,
                           square = is_too_close_square,
                           diamond = is_too_close_diamond)

    update_mask <- switch(mask_shape_type,
                          square = update_mask_square,
                          diamond = update_mask_diamond)
  }

  for (start in generate_starting_points(mask_shape)) {
    xi <- xlim[1] + (start[[1]] - 0.5) * ((xlim[2] - xlim[1]) / mask_shape[2])
    yi <- ylim[1] + (start[[2]] - 0.5) * ((ylim[2] - ylim[1]) / mask_shape[1])

    if (!is_too_close(xi, yi, mask, xlim, ylim, n)) {
      result <- euler_integrate_bidirectional(xi, yi, f, ds, max_length, max_steps, n, mask, xlim, ylim, is_too_close, update_mask)
      traj <- result$traj
      mask <- result$mask
      if (length(traj) > 1) {
        traj <- lapply(traj, function(point) c(point, id = traj_id))
        trajectories <- append(trajectories, list(traj))
        traj_id <- traj_id + 1
      }
    }
  }

  return(trajectories)
}
