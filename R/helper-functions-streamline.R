is_too_close <- function(xi, yi, mask, xlim, ylim, min_dist) {
  xi_index <- floor((xi - xlim[1]) / min_dist) + 1
  yi_index <- floor((yi - ylim[1]) / min_dist) + 1

  if (xi_index < 1 || yi_index < 1 || xi_index > ncol(mask) || yi_index > nrow(mask)) {
    return(TRUE)
  }

  return(mask[yi_index, xi_index] == 1)
}

update_mask <- function(mask, xi, yi, xlim, ylim, min_dist) {
  xi_index <- floor((xi - xlim[1]) / min_dist) + 1
  yi_index <- floor((yi - ylim[1]) / min_dist) + 1

  if (xi_index >= 1 && yi_index >= 1 && xi_index <= ncol(mask) && yi_index <= nrow(mask)) {
    mask[yi_index, xi_index] <- 1
  }
  return(mask)
}

euler_integrate_bidirectional <- function(xi, yi, f, ds, max_length, max_steps, min_dist, mask, xlim, ylim) {
  integrate_fixed <- function(xi, yi, f, ds, max_length, max_steps, min_dist, mask, xlim, ylim) {
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

      if (is_too_close(xi, yi, mask, xlim, ylim, min_dist)) break

      xy_traj <- append(xy_traj, list(c(xi, yi)))
    }

    return(list(traj = xy_traj))
  }

  forward_result <- integrate_fixed(xi, yi, f, ds, max_length, max_steps, min_dist, mask, xlim, ylim)
  reverse_f <- function(v) -f(v)
  backward_result <- integrate_fixed(xi, yi, reverse_f, ds, max_length, max_steps, min_dist, mask, xlim, ylim)

  combined_traj <- c(rev(backward_result$traj[-1]), forward_result$traj)

  for (point in combined_traj) {
    mask <- update_mask(mask, point[1], point[2], xlim, ylim, min_dist)
  }

  return(list(traj = combined_traj, mask = mask))
}

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

streamplot <- function(f, xlim, ylim, max_length, max_steps, ds, min_dist) {

  mask_shape <- c(ceiling((ylim[2] - ylim[1]) / min_dist), ceiling((xlim[2] - xlim[1]) / min_dist))
  mask <- matrix(0, nrow = mask_shape[1], ncol = mask_shape[2])

  trajectories <- list()
  traj_id <- 1

  for (start in generate_starting_points(mask_shape)) {
    xi <- xlim[1] + (start[[1]] - 0.5) * ((xlim[2] - xlim[1]) / mask_shape[1])
    yi <- ylim[1] + (start[[2]] - 0.5) * ((ylim[2] - ylim[1]) / mask_shape[1])

    if (!is_too_close(xi, yi, mask, xlim, ylim, min_dist)) {
      result <- euler_integrate_bidirectional(xi, yi, f, ds, max_length, max_steps, min_dist, mask, xlim, ylim)
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
