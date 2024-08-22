# Function to generate starting points for flowlines
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

ensure_length_two <- function(n) {
  if (length(n) == 1) n <- rep(n, 2)
  if (length(n) != 2) stop("Length of 'n' must be 2")
  return(n)
}

# Function to solve the ODE using deSolve with boundary checking
#' @importFrom deSolve ode
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

