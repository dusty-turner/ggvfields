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

