#' Create a Flow Plot Geom Layer
#'
#' `geom_flow()` generates a flow plot layer for a user-defined vector
#' field function. The lines in the plot represent the flow of data points
#' through the vector field.
#'
#' The flows are computed using the `deSolve` package's ODE solver. The
#' `rk4` method (a fourth-order Runge-Kutta method) is used by default for
#' numerical integration of the flows.
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#' @importFrom stats na.omit
#' @param fun A user-defined function that takes a numeric vector as input and
#'   returns a numeric vector of the same length, representing the vector field.
#' @param xlim A numeric vector of length 2 giving the x-axis limits.
#' @param ylim A numeric vector of length 2 giving the y-axis limits.
#' @param n A numeric vector of length 2 specifying the grid dimensions for the
#'   starting points of the flows.
#' @param iterations A numeric value specifying the number of time steps for the
#'   ODE solver used in flow generation. This determines the granularity
#'   of the flow paths; higher values result in smoother, more detailed
#'   flows.
#' @param threshold_distance A numeric value specifying the minimum distance
#'   between flows to avoid overlap. If not provided, it defaults to half
#'   the Euclidean distance between adjacent grid points.
#' @param arrow Arrow specification, as created by `grid::arrow()`, for adding
#'   arrows to the flows.
#' @name geom_flow
#' @rdname geom_flow
#'
#' @return A ggplot2 layer that can be added to a ggplot object to produce a
#'   flow plot.
#' @export
#'
#' @examples
#' library(ggvfields)
#' # Example user-defined function
#' f <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(-1 - x^2 + y, 1 + x - y^2)
#' }
#'
#' # Create a ggplot with the flow plot layer
#' ggplot() +
#'   geom_flow(
#'     fun = f, n = c(11, 11),
#'     xlim = c(-10, 10), ylim = c(-10, 10),
#'     iterations = 100, threshold_distance = 0,
#'     arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
#'   ) +
#'   coord_fixed() +
#'   theme_minimal()
NULL

#' @rdname geom_flow
#' @export
geom_flow <- function(mapping = NULL, data = NULL,
                      stat = "flow", position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = TRUE,
                      inherit.aes = TRUE,
                      fun,
                      xlim = c(-10, 10),
                      ylim = c(-10, 10),
                      n = c(21, 21),
                      iterations = 100,
                      threshold_distance = .5,
                      arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")) {
  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)
  layer(
    stat = StatFlow,
    data = data,
    mapping = mapping,
    geom = GeomFlow,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      iterations = iterations,
      threshold_distance = threshold_distance,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_flow
#' @export
stat_flow <- function(mapping = NULL, data = NULL,
                      stat = "flow", position = "identity",
                      ...,
                      na.rm = FALSE,
                      show.legend = TRUE,
                      inherit.aes = TRUE,
                      fun,
                      xlim = c(-10,10),
                      ylim = c(-10,10),
                      n = c(21, 21),
                      iterations = 100,
                      threshold_distance = .5,
                      arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")) {

  if (is.null(data)) data <- ensure_nonempty_data(data)
  layer(
    stat = StatFlow,
    data = data,
    mapping = mapping,
    geom = GeomFlow,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      iterations = iterations,
      threshold_distance = threshold_distance,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_flow
#' @format NULL
#' @usage NULL
#' @export
GeomFlow <- ggproto("GeomFlow", GeomPath)


StatFlow <- ggproto("StatFlow", Stat,
                    default_aes = aes(group = after_stat(id), rownum = after_stat(rownum)),

                    compute_group = function(data, scales, fun, xlim, ylim, n, iterations, threshold_distance = NULL) {

                      n <- ensure_length_two(n)

                      starting_points <- generate_starting_points_flow(n, build_type = "layer")

                      # Convert the starting points to actual coordinates
                      x_seq <- seq(xlim[1], xlim[2], length.out = n[2])
                      y_seq <- seq(ylim[1], ylim[2], length.out = n[1])
                      grid_coords <- lapply(starting_points, function(p) c(x_seq[p[1]], y_seq[p[2]]))

                      times_forward <- seq(0, 1, length.out = iterations)  # Time sequence for ODE solver
                      times_backward <- seq(0, -1, length.out = iterations)  # Time sequence for ODE solver

                      # Generate the forward flow
                      forward_trajectories <- lapply(seq_along(grid_coords), function(i) {

                        initial_state <- c(x = grid_coords[[i]][1], y = grid_coords[[i]][2])
                        forward_trajectory <- solve_flow(initial_state, fun, times = times_forward, xlim, ylim)
                        forward_trajectory$stream_moves <- iterations + seq_len(nrow(forward_trajectory))
                        forward_trajectory$check_p_in_this_order <- seq_len(nrow(forward_trajectory))
                        forward_trajectory$direction <- "forward"

                        if(nrow(forward_trajectory) == 1){
                          forward_trajectory <- NULL
                        }

                        combined_trajectory <- forward_trajectory

                        # reverse_trajectory <- solve_flow(initial_state, fun, times_backward, xlim, ylim) |>
                        #   mutate(stream_moves = n() - row_number()) |>
                        #   mutate(check_p_in_this_order = iterations + 1 + n() - row_number()) |>
                        #   mutate(direction = "reverse")
                        #
                        # if(nrow(reverse_trajectory) == 1){
                        #   reverse_trajectory <- NULL
                        # }
                        # # print(i)
                        # combined_trajectory <- rbind(forward_trajectory, reverse_trajectory)
                        combined_trajectory <- combined_trajectory[!duplicated(combined_trajectory[, c("x", "y")]), ]

                        if (!is.null(combined_trajectory) && nrow(combined_trajectory) > 0) {
                          combined_trajectory <- combined_trajectory[order(combined_trajectory$stream_moves), ]
                          combined_trajectory$stream_moves <- seq_len(nrow(combined_trajectory))
                        } else {
                          combined_trajectory <- NULL
                        }

                        combined_trajectory
                      })

                      # Combine all forward trajectories into a single data frame, filtering out empty ones
                      trajectory_data <- do.call(rbind, lapply(seq_along(forward_trajectories), function(i) {
                        traj <- forward_trajectories[[i]]
                        if (!is.null(traj) > 0) {
                          data.frame(t = traj$time, x = traj$x, y = traj$y, id = i)
                        } else {
                          NULL
                        }
                      }))

                      # new_coords <- t(apply(trajectory_data, 1, function(row) {
                      #   v <- c(row['x'], row['y'])
                      #   result <- f(as.numeric(v))
                      #   return(result)
                      # }))
                      #
                      # new_coords_df <- data.frame(xnew = new_coords[, 1], ynew = new_coords[, 2])
                      #
                      # trajectory_data <-
                      #   cbind(trajectory_data, new_coords_df)

                      # Add a rownum column within each trajectory group (id)
                      trajectory_data$rownum <- ave(trajectory_data$x, trajectory_data$id, FUN = seq_along)

                      # Calculate default threshold_distance if not provided
                      if (is.null(threshold_distance)) {
                        dx <- (xlim[2] - xlim[1]) / n[1]
                        dy <- (ylim[2] - ylim[1]) / n[2]
                        dc <- sqrt(dx^2 + dy^2)
                        threshold_distance <- dc / 2
                      }

                      # Proximity Check
                      for (id in unique(trajectory_data$id)[-1]) {  # Skip the first id since it has no previous trajectories
                        current_traj <- trajectory_data[trajectory_data$id == id, ]
                        previous_trajs <- trajectory_data[trajectory_data$id < id, ]

                        truncation_occurred <- FALSE

                        # Start from the first point and check along the trajectory
                        for (k in seq_along(current_traj$x)) {
                          distances <- sqrt((current_traj$x[k] - previous_trajs$x)^2 + (current_traj$y[k] - previous_trajs$y)^2)

                          # If any point is within the threshold_distance, delete the rest of the points
                          if (any(distances < threshold_distance)) {
                            trajectory_data <- trajectory_data[!(trajectory_data$id == id & trajectory_data$rownum >= current_traj$rownum[k]), ]
                            truncation_occurred <- TRUE
                            break  # Stop checking further points in this trajectory
                          }
                        }

                        # Remove trajectories that end up with only one point after truncation
                        remaining_points <- trajectory_data[trajectory_data$id == id, ]
                        if (nrow(remaining_points) <= 1) {
                          trajectory_data <- trajectory_data[trajectory_data$id != id, ]
                        }
                      }



                      # Prepare for animation
                      trajectory_data <- trajectory_data[order(trajectory_data$id), ]
                      trajectory_data$rownum <- ave(trajectory_data$id, trajectory_data$id, FUN = seq_along) |> as.integer()

                      # Calculus measures
                      grad <- apply(cbind(trajectory_data$x, trajectory_data$y), 1, numDeriv::grad, func = fun) |> t()

                      grad_u <- grad[, 1]
                      grad_v <- grad[, 2]

                      # Divergence
                      trajectory_data$divergence <- grad_u + grad_v

                      # Curl
                      trajectory_data$curl <- grad_v - grad_u

                      # trajectory_data <- trajectory_data |> filter(id ==61)

                      return(trajectory_data)
                    }
)
