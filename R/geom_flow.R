#' Create a Flow Plot Geom Layer
#'
#' `geom_flow()` generates a flow plot layer based on a user-defined vector
#' field function. The lines in the plot represent the flow of data points
#' through the vector field, visualizing how vectors evolve over a grid of points.
#'
#' The flow paths are computed using the `deSolve` package's ODE solver,
#' with the `rk4` method (a fourth-order Runge-Kutta method) used by default for
#' numerical integration.
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#' @importFrom stats na.omit
#' @importFrom deSolve ode
#' @param fun A user-defined function that takes a numeric vector of length 2
#'   (representing the x and y coordinates) and returns a numeric vector of the
#'   same length, representing the vector field at that point.
#' @param xlim A numeric vector of length 2 specifying the x-axis limits for the plot.
#' @param ylim A numeric vector of length 2 specifying the y-axis limits for the plot.
#' @param n A numeric vector of length 2 specifying the grid dimensions (number of rows
#'   and columns) for the starting points of the flows.
#' @param iterations A numeric value specifying the number of time steps for the
#'   ODE solver used to generate flow paths. Higher values result in smoother
#'   and more detailed flows. If `NULL`, a default value is calculated based on the
#'   time span (`T`).
#' @param threshold_distance A numeric value specifying the minimum distance
#'   between adjacent flow lines to prevent overlap. If not provided, it defaults to
#'   half the Euclidean distance between adjacent grid points.
#' @param T A numeric value representing the total time span for the ODE solver
#'   to trace the flow. If `NULL`, it is automatically calculated based on the
#'   distance between the longest-separated points in the vector field.
#' @param arrow Arrow specification, as created by `grid::arrow()`, for adding
#'   arrows to the flow lines. Defaults to an arrow with angle 20, length 0.015
#'   of the plot, and closed type.
#' @name geom_flow
#' @rdname geom_flow
#'
#' @return A ggplot2 layer that adds flow lines to a ggplot object, visualizing
#'   the movement of points in the vector field.
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(grid) # For arrow()
#'
#' # Example vector field function
#' f <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(-1 - x^2 + y, 1 + x - y^2)
#' }
#'
#' # Create a flow plot
#' ggplot() +
#'   geom_flow(
#'     fun = f, n = c(21, 21),
#'     xlim = c(-10, 10), ylim = c(-10, 10),
#'     iterations = 100, threshold_distance = NULL,
#'     arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
#'   ) +
#'   coord_fixed() +
#'   theme_minimal()
NULL

#' @rdname geom_flow
#' @export
geom_flow <- function(mapping = NULL, data = NULL,
                      stat = "flow", position = "identity",
                      na.rm = FALSE, show.legend = TRUE, inherit.aes = TRUE,
                      fun, xlim = c(-10,10), ylim = c(-10,10), n = c(21, 21),
                      iterations = NULL, threshold_distance = NULL, T = NULL,
                      arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                      ...) {
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
      T = T,
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
                      na.rm = FALSE, show.legend = TRUE, inherit.aes = TRUE,
                      fun, xlim = c(-10,10), ylim = c(-10,10), n = c(21, 21),
                      iterations = NULL, threshold_distance = NULL, T = NULL,
                      arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                      ...) {

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
      T = T,
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
                    default_aes = aes(group = after_stat(id), rownum = after_stat(rownum), color = after_stat(t)),

                    compute_group = function(data, scales, fun, xlim, ylim, n, iterations = NULL, threshold_distance = NULL, T = NULL) {
                      # print(T)
                      # print(iterations)

                      n <- ensure_length_two(n)

                      starting_points <- generate_starting_points_flow(n, build_type = "layer")

                      if(is.null(threshold_distance)){
                        x_range <- abs(xlim[2] - xlim[1])/n[1]
                        y_range <- abs(ylim[2] - ylim[1])/n[2]
                        threshold_distance <- sqrt(x_range^2 + y_range^2) * .5
                      }
                      # print(threshold_distance)

                      if(is.null(T)){

                        mid_x <- jitter(mean(c(xlim[2], xlim[1])))
                        mid_y <- jitter(mean(c(ylim[2], ylim[1])))
                        initial_state <- c(x = mid_x, y = mid_y)

                        parameters <- list(fun = f, xlim = xlim, ylim = ylim)

                        times <- seq(0, threshold_distance, length.out = 1000)
                        result <- ode(y = initial_state, times = times, func = flow_ode, parms = parameters, method = "rk4") |> na.omit() |> as.data.frame()
                        dist_matrix <- dist(result[, c("x", "y")])
                        max_dist <- max(dist_matrix)
                        dist_full_matrix <- as.matrix(dist_matrix)
                        max_index <- which(dist_full_matrix == max_dist, arr.ind = TRUE)
                        point_1_index <- max_index[1, 1]
                        point_2_index <- max_index[1, 2]
                        time_1 <- result$time[point_1_index]
                        time_2 <- result$time[point_2_index]
                        T <- abs((time_1 - time_2)) |> ceiling()
                      }
                      if(is.null(iterations)){
                      iterations <- 1000*T |> floor()
                      }
                      # print(T)
                      # print(iterations)


                      # Convert the starting points to actual coordinates
                      x_seq <- seq(xlim[1], xlim[2], length.out = n[2])
                      y_seq <- seq(ylim[1], ylim[2], length.out = n[1])
                      grid_coords <- lapply(starting_points, function(p) c(x_seq[p[1]], y_seq[p[2]]))

                      times_forward <- seq(0, T, length.out = iterations)  # Time sequence for ODE solver
                      times_backward <- seq(0, -T, length.out = iterations)  # Time sequence for ODE solver

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
