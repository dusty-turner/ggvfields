#' Create a Stream Plot Geom Layer
#'
#' `geom_streamplot()` generates a stream plot layer for a user-defined vector
#' field function. The lines in the plot represent the flow of data points
#' through the vector field.
#'
#' The streamlines are computed using the `deSolve` package's ODE solver. The
#' `rk4` method (a fourth-order Runge-Kutta method) is used by default for
#' numerical integration of the streamlines.
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#' @param fun A user-defined function that takes a numeric vector as input and
#'   returns a numeric vector of the same length, representing the vector field.
#' @param xlim A numeric vector of length 2 giving the x-axis limits.
#' @param ylim A numeric vector of length 2 giving the y-axis limits.
#' @param n A numeric vector of length 2 specifying the grid dimensions for the
#'   starting points of the streamlines.
#' @param iterations A numeric value specifying the number of time steps for the
#'   ODE solver used in streamline generation. This determines the granularity
#'   of the streamline paths; higher values result in smoother, more detailed
#'   streamlines.
#' @param chop A logical value indicating whether to chop the trajectories into
#'   segments based on their arclength.
#' @param scale_stream A numeric value specifying the maximum allowable segment
#'   size as a fraction of the total range for chopping the trajectories.
#' @param threshold_distance A numeric value specifying the minimum distance
#'   between streamlines to avoid overlap. If not provided, it defaults to half
#'   the Euclidean distance between adjacent grid points.
#' @param arrow Arrow specification, as created by `grid::arrow()`, for adding
#'   arrows to the streamlines.
#' @name geom_streamplot
#' @rdname geom_streamplot
#'
#' @return A ggplot2 layer that can be added to a ggplot object to produce a
#'   stream plot.
#' @export
#'
#' @examples
#' library(ggplot2)
#' # Example user-defined function
#' f <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(-1 - x^2 + y, 1 + x - y^2)
#' }
#'
#' # Create a ggplot with the stream plot layer
#' ggplot() +
#'   geom_streamplot(
#'     fun = f, xlim = c(-3, 3),
#'     ylim = c(-3, 3), n = c(15, 15),
#'     iterations = 100, chop = TRUE, scale_stream = 1,
#'     threshold_distance = NULL,  # Default value
#'     arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
#'   ) +
#'   coord_fixed() +
#'   theme_minimal()
NULL

#' @rdname geom_streamplot
#' @export
geom_streamplot <- function(mapping = NULL, data = NULL,
                            stat = "streamplot", position = "identity",
                            na.rm = FALSE, show.legend = TRUE, inherit.aes = TRUE,
                            fun, xlim = c(-10,10), ylim = c(-10,10), n = c(21, 21),
                            iterations = 100, chop = TRUE, scale_stream = 1, threshold_distance = .5,
                            arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                            ...) {
  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)
  layer(
    stat = StatStreamplot,
    data = data,
    mapping = mapping,
    geom = GeomStreamplot,
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
      chop = chop,
      scale_stream = scale_stream,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_streamplot
#' @format NULL
#' @usage NULL
#' @export
GeomStreamplot <- ggproto("GeomStreamplot", GeomPath)

#' @rdname geom_streamplot
#' @export
stat_streamplot <- function(mapping = NULL, data = NULL,
                            stat = "streamplot", position = "identity",
                            na.rm = FALSE, show.legend = TRUE, inherit.aes = TRUE,
                            fun, xlim = c(-10,10), ylim = c(-10,10), n = c(21, 21),
                            iterations = 100, chop = TRUE, scale_stream = 1, threshold_distance = .5,
                            arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                            ...) {

  if (is.null(data)) data <- ensure_nonempty_data(data)
  layer(
    stat = StatStreamplot,
    data = data,
    mapping = mapping,
    geom = GeomStreamplot,
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
      chop = chop,
      scale_stream = scale_stream,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_streamplot
#' @format NULL
#' @usage NULL
#' @export
StatStreamplot <- ggproto("StatStreamplot", Stat,
   default_aes = aes(group = after_stat(id), rownum = after_stat(rownum)),

   compute_group = function(data, scales, fun, xlim, ylim, n, iterations, chop, scale_stream, threshold_distance = NULL) {

     n <- ensure_length_two(n)

     starting_points <- generate_starting_points(n)

     # Convert the starting points to actual coordinates
     x_seq <- seq(xlim[1], xlim[2], length.out = n[2])
     y_seq <- seq(ylim[1], ylim[2], length.out = n[1])
     grid_coords <- lapply(starting_points, function(p) c(x_seq[p[1]], y_seq[p[2]]))

     times <- seq(0, 1, length.out = iterations)  # Time sequence for ODE solver

     # Generate all streamlines following the specified order
     trajectories <- lapply(seq_along(grid_coords), function(i) {
       initial_state <- c(x = grid_coords[[i]][1], y = grid_coords[[i]][2])
       solve_streamline(initial_state, fun, times, xlim, ylim)
     })

     # Combine all trajectories into a single data frame
     trajectory_data <- do.call(rbind, lapply(seq_along(trajectories), function(i) {
       traj <- trajectories[[i]]
       data.frame(x = traj$x, y = traj$y, id = i)
     }))

     # Remove trajectories with only one observation
     trajectory_data <- trajectory_data[ave(trajectory_data$id, trajectory_data$id, FUN = length) > 1, ]

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

       for (k in seq_along(current_traj$x)) {
         distances <- sqrt((current_traj$x[k] - previous_trajs$x)^2 + (current_traj$y[k] - previous_trajs$y)^2)

         if (any(distances < threshold_distance)) {
           # Truncate the current trajectory
           trajectory_data <- trajectory_data[!(trajectory_data$id == id & trajectory_data$rownum >= current_traj$rownum[k]), ]
           truncation_occurred <- TRUE
           break
         }
       }

       # Remove trajectories that end up with only one point after truncation
       remaining_points <- trajectory_data[trajectory_data$id == id, ]
       if (nrow(remaining_points) <= 1) {
         trajectory_data <- trajectory_data[trajectory_data$id != id, ]
       }

       if (truncation_occurred) {
         next
       }
     }

     # Chop up long streams
     if (chop) {
       dx <- (xlim[2] - xlim[1]) / n[1]
       dy <- (ylim[2] - ylim[1]) / n[2]
       dc <- sqrt(dx^2 + dy^2)

       # Convert scale_stream to an actual distance
       divide_smaller_than <- dc * scale_stream

       # Calculate arclength
       arclength <- numeric(nrow(trajectory_data))

       for (i in seq_along(unique(trajectory_data$id))) {
         id_indices <- which(trajectory_data$id == unique(trajectory_data$id)[i])
         arclength[id_indices] <- c(0, cumsum(sqrt(diff(trajectory_data$x[id_indices])^2 + diff(trajectory_data$y[id_indices])^2)))
       }

       trajectory_data$arclength <- arclength

       # Calculate total arclength for each id
       total_arclength <- tapply(trajectory_data$arclength, trajectory_data$id, max)
       trajectory_data$total_arclength <- total_arclength[as.character(trajectory_data$id)]

       # Create new id with segment information
       trajectory_data$id <- paste0(trajectory_data$id, "_", floor(trajectory_data$arclength / divide_smaller_than) + 1)
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

     return(trajectory_data)
   }
)

