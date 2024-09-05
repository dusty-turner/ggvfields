#' Create a Stream Plot Geom Layer
#'
#' [geom_streamplot()] generates a stream plot layer of a user-defined vector
#' field function. The lines in the plot represent the flow of data points
#' through the vector field.
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#' @param fun A user-defined function that takes a numeric vector as input and
#'   returns a numeric vector of the same length.
#' @param xlim A numeric vector of length 2 giving the x-axis limits.
#' @param ylim A numeric vector of length 2 giving the y-axis limits.
#' @param n A numeric vector of length 2 specifying the grid dimensions.
#' @param iterations An integer specifying the number of steps to perform for
#'   the numerical integration of the flow. The higher the number, the smoother
#'   and more detailed the resulting streamlines, but also the higher the computational cost.
#' @param chop A logical value indicating whether to chop the trajectories into
#'   segments.
#' @param scale_stream A numeric value specifying the maximum allowable segment
#'   size as a fraction of the total range for chopping the trajectories.
#' @param mask_shape_type A character string specifying the mask shape type:
#'   "square", "diamond", "inset_square", or "circle".
#' @param arrow Arrow specification, as created by `grid::arrow()`.
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
#'     iterations = 100,
#'     chop = TRUE, scale_stream = 1,
#'     mask_shape_type = "square"
#'   ) +
#'   coord_fixed() +
#'   theme_minimal()
#'
NULL

#' @rdname geom_streamplot
#' @export
geom_streamplot <- function(mapping = NULL, data = NULL,
                            stat = "streamplot", position = "identity",
                            na.rm = FALSE, show.legend = TRUE, inherit.aes = TRUE,
                            fun, xlim = c(-10,10), ylim = c(-10,10), n = c(21, 21),
                            mask_shape_type = "square", iterations = 100,
                            arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                            chop = TRUE, scale_stream = 1, ...) {
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
      mask_shape_type = mask_shape_type,
      iterations = iterations,
      arrow = arrow,
      chop = chop,
      scale_stream = scale_stream,
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
                            mask_shape_type = "square", iterations = 100,
                            arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                            chop = TRUE, scale_stream = 1, ...) {

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
      mask_shape_type = mask_shape_type,
      iterations = iterations,
      arrow = arrow,
      chop = chop,
      scale_stream = scale_stream,
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

                          default_aes = aes(group = after_stat(super_id), rownum = after_stat(rownum)),

                          compute_group = function(data, scales, fun, xlim, ylim, n, chop, scale_stream, mask_shape_type, iterations) {

                            n <- ensure_length_two(n)

                            # Initialize the mask
                            mask_shape <- c(n[2], n[1])
                            mask <- matrix(0, nrow = mask_shape[1], ncol = mask_shape[2])

                            starting_points <- generate_starting_points(mask_shape = n)

                            # Convert starting points to actual coordinates
                            x_seq <- seq(xlim[1], xlim[2], length.out = n[2])
                            y_seq <- seq(ylim[1], ylim[2], length.out = n[1])
                            grid_coords <- lapply(starting_points, function(p) c(x_seq[p[1]], y_seq[p[2]]))

                            times_forward <- seq(0, 1, length.out = iterations)  # Time sequence for ODE solver
                            times_backward <- seq(0, -1, length.out = iterations)  # Time sequence for ODE solver

                            trajectories <- list()

                            # for (i in 1:9) {
                            for (i in seq_along(grid_coords)) {

                              initial_state <- c(x = grid_coords[[i]][1], y = grid_coords[[i]][2])

                              # Forward Trajectory
                              forward_trajectory <- solve_flow(initial_state, fun, times = times_forward, xlim, ylim)
                              if (nrow(forward_trajectory) > 1) {
                                forward_trajectory$stream_moves <- iterations + seq_len(nrow(forward_trajectory))
                                forward_trajectory$check_p_in_this_order <- seq_len(nrow(forward_trajectory))
                                forward_trajectory$direction <- "forward"
                                forward_trajectory$id <- i
                              } else {
                                forward_trajectory <- NULL
                              }

                              # Reverse Trajectory
                              reverse_trajectory <- solve_flow(initial_state, fun, times = times_backward, xlim, ylim)
                              if (nrow(reverse_trajectory) > 1) {
                                reverse_trajectory$stream_moves <- iterations + rev(seq_len(nrow(reverse_trajectory)))
                                reverse_trajectory$check_p_in_this_order <- iterations + 1 + rev(seq_len(nrow(reverse_trajectory)))
                                reverse_trajectory$direction <- "reverse"
                                reverse_trajectory$id <- i
                              } else {
                                reverse_trajectory <- NULL
                              }

                              # Combine forward and reverse trajectories
                              combined_trajectory <- rbind(forward_trajectory, reverse_trajectory)

                              if (!is.null(combined_trajectory) && nrow(combined_trajectory) > 0) {
                                combined_trajectory <- combined_trajectory[!duplicated(combined_trajectory[, c("x", "y")]), ]

                                # Reorder by stream_moves and reset stream_moves to be sequential
                                combined_trajectory <- combined_trajectory[order(combined_trajectory$time), ]
                                combined_trajectory$stream_moves <- seq_len(nrow(combined_trajectory))

                                # Initialize the initial streamline id and segment id (id2)
                                current_id2 <- 1

                                # Create vectors to hold the ids and segment ids for each point in the trajectory
                                ids <- integer(nrow(combined_trajectory))
                                id2s <- integer(nrow(combined_trajectory))  # For storing the segment id (id2)
                                super_ids <- character(nrow(combined_trajectory))  # For storing super_id

                                # First loop: Delete rows in combined_trajectory that are already in the mask
                                valid_rows <- logical(nrow(combined_trajectory))  # To track valid points

                                for (j in seq_len(nrow(combined_trajectory))) {
                                  indices <- calculate_indices(combined_trajectory$x[j], combined_trajectory$y[j], xlim, ylim, n)
                                  xi <- indices[1]
                                  yi <- indices[2]

                                  # Check if the trajectory point hits a masked area
                                  if (!is_too_close_square(xi, yi, n, mask)) {
                                    valid_rows[j] <- TRUE  # Mark this point as valid if it's not too close
                                    ids[j] <- combined_trajectory$id[j]  # Maintain the original id for this segment
                                    id2s[j] <- current_id2  # Assign the current segment id (id2)

                                    # Create super_id by combining id and id2
                                    super_ids[j] <- paste0(ids[j], "-", id2s[j])
                                  } else {
                                    # If we encounter a masked area, start a new segment with a new id2
                                    current_id2 <- current_id2 + 1
                                  }
                                }

                                # Remove invalid rows from the trajectory
                                combined_trajectory <- combined_trajectory[valid_rows, ]
                                ids <- ids[valid_rows]  # Keep only valid ids
                                id2s <- id2s[valid_rows]  # Keep only valid id2s
                                super_ids <- super_ids[valid_rows]  # Keep only valid super_ids

                                # Assign the updated ids, id2s, and super_ids to the trajectory data
                                combined_trajectory$id2 <- id2s
                                combined_trajectory$super_id <- super_ids

                                # Second loop: Now update the mask with the remaining valid points of the trajectory
                                for (j in seq_len(nrow(combined_trajectory))) {
                                  indices <- calculate_indices(combined_trajectory$x[j], combined_trajectory$y[j], xlim, ylim, n)
                                  xi <- indices[1]
                                  yi <- indices[2]

                                  # Update the mask at the valid point
                                  mask <- update_mask_square(mask, xi, yi, n)
                                }

                                # Store the trajectory along with the streamline id
                                trajectories[[i]] <- combined_trajectory
                              }
                            }

                            # Remove NULL values from the list
                            valid_trajectories <- Filter(Negate(is.null), trajectories)

                            # Combine all valid trajectories
                            trajectory_data <- do.call(rbind, lapply(seq_along(valid_trajectories), function(i) {
                              traj <- valid_trajectories[[i]]
                              data.frame(
                                time = traj$time,
                                x = traj$x,
                                y = traj$y,
                                stream_moves = traj$stream_moves,
                                check_p_in_this_order = traj$check_p_in_this_order,
                                direction = traj$direction,
                                id = traj$id,         # Keep the original streamline id
                                id2 = traj$id2,       # Add the segment id (id2)
                                super_id = traj$super_id  # Add the combined super_id (id-id2)
                              )
                            }))

                            # Calculate the norm (magnitude of the flow at each point)
                            trajectory_data$norm <- sqrt(trajectory_data$x^2 + trajectory_data$y^2)

                            # Sort trajectory data by id and create row number for each trajectory
                            trajectory_data <- trajectory_data[order(trajectory_data$id), ]
                            trajectory_data$rownum <- ave(trajectory_data$id, trajectory_data$id, FUN = seq_along) |> as.integer()

                            # Return the final trajectory data
                            return(trajectory_data)
                          }
)
