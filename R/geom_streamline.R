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
#'   the numerical integration of the flow.
#' @param chop A logical value indicating whether to chop the trajectories into
#'   segments.
#' @param scale_stream A numeric value specifying the maximum allowable segment
#'   size as a fraction of the total range for chopping the trajectories.
#' @param mask_shape_type A character string specifying the mask shape type:
#'   "square", "diamond", "inset_square", or "circle".
#' @param stream_density A numeric value that controls the density of the streamlines.
#'   Higher values produce more streamlines. Default is 1.
#' @param arrow Arrow specification, as created by `grid::arrow()`.
#' @param method A character string specifying the ODE solver method to be used
#'   (passed to [deSolve::ode()]). Default is "rk4".
#'
#' @name geom_streamplot
#' @rdname geom_streamplot
#'
#' @return A ggplot2 layer that can be added to a ggplot object to produce a
#'   stream plot.
#' @export
#' @examples
#'
#' # Example user-defined function
#' f <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(-1 - x^2 + y, 1 + x - y^2)
#' }
#'
#' ggplot() +
#'   geom_streamplot(
#'     fun = f, xlim = c(-3, 3), ylim = c(-3, 3), n = c(15, 15),
#'     iterations = 100, chop = TRUE, scale_stream = 1,
#'     mask_shape_type = "square", method = "rk4"
#'   ) +
#'   coord_fixed() +
#'   theme_minimal()
NULL


#' @rdname geom_streamplot
#' @export
# Geom function to add the stream plot layer
geom_streamplot <- function(mapping = NULL, data = NULL,
                            stat = "streamplot", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = TRUE,
                            inherit.aes = TRUE,
                            fun,
                            xlim = c(-1, 1),
                            ylim = c(-1, 1),
                            n = 11,
                            mask_shape_type = "square",
                            iterations = 100,
                            stream_density = 1,
                            method = "rk4",
                            arrow = grid::arrow(angle = 20,
                                                length = unit(0.015, "npc"),
                                                type = "closed"),
                            chop = TRUE,
                            scale_stream = 1) {

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
      method = method,
      mask_shape_type = mask_shape_type,
      iterations = iterations,
      stream_density = stream_density,
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
# Geom prototype for stream plot
GeomStreamplot <- ggproto("GeomStreamplot", GeomPath)

#' @rdname geom_streamplot
#' @export
# Stat function for stream plot computation
stat_streamplot <- function(mapping = NULL, data = NULL,
                            stat = "streamplot", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = TRUE,
                            inherit.aes = TRUE,
                            fun,
                            xlim = c(-1, 1),
                            ylim = c(-1, 1),
                            n = 11,
                            mask_shape_type = "square",
                            iterations = 100,
                            stream_density = 1,
                            method = "rk4",
                            arrow = grid::arrow(angle = 20,
                                                length = unit(0.015, "npc"),
                                                type = "closed"),
                            chop = TRUE,
                            scale_stream = 1) {


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
      method = method,
      mask_shape_type = mask_shape_type,
      iterations = iterations,
      stream_density = stream_density,
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
# Define the StatStreamplot object and computation for stream plots
StatStreamplot <- ggproto("StatStreamplot", Stat,

  default_aes = aes(group = after_stat(super_id), rownum = after_stat(rownum)),

  compute_group = function(data, scales, fun, xlim, ylim, n, chop, scale_stream, stream_density, mask_shape_type, iterations, method) {

    stream_density <- floor(stream_density)

    n <- ensure_length_two(n)

    # Initialize mask
    mask_shape <- c(n[2]*stream_density, n[1]*stream_density)
    mask <- matrix(0, nrow = mask_shape[1], ncol = mask_shape[2])

    # Generate starting points and convert them to coordinates
    starting_points <- generate_starting_points(mask_shape = n)
    x_seq <- seq(xlim[1], xlim[2], length.out = n[2])
    y_seq <- seq(ylim[1], ylim[2], length.out = n[1])
    grid_coords <- lapply(starting_points, function(p) c(x_seq[p[1]], y_seq[p[2]]))

    times_forward <- seq(0, 1, length.out = iterations)
    times_backward <- seq(0, -1, length.out = iterations)

    trajectories <- list()

    # Loop through each starting point
    for (i in seq_along(grid_coords)) {
      initial_state <- c(x = grid_coords[[i]][1], y = grid_coords[[i]][2])

      # Forward trajectory
      forward_trajectory <- solve_flow(initial_state, fun, times = times_forward, xlim, ylim, method)
      if (nrow(forward_trajectory) > 1) {
        forward_trajectory$stream_moves <- iterations + seq_len(nrow(forward_trajectory))
        forward_trajectory$check_p_in_this_order <- seq_len(nrow(forward_trajectory))
        forward_trajectory$direction <- "forward"
        forward_trajectory$id <- i
      } else {
        forward_trajectory <- NULL
      }

      # Reverse trajectory
      reverse_trajectory <- solve_flow(initial_state, fun, times = times_backward, xlim, ylim, method)
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
        combined_trajectory <- combined_trajectory[order(combined_trajectory$time), ]
        combined_trajectory$stream_moves <- seq_len(nrow(combined_trajectory))

        # Track trajectory ids
        current_id2 <- 1
        ids <- integer(nrow(combined_trajectory))
        id2s <- integer(nrow(combined_trajectory))
        super_ids <- character(nrow(combined_trajectory))

        # First pass: remove rows in masked areas
        valid_rows <- logical(nrow(combined_trajectory))
        for (j in seq_len(nrow(combined_trajectory))) {
          indices <- calculate_indices(combined_trajectory$x[j], combined_trajectory$y[j], xlim, ylim, mask_shape)
          xi <- indices[1]
          yi <- indices[2]

          # Check for mask overlap
          if (!is_masked_area(xi, yi, mask_shape, mask)) {
            valid_rows[j] <- TRUE
            ids[j] <- combined_trajectory$id[j]
            id2s[j] <- current_id2
            super_ids[j] <- paste0(ids[j], "-", id2s[j])
          } else {
            current_id2 <- current_id2 + 1
          }
        }

        combined_trajectory <- combined_trajectory[valid_rows, ]
        ids <- ids[valid_rows]
        id2s <- id2s[valid_rows]
        super_ids <- super_ids[valid_rows]
        combined_trajectory$id2 <- id2s
        combined_trajectory$super_id <- super_ids

        # Second pass: update mask for valid points
        for (j in seq_len(nrow(combined_trajectory))) {
          xi <- combined_trajectory$x[j]
          yi <- combined_trajectory$y[j]

          if (mask_shape_type == "square") {
            mask <- update_mask_square(mask, xi, yi, xlim, ylim, mask_shape)
          } else if (mask_shape_type == "diamond") {
            mask <- update_mask_diamond(mask, xi, yi, xlim, ylim, mask_shape)
          } else if (mask_shape_type == "inset_square") {
            mask <- update_mask_inset_square(mask, xi, yi, xlim, ylim, mask_shape)
          } else if (mask_shape_type == "circle") {
            mask <- update_mask_circle(mask, xi, yi, xlim, ylim, mask_shape)
          }
        }
        # print(mask)

        # Store trajectory
        trajectories[[i]] <- combined_trajectory
      }
    }

    # Remove null trajectories and combine valid ones
    valid_trajectories <- Filter(Negate(is.null), trajectories)
    trajectory_data <- do.call(rbind, lapply(seq_along(valid_trajectories), function(i) {
      traj <- valid_trajectories[[i]]
      data.frame(
        time = traj$time, x = traj$x, y = traj$y, stream_moves = traj$stream_moves,
        check_p_in_this_order = traj$check_p_in_this_order, direction = traj$direction,
        id = traj$id, id2 = traj$id2, super_id = traj$super_id
      )
    }))

    # If chop is enabled, calculate and divide segments by arclength
    if (chop) {
      dx <- (xlim[2] - xlim[1]) / n[1]
      dy <- (ylim[2] - ylim[1]) / n[2]
      dc <- sqrt(dx^2 + dy^2)
      divide_smaller_than <- dc * scale_stream

      # Calculate arclength for each point
      arclength <- numeric(nrow(trajectory_data))
      for (i in seq_along(unique(trajectory_data$super_id))) {
        id_indices <- which(trajectory_data$super_id == unique(trajectory_data$super_id)[i])
        arclength[id_indices] <- c(0, cumsum(sqrt(diff(trajectory_data$x[id_indices])^2 + diff(trajectory_data$y[id_indices])^2)))
      }
      trajectory_data$arclength <- arclength

      # Calculate total arclength for each id
      total_arclength <- tapply(trajectory_data$arclength, trajectory_data$super_id, max)
      trajectory_data$total_arclength <- total_arclength[as.character(trajectory_data$super_id)]

      # Segment super_id by arclength
      trajectory_data$super_id <- paste0(trajectory_data$super_id, "_", floor(trajectory_data$arclength / divide_smaller_than) + 1)
    }

    # Compute norm and return the result
    trajectory_data$norm <- sqrt(trajectory_data$x^2 + trajectory_data$y^2)
    trajectory_data <- trajectory_data[order(trajectory_data$id), ]
    trajectory_data$rownum <- ave(trajectory_data$id, trajectory_data$id, FUN = seq_along) |> as.integer()

    ## Calculus measures
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
