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
#' @param max_length A numeric value specifying the maximum length of
#'   streamlines.
#' @param max_steps An integer specifying the maximum number of steps for
#'   streamline generation.
#' @param ds A numeric value specifying the distance between steps for
#'   streamline generation.
#' @param min_dist A numeric value specifying the minimum distance between
#'   streamlines.
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
#' x <- v[1]
#' y <- v[2]
#' c(-1 - x^2 + y, 1 + x - y^2)
#' }
#'
#' # Create a ggplot with the stream plot layer
#' ggplot() +
#'   geom_streamplot(aes(group = after_stat(id)),
#'                   fun = f, xlim = c(-3, 3), ylim = c(-3, 3), max_length = 10000,
#'                   max_steps = 10000, ds = .05, min_dist = .25) +
#'   coord_fixed() +
#'   theme_minimal()
#'
NULL

#' @rdname geom_streamplot
#' @export

geom_streamplot <- function(mapping = NULL, data = NULL,
                            stat = "streamplot", position = "identity",
                            na.rm = FALSE, show.legend = TRUE, inherit.aes = TRUE,
                            fun, xlim, ylim, max_length, max_steps,
                            ds, min_dist,
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
      max_length = max_length,
      max_steps = max_steps,
      ds = ds,
      min_dist = min_dist,
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

stat_streamline <- function(mapping = NULL, data = NULL,
                              stat = "streamplot", position = "identity",
                              na.rm = FALSE, show.legend = TRUE, inherit.aes = TRUE,
                              fun, xlim, ylim, max_length, max_steps,
                              ds, min_dist, arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
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
      max_length = max_length,
      max_steps = max_steps,
      ds = ds,
      min_dist = min_dist,
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
  compute_group = function(data, scales, fun, xlim, ylim, max_length, max_steps, ds, min_dist) {

    trajectories <- streamplot(fun, xlim, ylim, max_length, max_steps, ds, min_dist)

    trajectory_data <- do.call(rbind, lapply(seq_along(trajectories), function(i) {
      traj <- trajectories[[i]]
      data.frame(x = sapply(traj, `[[`, 1), y = sapply(traj, `[[`, 2), id = i)
    }))

    trajectory_data$norm <- sqrt(trajectory_data$x^2 + trajectory_data$y^2)

    for (current_id in unique(trajectory_data$id)) {
      # Subset data for the current id
      traj <- subset(trajectory_data, id == current_id)

      # Calculate new id2 values
      new_id2 <- paste(current_id, (seq_along(traj$id) - 1) %/% 10, sep = "_")

      # Assign the new id2 values back to the original data
      trajectory_data$id2[trajectory_data$id == current_id] <- new_id2
    }

    return(trajectory_data)
  }
)
