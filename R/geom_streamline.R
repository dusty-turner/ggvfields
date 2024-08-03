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
#' @param ds A numeric value specifying the distance between steps for
#'   streamline generation.
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
#'     max_length = 10000, max_steps = 10000,
#'     ds = .05, chop = TRUE, scale_stream = 1,
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
                            fun, xlim, ylim, n = c(21, 21),
                            ds = .05, mask_shape_type = "square",
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
      ds = ds,
      mask_shape_type = mask_shape_type,
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
stat_streamline <- function(mapping = NULL, data = NULL,
                            stat = "streamplot", position = "identity",
                            na.rm = FALSE, show.legend = TRUE, inherit.aes = TRUE,
                            fun, xlim, ylim, n = c(21, 21),
                            ds = .05, mask_shape_type = "square",
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
      ds = ds,
      mask_shape_type = mask_shape_type,
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

                          default_aes = aes(group = after_stat(id)),

                          compute_group = function(data, scales, fun, xlim, ylim, n, ds, chop, scale_stream, mask_shape_type) {

                            n <- ensure_length_two(n)

                            trajectories <- streamplot(fun, xlim, ylim, n, ds, mask_shape_type)

                            trajectory_data <- do.call(rbind, lapply(seq_along(trajectories), function(i) {
                              traj <- trajectories[[i]]
                              data.frame(x = sapply(traj, `[[`, 1), y = sapply(traj, `[[`, 2), id = i)
                            }))

                            trajectory_data$norm <- sqrt(trajectory_data$x^2 + trajectory_data$y^2)

                            if(chop){

                              dx <- (xlim[2] - xlim[1]) / n[1]
                              dy <- (ylim[2] - ylim[1]) / n[2]
                              dc <- sqrt(dx^2 + dy^2)

                              # Convert scale_stream to an actual distance
                              divide_smaller_than <- dc * scale_stream

                              # Calculate arclength
                              arclength <- numeric(nrow(trajectory_data))

                              for(i in seq_along(unique(trajectory_data$id))) {
                                id_indices <- which(trajectory_data$id == unique(trajectory_data$id)[i])
                                arclength[id_indices] <- c(0, cumsum(sqrt(diff(trajectory_data$x[id_indices])^2 + diff(trajectory_data$y[id_indices])^2)))
                              }

                              trajectory_data$arclength <- arclength

                              # Calculate total arclength for each id
                              total_arclength <- tapply(trajectory_data$arclength, trajectory_data$id, max)

                              # Add total_arclength to trajectory_data
                              trajectory_data$total_arclength <- total_arclength[as.character(trajectory_data$id)]

                              # Create new id with segment information
                              trajectory_data$id <- paste0(trajectory_data$id, "_", floor(trajectory_data$arclength / divide_smaller_than) + 1)
                            }
                            return(trajectory_data)
                          }
)
