#' Create a Vector Field Plot Layer
#'
#' [geom_vector_field()] generates a vector field plot layer using a
#' user-defined function to compute the vector components. This is particularly
#' useful for visualizing vector fields in a two-dimensional space.
#' @inheritParams ggplot2::geom_raster
#' @inheritParams ggplot2::stat_identity
#' @param fun A user-defined function that takes two arguments (x and y
#'   coordinates) and returns a list of two components: the x and y components
#'   of the vector field.
#' @param xlim,ylim A numeric vector of length 2 giving the x-axis limits.
#' @param n An integer specifying the number of grid points along each axis.
#' @return A ggplot2 layer that can be added to a ggplot object to produce a
#'   vector field plot.
#' @name geom_vector_field
#' @rdname geom_vector_field
#' @examples
#'
#' # example user-defined function
#' f <- function(x, y) {
#'   u <- -y
#'   v <- x
#'   list(u, v)
#' }
#'
#' # Create a ggplot with the vector field layer
#' ggplot() +
#'   geom_vector_field(fun = f, xlim = c(-10, 10), ylim = c(-10, 10), n = 20, arrow = arrow(length = unit(1, "mm")))
#'
NULL


#' @rdname geom_vector_field
#' @export
geom_vector_field <- function(mapping = NULL, data = NULL,
                              stat = "vectorfield",
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE,
                              fun, xlim, ylim, n = 10, ...) {

  if (is.null(data)) data <- ensure_nonempty_data(data)

  layer(
    stat = StatVectorField,
    data = data,
    mapping = mapping,
    geom = GeomVectorField,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_vector_field
#' @format NULL
#' @usage NULL
#' @export
GeomVectorField <- ggproto("GeomVectorField", GeomSegment)

#' @rdname geom_vector_field
#' @export
stat_vector_field <- function(mapping = NULL, data = NULL, geom = "segment",
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE,
                              fun, xlim, ylim, n = 10, ...) {

  if (is.null(data)) data <- ensure_nonempty_data

  layer(
    stat = StatVectorField,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}


#' @rdname geom_vector_field
#' @format NULL
#' @usage NULL
#' @export
StatVectorField <- ggproto("StatVectorField", Stat,

  # setup_data = cpt_data,

  # required_aes = c("x","y"),

  compute_group = function(data, scales, fun, xlim, ylim, n = n) {
    # Create a sequence of x and y values within the limits
    x_seq <- seq(xlim[1], xlim[2], length.out = n)
    y_seq <- seq(ylim[1], ylim[2], length.out = n)
    grid <- expand.grid(x = x_seq, y = y_seq)

    # Evaluate the function to get vector components
    vectors <- fun(grid$x, grid$y)

    # Create a data frame for geom_segment
    data <- data.frame(
      x = grid$x,
      y = grid$y,
      u = vectors[[1]],
      v = vectors[[2]]
    )

    # Calculate magnitude
    magnitude <- sqrt(data$u^2 + data$v^2)

    # Normalize the vectors
    data$u_norm <- data$u / magnitude
    data$v_norm <- data$v / magnitude

    # Calculate the end points of the vectors
    data$xend <- data$x + data$u_norm
    data$yend <- data$y + data$v_norm

    data
  }

)



