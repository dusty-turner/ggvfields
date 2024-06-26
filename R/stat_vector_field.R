
#' stat_vector_field
#'
#' @param mapping
#' @param data
#' @param geom
#' @param position
#' @param ...
#' @param fun
#' @param scaling_factor
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#'
#' @return
#' @export
#' @import ggplot2
#'
#' @examples

ensure_nonempty_data <- function(data) {
  if (is_empty(data)) {
    data.frame(x = 1)
  }
  else {
    data
  }
}

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
                             data <-
                               data.frame(x = grid$x,
                                          y = grid$y,
                                          u = vectors[[1]],
                                          v = vectors[[2]]
                               ) |>
                               mutate(magnitude = sqrt(u^2 + v^2)) |>
                               mutate(u_norm = u / magnitude, v_norm = v / magnitude) |>
                               mutate(xend = x + u_norm, yend = y + v_norm)

                             data
                           }

)
