
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
stat_vector_field <- function(mapping = NULL, data = NULL, geom = "segment",
                              position = "identity", ..., fun = NULL,
                              scaling_factor = 1, na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE) {
  layer(
    stat = StatVectorField,
    mapping = mapping,
    data = data,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, fun = fun, scaling_factor = scaling_factor, ...)
  )
}


StatVectorField <-
  ggproto("StatVectorField", Stat,
          required_aes = c("x", "y"),

          compute_group = function(data, scales, fun = NULL, scaling_factor = 1,...) {
            if (is.null(fun)) {
              stop("A transformation function must be provided")
            }

            transformed <- fun(data$x, data$y)

            data <-
              data.frame(x = data$x,
                         y = data$y,
                         u = transformed[[1]],
                         v = transformed[[2]]
              ) |>
              mutate(magnitude = sqrt(u^2 + v^2)) |>
              mutate(u_norm = u / magnitude, v_norm = v / magnitude) |>
              mutate(xend = x + u_norm * scaling_factor, yend = y + v_norm * scaling_factor)

            return(data)
          }
  )
