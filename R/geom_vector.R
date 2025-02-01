#' geom_vector
#'
#' A convenient ggplot2 layer for drawing vectors. This function accepts
#' wide-format data with the aesthetics `x` and `y` plus either (`xend`, `yend`)
#' or (`angle`, `distance`). If the latter is supplied, the endpoints are computed
#' as a translation of the starting point using polar coordinates (assuming
#' the angle is in degrees). The data is then converted into long format (two rows
#' per vector) using [StatVector] and plotted using [GeomStream]. Directional
#' arrowheads can be added to indicate the vector direction.
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams geom_stream
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()]. **Required:**
#'   `x` and `y` must always be provided. In addition, either `xend` and `yend` or
#'   `angle` and `distance` must be supplied. No grouping (or id) is needed.
#' @param data A data frame containing the vector data in wide format.
#' @param stat The statistical transformation to use on the data for this layer.
#'   Defaults to [StatVector].
#' @param position Position adjustment, either as a string or the result of a call
#'   to a position adjustment function.
#' @param na.rm Logical. If `FALSE` (the default), missing values are removed with a warning.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather than combining with them.
#' @param arrow An optional [grid::arrow()] specification to add arrowheads to the vectors
#'   (default: `grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")`).
#' @param center Logical. If `TRUE`, the vector is recentered so that the original
#'   (x, y) becomes the midpoint of the vector. Default is `FALSE`.
#' @param ... Other arguments passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer that can be added to a plot.
#'
#' @examples
#' vectors1 <- data.frame(
#'   x    = c(0, 1, 2),
#'   y    = c(0, 1, 2),
#'   xend = c(3, 1, 5),
#'   yend = c(0, 5, 6)
#' )
#' ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_vector(center = TRUE)
#'
#' vectors2 <- data.frame(
#'   x        = c(0, 1, 2),
#'   y        = c(0, 1, 2),
#'   angle    = c(0, 90, 45),
#'   distance = c(3, 4, 5)
#' )
#' ggplot(vectors2, aes(x = x, y = y, angle = angle, distance = distance)) +
#'   geom_vector(center = TRUE)
#'
#' @export
geom_vector <- function(mapping = NULL, data = NULL,
                        stat = StatVector,
                        position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        center = FALSE,
                        arrow = grid::arrow(angle = 25,
                                            length = unit(0.025, "npc"),
                                            type = "closed")) {

  layer(
    stat = stat,
    geom = GeomStream,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      center = center,
      ...
    )
  )
}

#' @rdname geom_vector
#' @export
stat_vector <- function(mapping = NULL, data = NULL,
                        geom = GeomStream,
                        position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        center = FALSE,
                        arrow = grid::arrow(angle = 25,
                                            length = unit(0.025, "npc"),
                                            type = "closed")) {

  layer(
    stat = StatVector,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      center = center,
      ...
    )
  )
}

#' @rdname geom_vector
#' @export
StatVector <- ggproto("StatVector", Stat,
                      required_aes = c("x", "y"),
                      default_params = list(center = FALSE),
                      compute_group = function(data, scales, center, ...) {

                        n <- nrow(data)

                        if(n == 0) return(data)

                        if((!"xend" %in% names(data) || all(is.na(data$xend))) ||
                           (!"yend" %in% names(data) || all(is.na(data$yend)))) {
                          if("angle" %in% names(data) && "distance" %in% names(data)) {
                            data$xend <- data$x + data$distance * cos(data$angle * pi/180)
                            data$yend <- data$y + data$distance * sin(data$angle * pi/180)
                          } else {
                            stop("Either xend/yend or angle/distance must be provided.")
                          }
                        }

                        if(center) {
                          print(data)
                          xdiff <- data$xend - data$x
                          ydiff <- data$yend - data$y
                          data$x <- data$x - (xdiff / 2)
                          data$y <- data$y - (ydiff / 2)
                          data$xend <- data$x + xdiff
                          data$yend <- data$y + ydiff
                          print(data)
                        }

                        data_start <- data
                        data_start$t <- 0
                        data_end <- data
                        data_end$t <- 1
                        data_end$x <- data_end$xend
                        data_end$y <- data_end$yend
                        data_start$xend <- NA_real_
                        data_start$yend <- NA_real_
                        data_end$xend <- NA_real_
                        data_end$yend <- NA_real_
                        if("angle" %in% names(data_start)) data_start$angle <- NA_real_
                        if("distance" %in% names(data_start)) data_start$distance <- NA_real_
                        if("angle" %in% names(data_end)) data_end$angle <- NA_real_
                        if("distance" %in% names(data_end)) data_end$distance <- NA_real_
                        combined <- rbind(data_start, data_end)
                        interleaved <- combined[c(rbind(seq_len(n), seq_len(n) + n)), ]
                        rownames(interleaved) <- NULL
                        interleaved$group <- rep(seq_len(n), each = 2)
                        print(interleaved)
                        interleaved
                      }
)
