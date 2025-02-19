#' Vector Layers for ggplot2
#'
#' These functions provide convenient ggplot2 layers for drawing vectors.
#'
#' They accept wide-format data with the aesthetics `x` and `y` plus either
#' (`xend`, `yend`) or (`angle`, `distance`). If the latter is supplied, the
#' endpoints are computed as a translation of the starting point using polar
#' coordinates (assuming the angle is in degrees). The data is then converted
#' into long format (two rows per vector) using [StatVector] and plotted using
#' [GeomStream]. Directional arrowheads can be added to indicate the vector
#' direction.
#'
#' Two variants are available:
#'
#' - **geom_vector()** creates the layer with the user-supplied aesthetic
#' mapping.
#' - **geom_vector2()** uses the same underlying stat ([StatVector]) but adds a
#' default mapping for `length = after_stat(norm)` so that the computed vector
#' norm is available as an aesthetic.
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams geom_stream
#'
#' @param mapping A set of aesthetic mappings created by [ggplot2::aes()].
#'   **Required:** `x` and `y` must always be provided. In addition, either
#'   `xend` and `yend` or `angle` and `distance` must be supplied.
#' @param data A data frame containing the vector data in wide format.
#' @param stat The statistical transformation to use on the data for this layer.
#'   Defaults to [StatVector].
#' @param position Position adjustment, either as a string or the result of a
#'   call to a position adjustment function.
#' @param na.rm Logical. If `FALSE` (the default), missing values are removed
#'   with a warning.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes Logical. If `FALSE`, overrides the default aesthetics
#'   rather than combining with them.
#' @param arrow An optional [grid::arrow()] specification to add arrowheads to
#'   the vectors (default: `grid::arrow(angle = 25, length = unit(0.025, "npc"),
#'   type = "closed")`).
#' @param center Logical. If `TRUE`, the vector is recentered so that the
#'   original (x, y) becomes the midpoint of the vector. Default is `FALSE`.
#' @param normalize Logical. If `TRUE`, the vector endpoints are adjusted so
#'   that each vector has unit length.
#' @param tail_point Logical. If `TRUE`, a point is drawn at the tail (i.e., the
#'   starting point) of each vector. This can help to indicate the origin of the
#'   vector. Default is `FALSE`.
#' @param eval_point Logical. If `TRUE`, a point is drawn at the evaluation
#'   point, corresponding to the original (untransformed) seed point before any
#'   centering or normalization is applied. This allows for comparison between
#'   the original and transformed positions. Default is `FALSE`.
#' @param L Numeric scalar. The desired length for the vectors (in data units).
#'   If `NULL` (the default), a default value is computed automatically based on
#'   the plot’s x and y limits.
#' @param ... Other arguments passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer that can be added to a plot.
#'
#' @examples
#' # Basic usage with explicit start and end points:
#' vectors1 <- data.frame(
#'   x    = c(0, 1, 2),
#'   y    = c(0, 1, 2),
#'   xend = c(3, 1, 5),
#'   yend = c(0, 5, 6)
#' )
#' ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_vector()
#'
#' # Basic usage with angle and distance:
#' vectors2 <- data.frame(
#'   x        = c(0, 1, 2),
#'   y        = c(0, 1, 2),
#'   angle    = c(0, pi/2, pi/4),
#'   distance = c(3, 4, 5)
#' )
#' ggplot(vectors2) +
#'   geom_vector(aes(x = x, y = y, angle = angle, distance = distance))
#'
#' # Using center = TRUE to adjust vectors so that they originate from their midpoints:
#' ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_vector(center = TRUE)
#'
#' # Using normalize = TRUE to adjust vectors to have unit length:
#' ggplot(vectors2, aes(x = x, y = y, angle = angle, distance = distance)) +
#'   geom_vector(normalize = TRUE)
#'
#' # Using geom_vector2, which adds a default mapping for length:
#' ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_vector2()
#'
#' @name geom_vector
#' @aliases geom_vector geom_vector2 stat_vector StatVector
#' @export
NULL

geom_vector <- function(
  mapping = NULL,
  data = NULL,
  stat = StatVector,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  center = TRUE,
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  L = NULL,
  arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")
  ) {

  default_mapping <- ggplot2::aes(color = after_stat(norm))

  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

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
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      L = L,
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
  center = TRUE,
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  L = NULL,
  arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")
  ) {

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
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      L = L,
      ...
    )
  )
}

#' @rdname geom_vector
#' @export
StatVector <- ggproto("StatVector", Stat,
  required_aes = c("x", "y"),

  default_aes = aes(xend = NA, yend = NA, distance = NA, angle = NA,
                    fx = NA, fy = NA),

  compute_group = function(data, scales, center, normalize, L,...) {
# browser()

    n <- nrow(data)

    data$x0 <- data$x
    data$y0 <- data$y

    # If xend/yend are not provided (or are all missing), try fx/fy, then angle/distance.
    if((!"xend" %in% names(data) || all(is.na(data$xend))) ||
       (!"yend" %in% names(data) || all(is.na(data$yend)))) {

      if("fx" %in% names(data) && "fy" %in% names(data) &&
         !(all(is.na(data$fx)) || all(is.na(data$fy)))) {
        # Use fx and fy to compute the endpoints.
        data$xend <- data$x + data$fx
        data$yend <- data$y + data$fy
      } else if("angle" %in% names(data) && "distance" %in% names(data) &&
                !(all(is.na(data$angle)) || all(is.na(data$distance)))) {
        # Use angle and distance to compute the endpoints.
        data$xend <- data$x + data$distance * cos(data$angle * 180 / pi)
        data$yend <- data$y + data$distance * sin(data$angle * 180 / pi)
      } else {
        stop("Either xend/yend or fx/fy or angle/distance must be provided.")
      }
    }

    data$norm <- sqrt((data$xend - data$x)^2 + (data$yend - data$y)^2)

    if (is.null(L)) {
      # use x/ylim if provided by a previous layer, otherwise base it on data
      xlim <- if (!is.null(scales$x$range$range)) scales$x$range$range else range(data$x, na.rm = TRUE)
      ylim <- if (!is.null(scales$y$range$range)) scales$y$range$range else range(data$y, na.rm = TRUE)

      # Set a default grid resolution (number of cells along the larger axis).
      # In geom_stream_field n is passed in; here we assume, say, 20.
      grid_n <- 20

      # Compute L similarly to geom_stream_field:
      L <- min(diff(xlim), diff(ylim)) / (grid_n - 1) * 0.85
    }

    # This normalizes the vector to 1 unit then scales it by 'L'
    if (normalize) {
      data$xend <- data$x + L * (data$xend - data$x) / data$norm
      data$yend <- data$y + L * (data$yend - data$y) / data$norm
    }

    if(center) {
      xdiff <- data$xend - data$x
      ydiff <- data$yend - data$y
      data$x <- data$x - xdiff / 2
      data$y <- data$y - ydiff / 2
      data$xend <- data$xend - xdiff / 2
      data$yend <- data$yend - ydiff / 2
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
    interleaved
  }
)


#' @rdname geom_vector
#' @export
geom_vector2 <- function(
   mapping = NULL,
   data = NULL,
   stat = StatVector,
   position = "identity",
   ...,
   na.rm = FALSE,
   show.legend = NA,
   inherit.aes = TRUE,
   center = FALSE,
   tail_point = TRUE,
   eval_point = FALSE,
   L = NULL,
   arrow = NULL) {

  default_mapping <- ggplot2::aes(length = after_stat(norm))

  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

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
      normalize = TRUE,
      tail_point = tail_point,
      eval_point = eval_point,
      L = L,
      ...
    )
  )
}

#' @rdname geom_vector
#' @export
stat_vector2 <- function(
   mapping = NULL,
   data = NULL,
   geom = GeomStream,
   position = "identity",
   ...,
   na.rm = FALSE,
   show.legend = NA,
   inherit.aes = TRUE,
   center = FALSE,
   tail_point = TRUE,
   eval_point = FALSE,
   L = NULL,
   arrow = NULL) {

  default_mapping <- ggplot2::aes(length = after_stat(norm))

  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

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
      normalize = TRUE,
      tail_point = tail_point,
      eval_point = eval_point,
      L = L,
      ...
    )
  )
}

