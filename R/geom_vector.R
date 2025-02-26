#' Vector Layers for ggplot2
#'
#' Create layers for drawing vectors on ggplot2 plots. These functions accept
#' wide-format data with the required aesthetics **`x`** and **`y`** plus either
#' **`xend`** and **`yend`** or one of the alternative specifications: **`fx`**
#' and **`fy`**, or **`angle`**/ **`angle_deg`** and **`distance`**.
#'
#' When specifying the vector direction using polar coordinates, you can provide
#' either:
#'
#' - **`angle`**: the vector direction in **radians**.
#' - **`angle_deg`**: the vector direction in **degrees** (which is
#' automatically converted to radians).
#'
#' The endpoints are computed by translating the starting point using these
#' polar coordinates along with the supplied **`distance`**.
#'
#' The data is converted to long format (two rows per vector) via [StatVector]
#' and rendered with [GeomStream]. Optionally, arrowheads can be added to
#' indicate direction.
#'
#' There are two variants:
#'
#' - `geom_vector()`: Uses the user-supplied aesthetic mapping.
#' - `geom_vector2()`: Uses the same underlying stat ([StatVector]) but adds a
#' default mapping for `length = after_stat(norm)`, making the computed vector
#' norm available as an aesthetic.
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams geom_stream
#'
#' @param mapping A set of aesthetic mappings created by [ggplot2::aes()].
#'   **Required:** Must include **`x`** and **`y`**; in addition, either **`xend`**
#'   and **`yend`** or one of the alternative specifications (**`fx`**/ **`fy`**
#'   or **`angle`**/ **`angle_deg`** and **`distance`**) must be provided.
#' @param data A data frame containing the vector data in wide format.
#' @param stat The statistical transformation to use on the data (default:
#'   [StatVector]).
#' @param position Position adjustment, either as a string or the result of a
#'   position adjustment function.
#' @param na.rm Logical. If `FALSE` (the default), missing values are removed
#'   with a warning.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes Logical. If `FALSE`, overrides the default aesthetics
#'   rather than combining with them.
#' @param arrow An optional [grid::arrow()] specification to add arrowheads to
#'   the vectors (default: `grid::arrow(angle = 25, length = unit(0.025, "npc"),
#'   type = "closed")`).
#' @param center Logical. If `TRUE`, the vector is recentered so that the
#'   original `(x, y)` becomes the midpoint (default is `TRUE` for
#'   `geom_vector()` and `FALSE` for `geom_vector2()`).
#' @param normalize Logical. If `TRUE`, the vector endpoints are scaled to unit
#'   length before being scaled by `L` (default: `TRUE`).
#' @param tail_point Logical. If `TRUE`, a point is drawn at the tail (the
#'   starting point) of each vector (default is `FALSE` for `geom_vector()` and
#'   `TRUE` for `geom_vector2()`).
#' @param eval_point Logical. If `TRUE`, a point is drawn at the evaluation
#'   point corresponding to the original (untransformed) seed point before any
#'   centering or normalization (default: `FALSE`).
#' @param L Numeric scalar. The desired length for the vectors in data units. If
#'   `NULL` (the default), a value is computed automatically based on the plot’s
#'   x and y limits.
#' @param ... Other arguments passed on to [ggplot2::layer()].
#'
#' @section Aesthetics:
#'
#'   `geom_vector()` and `geom_vector2()` understand the following aesthetics
#'   (required aesthetics are in bold):
#'
#'   - **x**
#'   - **y**
#'   - xend
#'   - yend
#'   - fx (alternative specification)
#'   - fy (alternative specification)
#'   - angle (vector direction in radians; alternative specification)
#'   - angle_deg (vector direction in degrees; alternative specification, converted to radians)
#'   - distance (with angle/angle_deg, used to compute endpoints)
#'   - alpha
#'   - color
#'   - fill
#'   - group
#'   - linetype
#'   - size
#'
#' @section Computed Variables:
#'
#' These are calculated by the 'stat' part of layers and can be accessed with
#'   delayed evaluation.
#'
#' \describe{
#'   \item{norm}{Calculated as the Euclidean distance between the starting point
#'     (\code{x}, \code{y}) and the computed endpoint (\code{xend}, \code{yend}). This
#'     value is used to normalize the vector length when the \code{normalize} parameter
#'     is set to \code{TRUE}.}
#' }
#'
#' @return A ggplot2 layer that can be added to a plot.
#'
#' @examples
#' set.seed(1234)
#' n <- 10
#'
#' # Generate wind data in polar coordinates
#' data <- data.frame(
#'   x = rnorm(n),
#'   y = rnorm(n),
#'   dir = runif(n, -pi, pi), # angle in radians
#'   spd = rchisq(n, df = 2)  # speed
#' ) |>
#'   transform(fx = spd * cos(dir), fy = spd * sin(dir))
#'
#' # Using fx/fy to compute endpoints
#' ggplot(data, aes(x, y)) +
#'   geom_vector(aes(fx = fx, fy = fy))
#'
#' # Using angle (in radians) and distance to compute endpoints
#' ggplot(data, aes(x, y)) +
#'   geom_vector(aes(angle = dir, distance = spd))
#'
#' # Using angle_deg (in degrees) and distance to compute endpoints
#' vectors3 <- data.frame(
#'   x = c(0, 1, 2),
#'   y = c(0, 1, 2),
#'   angle_deg = c(0, 90, 45),
#'   angle = c(0, pi/2, pi/4),
#'   distance = c(3, 4, 5)
#' )
#' ggplot(vectors3, aes(x, y)) +
#'   geom_vector(aes(angle_deg = angle_deg, distance = distance))
#'
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
#' # Using center = TRUE to recenter vectors:
#' ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_vector(center = TRUE)
#'
#' # Using normalize = TRUE to adjust vectors to unit length:
#' ggplot(vectors3, aes(x = x, y = y, angle = angle, distance = distance)) +
#'   geom_vector(normalize = TRUE)
#'
#' # Using geom_vector2, which adds a default mapping for `length`
#' ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
#'   geom_vector2()
#'
#' @aliases geom_vector stat_vector geom_vector2 stat_vector2
#' @name geom_vector
#' @export
NULL

#' @rdname geom_vector
#' @export
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
      type = "vector",
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
      type = "vector",
      ...
    )
  )
}

#' @rdname geom_vector
#' @format NULL
#' @usage NULL
#' @export
StatVector <- ggproto("StatVector", Stat,

  required_aes = c("x", "y"),

  default_aes = aes(xend = NA, yend = NA, distance = NA, angle = NA,
                    angle_deg = NA, fx = NA, fy = NA),

  compute_group = function(data, scales, center, normalize, L,...) {

    n <- nrow(data)

    data$x0 <- data$x
    data$y0 <- data$y

    if ("angle_deg" %in% names(data) && !all(is.na(data$angle_deg))) {
      data$angle <- data$angle_deg * pi / 180
    }

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
        data$xend <- data$x + data$distance * cos(data$angle)
        data$yend <- data$y + data$distance * sin(data$angle)
      } else {
        stop("Either xend/yend or fx/fy or angle/distance must be provided.")
      }
    }

    data$norm <- sqrt((data$xend - data$x)^2 + (data$yend - data$y)^2)

    if (is.null(L)) {
      # use x/ylim if provided by a previous layer (and make sure its no c(0,0)), otherwise base it on data
      xlim <- if (!is.null(scales$x$range$range) && !all(scales$x$range$range == 0))
        scales$x$range$range
      else
        range(c(data$x, data$xend), na.rm = TRUE)

      ylim <- if (!is.null(scales$y$range$range) && !all(scales$y$range$range == 0))
        scales$y$range$range
      else
        range(c(data$y, data$yend), na.rm = TRUE)


      # Set a default grid resolution (number of cells along the larger axis).
      # In geom_stream_field n is passed in; here we assume, say, 20.
      grid_n <- 11

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
      type = "vector",
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
      type = "vector",
      ...
    )
  )
}

