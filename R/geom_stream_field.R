#' Create a Stream Field Layer in ggplot2
#'
#' [geom_stream_field()] creates a ggplot2 layer that integrates a user-defined
#' vector field function \eqn{f(x, y) \to (dx, dy)} over a grid of seed points
#' within a specified domain. The function numerically integrates the field
#' starting from these seeds, producing streamlines that visualize the flow.
#' This is useful for visualizing vector fields, flow patterns, or trajectories,
#' such as in fluid dynamics or gradient fields.
#'
#' @param mapping A set of aesthetic mappings created by \code{ggplot2::aes()}.
#'   (Optional)
#' @param data A data frame or other object, as in \code{ggplot2::layer()}.
#'   (Optional)
#' @param stat The statistical transformation to use on the data (default:
#'   [StatStreamField]).
#' @param geom The geometric object used to render the streamlines (defaults to
#'   [GeomStream]).
#' @param position Position adjustment, either as a string or the result of a
#'   position adjustment function.
#' @param na.rm Logical. If `FALSE` (the default), missing values are removed
#'   with a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes Logical. If `FALSE`, overrides the default aesthetics
#'   rather than combining with them.
#' @param fun A function of two variables, `fun(x, y)`, returning a two-element
#'   vector \eqn{(dx, dy)} that defines the local flow direction at any point.
#' @param xlim Numeric vector of length 2 specifying the domain limits in the
#'   \eqn{x}-direction. Defaults to \eqn{c(-1, 1)}.
#' @param ylim Numeric vector of length 2 specifying the domain limits in the
#'   \eqn{y}-direction. Defaults to \eqn{c(-1, 1)}.
#' @param n Integer or two-element numeric vector specifying the grid resolution
#'   (number of seed points) along each axis. Defaults to `11`, producing an
#'   \eqn{11 \times 11} grid.
#' @param args A list of additional arguments passed to `fun`.
#' @param max_it `integer(1)`; Maximum number of integration steps per
#'   streamline (default: `1000L`).
#' @param tol `numeric(1)`; a tolerance used to determine if a sink has been
#'   hit, among other things (default: `sqrt(.Machine$double.eps)`).
#' @param L Numeric. Maximum arc length for each streamline. When `normalize =
#'   TRUE`, integration stops once the cumulative arc length reaches `L`. When
#'   `normalize = FALSE`, streamlines are initially computed for a fixed time
#'   `T` and then cropped so that all are truncated to the duration it takes the
#'   fastest streamline to reach the arc length `L`. Defaults to `NULL` (a
#'   suitable default is computed from the grid spacing).
#' @param T Numeric. When `normalize = FALSE`, each streamline is integrated for
#'   a fixed time `T` before being cropped to match the duration of the fastest
#'   streamline reaching the arc length `L`. When `normalize = TRUE`,
#'   integration instead stops when the cumulative arc length reaches `L`, and
#'   the parameter `T` is ignored.
#' @param center Logical. If `TRUE` (default), centers the seed points (or
#'   resulting streamlines) so that the original (x, y) becomes the midpoint.
#' @param type Character. Either `"stream"` (default) or `"vector"`. `"stream"`
#'   computes a full streamline by integrating in both directions (if `center =
#'   TRUE`), while `"vector"` computes a single vector.
#' @param normalize Logical.
#'   When `normalize = TRUE` (the default), each streamline is integrated until its
#'   cumulative arc length reaches the specified value `L`, ensuring that all streams
#'   have a uniform, normalized length based on grid spacing.
#'   When `normalize = FALSE`, the integration runs for a fixed time (`T`), and afterward,
#'   all streamlines are cropped to the duration it takes for the fastest one to reach
#'   the length `L`, allowing for variations in arc lengths that reflect differences in
#'   flow speeds.
#' @param method Character. Integration method. `"rk4"` (Runge-Kutta 4, the
#'   default) and `"euler"` (Euler's method) step through the field in equal
#'   shares of the arc length `L` or, when streams are run to a time `T`, of
#'   that time. A step is shortened where the field turns or changes speed
#'   sharply, so a stream that runs into a sink or a singularity (such as a
#'   point charge) stops there, short of `L`. `"lsoda"` uses the adaptive solver from
#'   \code{deSolve::ode()}, stopping each stream when its arc length reaches
#'   `L`; it was the integrator in ggvfields 1.0.1. `geom_gradient_field()`,
#'   `geom_stream_smooth()` and `geom_gradient_smooth()` always use `"rk4"`.
#' @param grid A data frame containing precomputed grid points for seed
#'   placement. If `NULL` (default), a regular Cartesian grid is generated based
#'   on `xlim`, `ylim`, and `n`.
#' @param arrow A \code{grid::arrow()} specification for adding arrowheads to the
#'   streamlines. Defaults to a closed arrow with a 30° angle and length `0.02`
#'   npc.
#' @param tail_point Logical. If `TRUE`, draws a point at the tail (starting
#'   point) of each streamline. Defaults to `FALSE`.
#' @param eval_point Logical. If `TRUE`, draws a point at the evaluation point
#'   where the field was computed. Defaults to `FALSE`.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
#' @param ... Other arguments passed to \code{ggplot2::layer()} and the underlying
#'   geometry/stat.
#'
#' @section Aesthetics: `geom_stream_field()` (and its stat variant) inherit
#'   aesthetics from [GeomStream] and understand the following:
#'
#'   - **`x`**: x-coordinate of the seed point.
#'   - **`y`**: y-coordinate of the seed point.
#'   - `color`: Color, typically used to represent computed statistics (e.g. average speed).
#'   - `linetype`: Type of line used to draw the streamlines.
#'   - `linewidth`: Thickness of the streamlines.
#'   - `alpha`: Transparency of the streamlines.
#'
#' @section Details: The streamlines are generated by numerically integrating
#'   the vector field defined by `fun(x, y)`. When `normalize = TRUE`,
#'   integration stops once the cumulative arc length reaches `L`; otherwise,
#'   integration runs until time `T` is reached. If both `T` and `L` are
#'   provided in incompatible combinations, one parameter is ignored. The
#'   computed paths are rendered by [GeomStream].
#'
#' @return A ggplot2 layer that computes and renders streamlines over the
#'   specified domain.
#'
#' @section Computed Variables:
#'
#' The following variables are computed internally by [StatStreamField] during the
#' integration of the vector field:
#'
#' \describe{
#'   \item{avg_spd}{The average speed along the stream: its arc length divided by
#'     the time it took. Streams run to a time `T` (and, with `normalize = FALSE`,
#'     streams cut back to the time of the fastest one) use the time they were
#'     given, even if they stopped early at a sink. A stream run to a length `L`
#'     that stops short of it at a sink would take unbounded time to reach the
#'     sink, so its speed is measured up to 99% of the length it achieved. This
#'     is the default color of streams.}
#'
#'   \item{t}{The integration time at each computed point along a streamline.}
#'
#'   \item{d}{The distance between consecutive points along the computed path.}
#'
#'   \item{l}{The cumulative arc length along the streamline, calculated as the cumulative
#'     sum of \code{d}.}
#' }
#'
#' @examples
#'
#' f <- function(u) c(-u[2], u[1])
#'
#' # the basic usage involves providing a fun, xlim, and ylim
#' ggplot() + geom_stream_field(fun = f, xlim = c(-1,1), ylim = c(-1,1))
#' \donttest{
#' # if unspecified, xlim and ylim default to c(-1,1). we use this in what
#' # follows to focus on other parts of the code
#' ggplot() + geom_stream_field(fun = f)
#' ggplot() + geom_stream_field(fun = f, center = FALSE)
#'
#' ggplot() + geom_stream_field(fun = f, normalize = FALSE)
#' ggplot() + geom_stream_field(fun = f, normalize = FALSE, center = FALSE)
#'
#' # run systems until specified lengths
#' ggplot() + geom_stream_field(fun = f, normalize = TRUE, L = .8)
#' ggplot() + geom_vector_field(fun = f, normalize = TRUE, L = .3)
#' ggplot() + geom_vector_field(fun = f, normalize = FALSE, L = 2)
#'
#' # run systems for specified times
#' ggplot() + geom_stream_field(fun = f, normalize = FALSE, T = .1)
#'
#' # tail and eval points
#' ggplot() + geom_stream_field(fun = f, tail_point = TRUE)
#' ggplot() + geom_stream_field(fun = f, eval_point = TRUE)
#'
#' # changing the grid of evaluation
#' ggplot() + geom_stream_field(fun = f)
#' ggplot() + geom_stream_field(fun = f, grid = "hex")
#' ggplot() + geom_stream_field(fun = f, grid = "hex", n = 5)
#' ggplot() + geom_stream_field(fun = f, n = 5)
#' ggplot() + geom_stream_field(fun = f, xlim = c(-5, 5)) + coord_equal()
#' ggplot() + geom_stream_field(fun = f, xlim = c(-5, 5), n = c(21, 11)) + coord_equal()
#' ggplot() + geom_stream_field(fun = f)
#' ggplot() + geom_stream_field(fun = f, grid = grid_hex(c(-1,1), c(-1,1), .2))
#'
#' # using other ggplot2 tools
#' f <- efield_maker()
#'
#' ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2))
#'
#' ggplot() +
#'   geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2)) +
#'   scale_color_viridis_c(trans = "log10")
#'
#' ggplot() +
#'   geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2)) +
#'   scale_color_viridis_c(trans = "log10") +
#'   coord_equal()
#'
#'
#' # other vector fields
#' f <- function(u) u
#' ggplot() + geom_stream_field(fun = f, xlim = c(-1,1), ylim = c(-1,1))
#'
#' f <- function(u) c(2,1)
#' ggplot() + geom_stream_field(fun = f, xlim = c(-1,1), ylim = c(-1,1))
#'
#'
#'
#' # neat examples
#' f <- function(u) {
#'   x <- u[1]; y <- u[2]
#'   c(y, y*(-x^2 - 2*y^2 + 1) - x)
#' }
#' ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2))
#' ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2), type = "vector")
#'
#' f <- function(u) {
#'   x <- u[1]; y <- u[2]
#'   c(y, x - x^3)
#' }
#' ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2))
#' ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2),
#'   grid = grid_hex(c(-2,2), c(-2,2), .35))
#'
#' f <- function(u) {
#'   x <- u[1]; y <- u[2]
#'   c(x^2 - y^2, x^2 + y^2 - 2)
#' }
#' ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2))
#' ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2),
#'   grid = grid_hex(c(-2,2), c(-2,2), .35))
#'
#' ggplot() +
#'   geom_stream_field(fun = f, aes(alpha = after_stat(t)), xlim = c(-2,2), ylim = c(-2,2)) +
#'   scale_alpha(range  = c(0,1))
#'
#' ggplot() +
#'   geom_stream_field(
#'     fun = f, xlim = c(-1,1), ylim = c(-1,1),
#'     linewidth = .75, arrow = arrow(length = unit(0.015, "npc"))
#'   )
#' }
#'
#' @aliases geom_stream_field stat_stream_field geom_stream_field2 stat_stream_field2 StatStreamField
#' @name geom_stream_field
#' @export
NULL

#' @rdname geom_stream_field
#' @export
geom_stream_field <- function(
    mapping = NULL,
    data = NULL,
    stat = StatStreamField,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    xlim = NULL,
    ylim = NULL,
    n = 11,
    args = list(),
    max_it = 1000L,
    tol = sqrt(.Machine$double.eps),
    T = NULL,
    L = NULL,
    center = TRUE,
    type = "stream",
    normalize = TRUE,
    tail_point = FALSE,
    eval_point = FALSE,
    grid = NULL,
    method = "rk4",
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    arrow = grid::arrow(angle = 30, length = unit(0.02, "npc"), type = "closed")
) {

  call_args <- as.list(environment())

  construct_layer(
    stream_field_layer, call_args, list(...),
    geom = GeomStream
  )
}


#' @rdname geom_stream_field
#' @export
stat_stream_field <- function(
    mapping = NULL,
    data = NULL,
    geom = GeomStream,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    xlim = NULL,
    ylim = NULL,
    n = 11,
    args = list(),
    max_it = 1000,
    tol = sqrt(.Machine$double.eps),
    T = NULL,
    L = NULL,
    center = TRUE,
    type = "stream",
    normalize = TRUE,
    tail_point = FALSE,
    eval_point = FALSE,
    grid = NULL,
    method = "rk4",
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    arrow = grid::arrow(angle = 30, length = unit(0.02, "npc"), type = "closed")
) {

  call_args <- as.list(environment())

  construct_layer(
    stream_field_layer, call_args, list(...),
    stat = StatStreamField
  )
}

#' @rdname geom_stream_field
#' @export
geom_stream_field2 <- function(
    mapping = NULL,
    data = NULL,
    stat = StatStreamField,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    xlim = NULL,
    ylim = NULL,
    n = 11,
    args = list(),
    max_it = 1000,
    tol = sqrt(.Machine$double.eps),
    L = NULL,
    center = FALSE,
    type = "stream",
    tail_point = TRUE,
    eval_point = FALSE,
    grid = NULL,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    method = "rk4"
    ) {

  call_args <- as.list(environment())

  construct_layer(
    stream_field2_layer, call_args, list(...),
    geom = GeomStream
  )
}

#' @rdname geom_stream_field
#' @export
stat_stream_field2 <- function(
    mapping = NULL,
    data = NULL,
    geom = GeomStream,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = FALSE,
    fun,
    xlim = NULL,
    ylim = NULL,
    n = 11,
    args = list(),
    max_it = 1000,
    tol = sqrt(.Machine$double.eps),
    L = NULL,
    center = FALSE,
    type = "stream",
    tail_point = TRUE,
    eval_point = FALSE,
    grid = NULL,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    method = "rk4"
    ) {

  call_args <- as.list(environment())

  construct_layer(
    stream_field2_layer, call_args, list(...),
    stat = StatStreamField
  )
}

#' @rdname geom_stream_field
#' @format NULL
#' @usage NULL
#' @export
StatStreamField <- ggproto(
  "StatStreamField",
  Stat,
  default_aes = aes(group = after_stat(id)),

  setup_data = function(data, params) {

    # If data has xend/yend (and not already fx/fy), compute fx and fy.
    if (!is.null(data)) {
      if (all(c("xend", "yend") %in% names(data)) &&
          !(all(c("fx", "fy") %in% names(data)))) {
        data$fx <- data$xend - data$x
        data$fy <- data$yend - data$y
        # Remove xend and yend so that they don't affect scale calculations.
        data <- data[, setdiff(names(data), c("xend", "yend")), drop = FALSE]
      } else if (all(c("angle", "distance") %in% names(data)) &&
                 !(all(c("fx", "fy") %in% names(data)))) {
        # Assuming 'angle' is in radians.
        # If angles are in degrees, convert with: angle * pi / 180.
        data$fx <- data$distance * cos(data$angle)
        data$fy <- data$distance * sin(data$angle)
        # Remove angle and distance so that they don't affect scale calculations.
        data <- data[, setdiff(names(data), c("angle", "distance")), drop = FALSE]
      }
    }
    data
  },

  compute_group = function(data, scales, fun, xlim, ylim, n,
                           method = "rk4", max_it = 1000, tol = sqrt(.Machine$double.eps),
                           T = NULL, L = NULL,
                           center, type, normalize, args = NULL, grid, ...) {

    ## pick the right limits.  may need updating but currently prioritizing
    # limit specified by user
    # range of data received from ggplot layer or in this layer
    # range inherited from previous layer's scales
    if( is.null(grid) || (!is.data.frame(grid) && grid == "hex") ){

      xlim <- xlim %||%
        (if (!is.null(data) && "x" %in% names(data)) range(data$x, na.rm = TRUE) else NULL) %||%
        scales$x$range$range %||% c(-1, 1)

      ylim <- ylim %||%
        (if (!is.null(data) && "y" %in% names(data)) range(data$y, na.rm = TRUE) else NULL) %||%
        scales$y$range$range %||% c(-1, 1)

      # make grid of points on which to compute streams
      if (is.null(grid)) {

        grid <- cbind(
          "x" = rep(seq(xlim[1], xlim[2], length.out = n[1]), times = n[2]),
          "y" = rep(seq(ylim[1], ylim[2], length.out = n[2]), each = n[1])
        )

      } else if (grid == "hex") {

        grid <- as.matrix( grid_hex(xlim, ylim,
          d = sqrt(3)/2 * sqrt((diff(xlim)/n[1])^2 + (diff(ylim)/n[2])^2)
        ) )

      }


    } else {
      grid  <- as.matrix(grid)
      xlim <- range(grid[, "x"])
      ylim <- range(grid[, "y"])
    }

    # allow for additional args to be passed
    orig_fun <- fun
    fun <- function(v) rlang::inject(orig_fun(v, !!!args))

    ## vector and normalize
    if ( type == "vector" && normalize ) {
      if( is.null(L) ) L <- min(diff(xlim), diff(ylim)) / (max(n) - 1) * 0.85
    }

    ## vector and not normalize
    if ( type == "vector" && !normalize ) {
      if (!is.null(L)) {
        cli::cli_warn("Specifying L with non normalized vectors is incompatible. Ignoring L.")
      }
      T <- 1
    }

    ## stream and normalize
    if ( type == "stream" && normalize ) {
        if( is.null(L) ) L <- min(diff(xlim), diff(ylim)) / (max(n) - 1) * 0.85
        if( !is.null(T) ) cli::cli_alert("Specifying T for normalized streams is not compatible. Ignoring T.")
    }

    ## stream and not normalize
    ## If only L is given, grow all streams to L, find stream that got there in the shortest time, trim all steams back to that time
    ## If only T is given, grow all streams to T
    ## If both are given, ignore L and growl all streams to T
    if ( type == "stream" && !normalize ) {
      if( is.null(T)) {## user didn't specify T so we need to calculate L first, if L isn't given
        if( is.null(L) ) L <- min(diff(xlim), diff(ylim)) / (max(n) - 1) * 0.85
      }
      if( !is.null(T) && !is.null(L) ) {
        cli::cli_inform("Specifying L and T for non normalized streams is not compatible. Ignoring L.")
      }
    }

    # initialize the data frame
    list_of_streams <- vector(mode = "list", length = nrow(grid))
    # iterate computing streams/vectors

    for (i in 1:nrow(grid)) {

      # compute the stream for the current grid point using ode_stepper()
      if (type == "vector") {

        u <- grid[i,]
        fu <- fun(u)
        nfu <- norm(fu)

        v <- if (normalize) {
          L*fu/nfu
        } else {
          T*fu
        }

        if (center) {
          from <- u - v/2
          to <- u + v/2
        } else {
          from <- u
          to <- u + v
        }

        tempL <- ifelse(is.null(L), T*nfu, L)

        stream <- data.frame(
          "t" = c(0, tempL/nfu),
          "x" = c(from[1], to[1]),
          "y" = c(from[2], to[2]),
          "d" = c(NA_real_, L),
          "l" = c(0, tempL),
          "avg_spd" = nfu, # = L / (L/nfu)
          "norm" = nfu,
          "id" = i
        )

      }
      if (type == "stream") {

        # the field is undefined at this seed point, so there is no stream
        if (!all(is.finite(fun(grid[i, ])))) next

        if ( !normalize && !is.null(T) ) {
          stream <- ode_stepper(grid[i, ], fun, T = T, L = 1e6, max_it, tol, method, center)
        } else {
          stream <- ode_stepper(grid[i, ], fun, T = 1e6, L = L, max_it, tol, method, center)
        }

        stream <- if(nrow(stream) >= 1) transform(stream, "id" = i) else next

      }

      # add the original x and y coordinates to this stream's data
      stream$x0 <- grid[i, "x"]
      stream$y0 <- grid[i, "y"]

      # store the stream
      list_of_streams[[i]] <- stream

    }


    # temporally crop back streams if not normalizing and T not specified
    if ( !normalize && type == "stream" && is.null(T)) {

      # compute how long it takes for each stream to get to L
      times_to_L_by_stream <- vapply(
        list_of_streams,
        # if center = FALSE, will have negative times; skipped streams are NULL
        function(df) if (is.null(df)) NA_real_ else df$t[nrow(df)] - df$t[1],
        numeric(1)
      )
      times_to_L_by_stream <- unname(times_to_L_by_stream)

      # find the fastest stream to get to L, and the associated time
      fastest_stream_id <- which.min( times_to_L_by_stream )
      smallest_t <- times_to_L_by_stream[fastest_stream_id]

      # go back through grid cropping time to T = fastest_t
      for (i in 1:nrow(grid)) {
        if (is.null(list_of_streams[[i]])) next
        stream <- crop_stream_time(
          list_of_streams[[i]],
          T = smallest_t,
          centered = TRUE
        )

        # avg_spd is the length of the drawn (cropped) stream over the time
        # every stream was given, as for streams run to a time T; a stream that
        # reached a sink sooner still counts the whole time
        n_pts <- nrow(stream)
        if (n_pts > 1) stream$avg_spd <- stream$l[n_pts] / smallest_t

        list_of_streams[[i]] <- stream
      }

    }

    # combine streams and return
    list_of_streams <-  do.call("rbind", list_of_streams)

    ## fix remove aes warning
    list_of_streams$fx <- NA_real_
    list_of_streams$fy <- NA_real_
    list_of_streams$z <- NA_real_
    list_of_streams$distance <- NA_real_
    list_of_streams$angle <- NA_real_


    list_of_streams

  }


)

#' @keywords internal
fixed_step_path <- function(u0, fun, T = NULL, L = NULL, max_it = 1000,
                            tol = sqrt(.Machine$double.eps), method = "rk4",
                            n_steps = 25, max_turn = pi / 12, max_speed_ratio = 2,
                            max_halvings = 30) {

  T <- T %||% Inf
  L <- L %||% Inf

  empty <- data.frame(
    t = numeric(0), x = numeric(0), y = numeric(0), d = numeric(0),
    l = numeric(0), avg_spd = numeric(0), norm = numeric(0)
  )

  # fun is called with the seed's names (x and y from the grid) at every
  # point, as the lsoda path and vector layers do, so fields that index their
  # argument by name, e.g. u["x"] or with(as.list(u), ...), work with rk4 too
  coord_names <- names(u0)
  at <- function(p) { names(p) <- coord_names; p }
  field <- function(p) as.numeric(fun(at(p)))

  u <- as.numeric(u0)
  nfu <- norm(field(u))

  # the field is undefined at the seed; nothing to draw
  if (!is.finite(nfu)) return(empty)

  # the seed is a sink or source; the stream does not move
  if (nfu < tol) {
    return(matrix_to_df_with_names(rbind(c(0, u), c(T, u)), c("t","x","y")))
  }

  # streams are integrated in whichever variable bounds them first at the
  # starting speed. a stream run to length L is integrated in arc length s,
  # du/ds = f(u)/|f(u)| and dt/ds = 1/|f(u)|, so each step moves the same
  # distance and the time it takes is tracked for avg_spd. a stream run to
  # time T is integrated in time, du/dt = f(u). the state is v = (x, y, t).
  by_length <- L / nfu <= T
  deriv <- if (by_length) {
    function(v) { fv <- field(v[1:2]); nfv <- norm(fv); c(fv / nfv, 1 / nfv) }
  } else {
    function(v) c(field(v[1:2]), 1)
  }

  # each step returns the new state and the derivatives at its stages
  step <- switch(method,
    "euler" = function(v, dv, h) list(v = v + h * dv, stages = list()),
    "rk4" = function(v, dv, h) {
      k2 <- deriv(v + h/2 * dv)
      k3 <- deriv(v + h/2 * k2)
      k4 <- deriv(v + h * k3)
      list(v = v + h/6 * (dv + 2*k2 + 2*k3 + k4), stages = list(k2, k3, k4))
    }
  )

  # the direction of the field may turn at most max_turn over one step, at the
  # end of the step and at every stage. a step that crosses a sink turns the
  # direction around, so it is retried with half the step size; the stream
  # closes in on the sink and stops there, short of L
  turn_ok <- function(dv, dv_new) {
    a <- dv[1:2]; b <- dv_new[1:2]
    na <- norm(a); nb <- norm(b)
    if (!is.finite(nb)) return(FALSE)
    if (na < tol || nb < tol) return(TRUE)
    sum(a * b) / (na * nb) >= cos(max_turn)
  }

  # the speed of the field may change by at most a factor of max_speed_ratio
  # over one step, at its end and at every stage. near a singularity such as a
  # point charge the speed changes by orders of magnitude, and a long step can
  # jump over the singularity to a point where the field happens to point the
  # same way; this keeps the steps short there, so the stream stops at it
  speed <- function(dv) if (by_length) 1 / dv[3] else norm(dv[1:2])
  speed_ok <- function(dv, dv_new) {
    s0 <- speed(dv); s1 <- speed(dv_new)
    if (!is.finite(s1)) return(FALSE)
    if (s0 < tol) return(TRUE)
    s1 / s0 <= max_speed_ratio && s0 / s1 <= max_speed_ratio
  }

  # the field has stopped moving: a sink in time, or 1/|f| blowing up in length
  stalled <- function(dv) {
    if (by_length) dv[3] > 1 / tol else norm(dv[1:2]) < tol
  }

  h0 <- if (by_length) L / n_steps else T / n_steps
  h <- h0
  v <- c(u, 0)
  dv <- deriv(v)
  l <- 0; k <- 1

  path <- matrix(NA_real_, nrow = max_it + 1, ncol = 3)
  path[1,] <- c(0, u)

  # a stream within a relative 1e-9 of L or T has arrived: the sum of the step
  # lengths (or times) can fall a rounding error short, and another full step
  # would be cropped back to a segment of length ~1e-16, along which the
  # arrowhead would then point in an arbitrary direction
  L_end <- L * (1 - 1e-9)
  T_end <- T * (1 - 1e-9)

  while (l < L_end && v[3] < T_end && k <= max_it) {

    # land the last step of a time-bounded stream exactly on T
    if (!by_length) h <- min(h, T - v[3])

    accepted <- FALSE
    for (i in 0:max_halvings) {
      stepped <- step(v, dv, h)
      v_new <- stepped$v
      if (all(is.finite(v_new))) {
        dv_new <- deriv(v_new)
        checks <- c(stepped$stages, list(dv_new))
        if (all(is.finite(dv_new)) &&
            all(vapply(checks, turn_ok, logical(1), dv = dv)) &&
            all(vapply(checks, speed_ok, logical(1), dv = dv))) {
          accepted <- TRUE
          break
        }
      }
      h <- h / 2
    }

    # no step size works: the stream has reached a sink or an undefined point
    if (!accepted) break

    moved <- norm(v_new[1:2] - v[1:2])
    l <- l + moved
    v <- v_new
    dv <- dv_new
    k <- k + 1
    path[k,] <- c(v[3], v[1:2])

    # stop at a sink, or when the stream has stopped moving, as it does when
    # it closes in on a singularity in ever smaller steps
    if (stalled(dv) || moved < tol) break

    h <- min(2 * h, h0)

  }

  if (k == 1) return(empty)

  matrix_to_df_with_names(path[seq_len(k), , drop = FALSE], c("t","x","y"))

}

#' @keywords internal
# the length and time that avg_spd = length / time is computed from, for a
# stream that starts at its seed at t = 0 (one half of a centered stream).
# avg_spd is the length a stream achieved over the time it took, but two cases
# need care so that it describes the stream and not where the integrator stops:
# - a stream run to a time T has its speed over T, the time it was given, even
#   if it stopped early at a sink (T = 1e6 is the placeholder when streams are
#   run to a length instead)
# - a stream run to a length L that stops short of it at a sink would take
#   unbounded time to reach the sink, so its speed is measured up to 99% of the
#   length it achieved
avg_spd_parts <- function(df, T, L) {
  n <- nrow(df)
  run_to_time <- !is.null(T) && T < 1e6
  if (n < 2) return(c(l = 0, t = if (run_to_time) T else 0))

  l <- df$l[n]
  t <- df$t[n]
  if (run_to_time) return(c(l = l, t = T))

  if (l > 0 && !is.null(L) && l < L * (1 - 1e-6)) {
    t_99 <- stats::approx(df$l, df$t, 0.99 * l, ties = "ordered")$y
    return(c(l = 0.99 * l, t = t_99))
  }

  c(l = l, t = t)
}

#' @keywords internal
ode_stepper <- function(u0, fun, T = NULL, L = NULL, max_it = 5000, tol = sqrt(.Machine$double.eps),
                        method = "lsoda", center = FALSE) {
  if ( center ) {

    # define a few helpers
    neg_fun <- function(u) -fun(u)
    flip <- function(df) if (nrow(df) == 0L) df else df[nrow(df):1,]
    # the nrow(df) == 0 part is if the original eval point fails,
    # vec field is not defined at that point, so
    # if you df[nrow(df):1,] on a 0-row df, you get back a row of NAs

    # solve in both directions
    df_negative <- ode_stepper(u0, neg_fun, T, L/2, max_it, tol, method, center = FALSE)
    df_positive <- ode_stepper(u0,     fun, T, L/2, max_it, tol, method, center = FALSE)

    # the length and time avg_spd is computed from, for each half
    spd_neg <- avg_spd_parts(df_negative, T, L/2)
    spd_pos <- avg_spd_parts(df_positive, T, L/2)

    # format both for merging
    n_neg <- nrow(df_negative)
    n_pos <- nrow(df_positive)
    df_negative$t <- -df_negative$t
    if (n_neg > 0) {
      row.names(df_negative) <- as.character(-seq(0, n_neg - 1))
    } else {
      row.names(df_negative) <- character(0)
    }

    if (n_pos > 0) {
      row.names(df_positive) <- as.character(seq(0, n_pos - 1))
    } else {
      row.names(df_positive) <- character(0)
    }
    # merge; the seed (t = 0) is the first row of both halves, so it is taken
    # from the forward half, or from the backward half when the forward half is
    # empty (the stream can only move one way from this seed, e.g. at the edge
    # of the field's domain)
    df <- if (n_pos == 0) {
      flip(df_negative)
    } else {
      rbind( flip(df_negative[-1,]), df_positive )
    }

    if (nrow(df) == 0) {## this happens when stream is in a sync or source
      df <- data.frame(
        t       = numeric(0),
        x       = numeric(0),
        y       = numeric(0),
        d       = numeric(0),
        l       = numeric(0),
        avg_spd = numeric(0),
        norm    = numeric(0)
      )
      return(df)
    }


    # shift lengths, recompute distances, arc length, avg speed
    n <- nrow(df)
    df$d <- if (n <= 1) NA_real_ else c(NA_real_, sqrt(diff(df$x)^2 + diff(df$y)^2))
    df$l <- if (n <= 1) 0 else c(0, cumsum(df$d[-1]))
    df$avg_spd <- (spd_neg[["l"]] + spd_pos[["l"]]) / (spd_neg[["t"]] + spd_pos[["t"]])

    # return
    return(df)

  }
  if (method %in% c("rk4", "euler")) {

    df <- fixed_step_path(u0, fun, T, L, max_it, tol, method)
    if (nrow(df) == 0) return(df)

  } else {

  # deSolve::ode() is a little particular in terms of what it assumes about the
  # function. first, it assumes that the function returns a list with a vector
  # in it, e.g. list(1:2). second, it only solves up until at the points c(0,T)
  # unless it is interrupted by the length being L. in either case, it returns
  # a data frame with 2 rows, discarding intermediate information.
  # thus, we create a wrapper of fun that (1) returns the value as a list,
  # (2) tracks where fun was evaluated, and (3) adds the cumulative length
  # to the curve. for (3), the system is
  # x'(t) = fun(u)[1], y'(t) = fun(u)[2], l'(t) = sqrt(x'(t)^2 + y'(t)).
  # this system is interrupted by the "root" function that is 0 when l(t) = L
  fun_wrapper <- function(t, u, parms) {

    fu <- fun(u[1:2])
    n_evals <<- n_evals + 1L
    if (n_evals > length(evals)) length(evals) <<- 2L * length(evals)
    evals[[n_evals]] <<- c(t, u[1:2])
    list( c( fu, "l" = norm(fu) ) )
  }

  # two roots stop the solver: the arc length reaching L, and the speed of the
  # field falling to tol, which happens when the stream runs into a sink.
  # (fun is called directly so these evaluations are not tracked as the path)
  rootfun <- function(t, u, parms) {
    c(u[3] - parms$L, norm(fun(u[1:2])) - tol)
  }

  # initialize the list tracking evals of fun; it is doubled when it fills up
  evals <- vector("list", 64L)
  n_evals <- 0L

  # solve up to length L; soln is not actually used after since evals are captured
  # the invisible part is used to suppress warnings
  invisible(
    capture.output(
      soln <- deSolve::ode(
        func = fun_wrapper,
        y = c(u0, "l" = 0), # 0 = initial arc length
        parms = list("L" = L),
        times = c(0, T),
        rootfun = rootfun,
        maxsteps = max_it,
        method = method
      )
    )
  )

  # the evals have lots of near-duplicated rows. let's remove those
  # (a row is dropped when it is within tol of the eval just before it)
  evals <- unname( do.call(rbind, evals[seq_len(n_evals)]) )
  n <- nrow(evals)
  steps <- sqrt(rowSums( (evals[-1,,drop = FALSE] - evals[-n,,drop = FALSE])^2 ))
  df <- matrix_to_df_with_names(
    evals[c(TRUE, !(steps <= tol)),,drop = FALSE],
    c("t","x","y")
  )

  # time order streams. they are almost always in order, but not quite always.
  df <- df[order(df$t),]

  }

  # compute d and l stats
  n <- nrow(df)
  df$d <- c(NA_real_, apply( df[2:n,c("x","y")] - df[1:(n-1),c("x","y")], 1, norm ))
  df$l <- c(0, cumsum(df$d[-1]))

  # ode() evals of fun go further than needed, and ode() internally crops back
  # the result. since we don't use the output of ode(), we need to manually crop back
  if (!is.null(L)) df <- df |> crop_stream_length(L)
  if (!is.null(T)) df <- df |> crop_stream_time(T)

  # now add on average speed and the norm
  n <- nrow(df)
  if (n > 0) {
    spd <- avg_spd_parts(df, T, L)
    df$avg_spd <- spd[["l"]] / spd[["t"]]
  } else {
    df$avg_spd <- numeric(0)
  }

  if (nrow(df) == 0) {
    df$norm <- numeric(0)
    row.names(df) <- character(0)
  } else {
    df$norm <- norm(fun(u0))
  }

  # return
  df

}


#' @keywords internal
crop_stream_length <- function(data, L) {
  # data is assumed to have columns t, x, y, d, l
  # data is assumed to be ordered by t, but if you need to ensure this you
  # can uncomment the below
  # data <- data[order(data$t),]

  # determine number of points in the path
  n <- nrow(data)

  ## happens when in a sync or source
  if (is.na(data$l[n])) return(data[0, , drop = FALSE])

  # return data if length >= arc length of data
  # NOTE: this may need to change in the future, presumably to extent last seg
  if (L >= data$l[n]) return( data )

  # find index of first point overshooting desired length
  i <- min( which( data$l > L ) )

  # discard all the points past the first crossing
  data <- data[1:i,]

  # essentially we want data[1:(i-1),] plus another point on the line segment
  # between data[i-1,] and data[i,]. the length of that line should be enough
  # to get the total polyline length to L
  almost_L <- data$l[i-1]
  length_needed <- L - almost_L
  v <- data[i,c("x","y")] - data[i-1,c("x","y")]
  nv <- norm(v)
  m <- length_needed/nv
  data[i,c("x","y")] <- data[i-1,c("x","y")] + m * v

  # now to update t, d, and l
  data[i,"d"] <- length_needed
  data[i,"l"] <- L
  data[i,"t"] <- with(data, t[i-1] + m*(t[i]-t[i-1]))

  # return data
  data

}


# this function behaves just like crop_stream_length(), see main comments there
#' @keywords internal
crop_stream_time <- function(data, T, centered = FALSE) {
  # when centered = FALSE, the stream is assumed to run from 0 to T, or rather
  # something more than T, and this function crops back the stream to 0 to T
  # it's more challenging when t runs from some -T to T, usually resulting from
  # running the curve until length L/2 in reverse and L/2 in forward time.
  # these will have values tmin and tmax, and tmin is typically not -tmax.
  # to create a centered stream that is time-cropped to a given T,
  # assuming the original stream runs from tmin to tmax and is of length
  # L/2 from tmin to 0 and L/2 from 0 to tmax, we can't just look for T/2 in
  # both directions. (note: T is always less than the -tmin + tmax; tmin < 0)
  if ( centered ) {

    # define a helper
    flip <- function(df) if (nrow(df) == 0L) df else df[nrow(df):1,]

    # compute basic quantities
    n <- nrow(data)
    tmin <- data$t[1]; tmax <- data$t[n]
    stream_T <- tmax - tmin
    ptn <- T / stream_T # = proportion of time needed

    # break stream into two: positive and negative stream components
    rn <- as.integer( row.names(data) )
    neg_t_stream <- data[rn <= 0,]; n_neg <- nrow(neg_t_stream)
    pos_t_stream <- data[rn >= 0,]; n_pos <- nrow(pos_t_stream)

    # revert negative stream to pos t
    neg_t_stream <- flip( neg_t_stream )
    neg_t_stream$t <- -neg_t_stream$t

    # crop both to the desired proportion
    neg_t_stream <- crop_stream_time( neg_t_stream, ptn*(-tmin), centered = FALSE )
    pos_t_stream <- crop_stream_time( pos_t_stream, ptn * tmax, centered = FALSE )

    # flip negative
    neg_t_stream <- flip( neg_t_stream[-1,] ) # remove t = 0 row
    neg_t_stream$t <- -neg_t_stream$t

    # reconstruct cropped stream
    stream <- rbind(neg_t_stream, pos_t_stream)
    n <- nrow(stream)
    stream$d <- c(
      NA_real_,
      apply(
        stream[2:n,c("x","y")] - stream[1:(n-1),c("x","y")],
        1,
        norm
      )
    )
    stream$l <- c(0, cumsum(stream$d[-1]))

    # return
    return( stream )

  }

  # determine number of points in the path
  n <- nrow(data)
  if (n == 0) return(data)

  ## happens when in a sync or source
  if (is.na(data$t[n])) return(data[0, , drop = FALSE])
  # return data if doesn't get to time T, return data
  # NOTE: this may need to change in the future
  if (T >= data$t[n]) return( data )

  # find index of first point overshooting desired time
  i <- min( which( data$t > T ) )

  # discard all the points past the first crossing
  data <- data[1:i,]

  # essentially we want data[1:(i-1),] plus another point on the line segment
  # between data[i-1,] and data[i,]. the length of that line should be enough
  # to get the total polyline time to T
  almost_T <- data$t[i-1]
  proportion_needed <- (T - almost_T) / (data$t[i] - almost_T)
  v <- data[i,c("x","y")] - data[i-1,c("x","y")]
  data[i,c("x","y")] <- data[i-1,c("x","y")] + proportion_needed * v

  # now to update t, d, and l
  data[i,"d"] <- proportion_needed * norm(v)
  data[i,"l"] <- data[i-1,"l"] + data[i,"d"]
  data[i,"t"] <- T

  # return data
  data

}


#' @keywords internal
matrix_to_df_with_names <- function(mat, col_names = NULL) {
  # Check if the input is a matrix
  if (!is.matrix(mat)) cli::cli_abort("Input must be a matrix.")

  # Convert to a data frame
  df <- as.data.frame(mat)

  # Set column names if provided
  if (!is.null(col_names)) {
    if (length(col_names) != ncol(df)) {
      cli::cli_abort("`col_names` must have the same length as the number of columns in the matrix.")
    }
    names(df) <- col_names
  }

  return(df)
}
