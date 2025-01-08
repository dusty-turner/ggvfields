#' Compute and Plot Potential Function from a Conservative Vector Field
#'
#' `geom_potential()` adds a raster layer to a ggplot object, visualizing the potential function
#' derived from a conservative vector field. It computes the potential numerically over a specified
#' grid and displays it as a heatmap.
#'
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::stat_identity
#' @param data The data to be displayed in this layer. If `NULL`, the default, the data
#'   is inherited from the plot data as specified in the call to `ggplot()`.
#' @param fun A function that takes a numeric vector of length 2 (`c(x, y)`) and returns
#'   a numeric vector of length 2 (`c(dx, dy)`), defining the vector field.
#' @param xlim,ylim Numeric vectors of length 2 defining the domain limits on the x/y-axis.
#' @param n Integer, the number of grid points along each axis. Defaults to 21.
#' @param tolerance Numeric value specifying the tolerance level for verifying if the vector field
#' is conservative. Defaults to `1e-6`.
#
#'
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a potential function heatmap.
#'
#' @examples
#' # Define a conservative vector field function
#' fun <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(2 * x * y + exp(x), x^2 + 2 * y)
#' }
#'
#' # Define domain limits
#' xlim <- c(7, 10)
#' ylim <- c(7, 10)
#'
#' # Create the potential function heatmap
#' ggplot() +
#'   geom_potential(fun = fun, xlim = xlim, ylim = ylim)
#'
#' @export
geom_potential <- function(mapping = NULL, data = NULL,
                           stat = StatPotential, geom = GeomPotential,
                           ...,
                           inherit.aes = TRUE,
                           show.legend = NA,
                           position = "identity",
                           fun,
                           xlim = NULL,
                           ylim = NULL,
                           n = 21,
                           tolerance = 1e-6) {


  if (is.null(data)) {
    data <- data.frame(x = NA_real_, y = NA_real_)
  }

  # Ensure the potential function is provided
  if (missing(fun)) {
    stop("Parameter `fun` must be provided to compute the potential function.")
  }

  # Default aesthetic mapping for fill
  default_mapping <- aes(fill = after_stat(Potential))

  # Merge user-provided mappings with defaults
  if (is.null(mapping)) {
    mapping <- default_mapping
  } else {
    mapping <- modifyList(default_mapping, mapping)
  }

  layer(
    stat = stat,
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
      tolerance = tolerance,
      ...
    )
  )
}

#' @rdname geom_potential
#' @export
StatPotential <- ggproto(
  "StatPotential",
  Stat,

  # required_aes = c("x", "y"),

  default_aes = aes(fill = after_stat(Potential)),

  compute_group = function(data, scales, fun = NULL, xlim = NULL, ylim = NULL, n = 11, tolerance = 1e-6, ...) {
    # Ensure the vector field function is provided
    if (is.null(fun)) {
      stop("Parameter `fun` must be provided to compute the potential function.")
    }

    # Generate grid if not provided
    if (is.null(xlim) || is.null(ylim)) {
      if (nrow(data) > 0 && all(c("x", "y") %in% names(data))) {
        xlim <- range(data$x, na.rm = TRUE)
        ylim <- range(data$y, na.rm = TRUE)
      } else {
        stop("When using `StatPotential`, you must supply `xlim` and `ylim` if data is not provided.")
      }
    }

    # Generate grid
    grid_data <- expand.grid(
      x = seq(xlim[1], xlim[2], length.out = n),
      y = seq(ylim[1], ylim[2], length.out = n)
    )

    # Verify if the vector field is conservative
    grid_data$curl <- apply(grid_data[, c("x", "y")], 1, function(v) verify_potential(point = v, fun = fun, tolerance = tolerance))

    if (any(!grid_data$curl)) {
      warning("The provided vector field does not have a potential function everywhere within the specified domain.")
    }

    # Apply the numerical potential computation to all points
    grid_data$Potential <- apply(grid_data[, c("x", "y")], 1, function(v) compute_potential(point = v, fun =  fun, x0 = xlim[1], y0 =  ylim[1]))

    # Remove points where potential couldn't be computed
    grid_data <- grid_data[!is.na(grid_data$Potential), ]

    return(grid_data)
  }
)

#' @rdname geom_potential
#' @export
GeomPotential <- ggproto(
  "GeomPotential",
  GeomRaster,

  required_aes = c("fill"),
  # required_aes = c("x", "y", "fill"),

  default_aes = aes(
    alpha = 1
  ),

  draw_panel = function(data, panel_params, coord, ...) {
    GeomRaster$draw_panel(data, panel_params, coord, ...)
  },

  draw_key = draw_key_rect
)

