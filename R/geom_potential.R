#' Compute and Plot Potential Function from a Conservative Vector Field
#'
#' `geom_potential()` adds a raster layer to a ggplot object, visualizing the
#' potential function derived from a conservative vector field. It computes the
#' potential numerically over a specified grid and displays it as a heatmap.
#'
#' @param mapping A set of aesthetic mappings created by [ggplot2::aes()]. (Optional)
#' @param data The data to be displayed in this layer. If `NULL`, data is inherited from the plot.
#' @param stat The statistical transformation to use on the data (default: [StatPotential]).
#' @param geom The geometric object used to render the potential function. Defaults to [GeomPotential].
#' @param position Position adjustment, either as a string or the result of a position adjustment function.
#' @param na.rm Logical. If `FALSE` (default), missing values are removed with a warning.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes Logical. If `FALSE`, overrides the default aesthetics rather than combining with them.
#' @param fun A function that takes a numeric vector of length 2 (`c(x, y)`) and returns a numeric value,
#'   defining the conservative vector field. **(Required)**
#' @param xlim Numeric vector of length 2 defining the domain limits on the x-axis.
#'   Defaults to `c(-1, 1)`.
#' @param ylim Numeric vector of length 2 defining the domain limits on the y-axis.
#'   Defaults to `c(-1, 1)`.
#' @param n Integer. Number of grid points along each axis for computing the potential.
#'   Defaults to `21`.
#' @param tolerance Numeric. Tolerance for verifying if the vector field is conservative.
#'   Defaults to `1e-6`.
#' @param ... Other arguments passed to [ggplot2::layer()] and underlying methods.
#'
#' @section Aesthetics:
#' `geom_potential()` accepts all aesthetics supported by [GeomRaster]. In particular, the key aesthetics include:
#'
#'   - **fill**: The computed potential value at each grid cell, which is mapped to a color scale.
#'   - `x` and `y`: The coordinates of the grid cell centers. (calculated)
#'   - `alpha`: Controls the transparency of the raster fill.
#'
#' Additional raster-specific aesthetics (e.g. those controlled by [scale_fill_gradient()],
#' [scale_fill_viridis_c()], etc.) can be applied to modify the appearance of the potential heatmap.
#'
#' @section Computed Variables:
#'
#' The following variable is computed internally by [StatPotential] during the
#' potential function calculation:
#'
#' \describe{
#'   \item{Potential}{The scalar potential value computed numerically at each grid point.
#'     It represents the accumulated potential from a reference point (typically the lower
#'     bounds of \code{xlim} and \code{ylim}) to the given point. This value is mapped to
#'     the \code{fill} aesthetic in the raster layer.}
#' }
#'
#' @return A ggplot2 layer that produces a potential function heatmap.
#'
#' @examples
#' # Define a conservative vector field function
#' fun <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(sin(x) + y, x - sin(y))
#' }
#'
#' # Create the potential function heatmap
#' ggplot() +
#'   geom_potential(fun = fun)
#'
#' @export
geom_potential <- function(mapping = NULL, data = NULL,
   stat = StatPotential,
   position = "identity",
   ...,
   na.rm = FALSE,
   inherit.aes = TRUE,
   show.legend = NA,
   fun,
   xlim = NULL,
   ylim = NULL,
   n = 11,
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
    geom = GeomPotential,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
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
stat_potential <- function(mapping = NULL, data = NULL,
   geom = GeomPotential,
   position = "identity",
   ...,
   na.rm = FALSE,
   inherit.aes = TRUE,
   show.legend = NA,
   fun,
   xlim = NULL,
   ylim = NULL,
   n = 11,
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
    stat = StatPotential,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
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

  compute_group = function(data, scales, fun = NULL, xlim = NULL, ylim = NULL, n, tolerance = 1e-6, ...) {

    xlim <- xlim %||% scales$x$range$range
    if (is.null(xlim)) {
      # cli::cli_warn("No xlim provided or inherited; defaulting to c(-1, 1).")
      xlim <- c(-1, 1)
    }

    ylim <- ylim %||% scales$y$range$range
    if (is.null(ylim)) {
      # cli::cli_warn("No ylim provided or inherited; defaulting to c(-1, 1).")
      ylim <- c(-1, 1)
    }

    # Ensure the vector field function is provided
    if (is.null(fun)) {
      stop("Parameter `fun` must be provided to compute the potential function.")
    }

    # Generate grid
    grid_data <- expand.grid(
      x = seq(xlim[1], xlim[2], length.out = n),
      y = seq(ylim[1], ylim[2], length.out = n)
    )

    # Verify if the vector field is conservative
    grid_data$curl <- apply(grid_data[, c("x", "y")], 1, function(v) verify_potential(point = v, fun = fun, tolerance = tolerance))

    if (any(!grid_data$curl)) {
      cli::cli_warn(c(
        "!" = "The provided vector field does not have a potential function everywhere within the specified domain.",
        ">" = "Ensure that the vector field satisfies the necessary conditions for a potential function."
      ))
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

