#' Compute and Plot Potential Function from a Conservative Vector Field
#'
#' `geom_potential()` adds a raster layer to a ggplot object, visualizing the
#' potential function derived from a conservative vector field. It computes the
#' potential numerically over a specified grid and displays it as a heatmap.
#'
#' @param mapping A set of aesthetic mappings created by [ggplot2::aes()].
#'   (Optional)
#' @param data The data to be displayed in this layer. If `NULL`, data is
#'   inherited from the plot.
#' @param stat The statistical transformation to use on the data (default:
#'   [StatPotential]).
#' @param geom The geometric object used to render the potential function.
#'   Defaults to [GeomPotential].
#' @param position Position adjustment, either as a string or the result of a
#'   position adjustment function.
#' @param na.rm Logical. If `FALSE` (default), missing values are removed with a
#'   warning.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes Logical. If `FALSE`, overrides the default aesthetics
#'   rather than combining with them.
#' @param fun A function that takes a numeric vector of length 2 (`c(x, y)`) and
#'   returns a numeric value, defining the conservative vector field.
#'   **(Required)**
#' @param xlim Numeric vector of length 2 defining the domain limits on the
#'   x-axis. Defaults to `c(-1, 1)`.
#' @param ylim Numeric vector of length 2 defining the domain limits on the
#'   y-axis. Defaults to `c(-1, 1)`.
#' @param n Integer. Number of grid points along each axis for computing the
#'   potential. Defaults to `21`.
#' @param tol Numeric. Tolerance for verifying if the vector field is
#'   conservative. Defaults to `1e-6`.
#' @param verify_conservative Logical. If `TRUE`, the function verifies that the
#'   provided vector field is conservative (i.e., that the mixed partial
#'   derivatives are equal within the specified tolerance). Defaults to `FALSE`.
#' @param ... Other arguments passed to [ggplot2::layer()] and underlying
#'   methods.
#'
#' @section Aesthetics: `geom_potential()` accepts all aesthetics supported by
#'   [GeomRaster]. In particular, the key aesthetics include:
#'
#'   - **fill**: The computed potential value at each grid cell, which is mapped to a color scale.
#'   - `x` and `y`: The coordinates of the grid cell centers. (calculated)
#'   - `alpha`: Controls the transparency of the raster fill.
#'
#'   Additional raster-specific aesthetics (e.g. those controlled by
#'   [scale_fill_gradient()], [scale_fill_viridis_c()], etc.) can be applied to
#'   modify the appearance of the potential heatmap.
#'
#' @section Computed Variables:
#'
#'   The following variable is computed internally by [StatPotential] during the
#'   potential function calculation:
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
#' f <- function(u) {
#'   x <- u[1]; y <- u[2]
#'   c(sin(x) + y, x - sin(y))
#' }
#'
#' # Create the potential function heatmap
#' ggplot() + geom_potential(fun = f)
#' ggplot() + geom_potential(fun = f, verify_conservative = TRUE)
#'
#' @export
geom_potential <- function(
  mapping = NULL,
  data = NULL,
  stat = StatPotential,
  position = "identity",
  ...,
  na.rm = FALSE,
  inherit.aes = TRUE,
  show.legend = NA,
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 51,
  tol = 1e-6,
  verify_conservative = FALSE) {

  if (is.null(data)) {
    data <- data.frame(x = NA_real_, y = NA_real_)
  }

  # Ensure the potential function is provided
  if (missing(fun)) {
    stop("Parameter `fun` must be provided to compute the potential function.")
  }

  # Default aesthetic mapping for fill
  default_mapping <- aes(fill = after_stat(potential))

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
      tol = tol,
      verify_conservative = verify_conservative,
      ...
    )
  )
}

#' @rdname geom_potential
#' @export
stat_potential <- function(
  mapping = NULL,
  data = NULL,
  geom = GeomPotential,
  position = "identity",
  ...,
  na.rm = FALSE,
  inherit.aes = TRUE,
  show.legend = NA,
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 51,
  tol = 1e-6,
  verify_conservative = FALSE
  ) {

  if (is.null(data)) {
    data <- data.frame(x = NA_real_, y = NA_real_)
  }

  # Ensure the potential function is provided
  if (missing(fun)) {
    stop("Parameter `fun` must be provided to compute the potential function.")
  }

  # Default aesthetic mapping for fill
  default_mapping <- aes(fill = after_stat(potential))

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
      tol = tol,
      verify_conservative = verify_conservative,
      ...
    )
  )
}



#' @rdname geom_potential
#' @format NULL
#' @usage NULL
#' @export
StatPotential <- ggproto(
  "StatPotential",
  Stat,

  # required_aes = c("x", "y"),

  default_aes = aes(fill = after_stat(potential)),

  compute_group = function(data, scales, fun = NULL, xlim = NULL, ylim = NULL,
                           n, tol = 1e-6, verify_conservative = FALSE, ...) {

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
    if (verify_conservative) {

      grid_data$curl <- apply(
        grid_data[, c("x", "y")], 1, verify_potential, fun = fun, tol = tol
      )

      if (any(!grid_data$curl)) {
        cli::cli_warn(c(
          "!" = "The provided vector field does not have a potential function everywhere within the specified domain.",
          ">" = "Ensure that the vector field satisfies the necessary conditions for a potential function."
        ))
      }

    }


    # Apply the numerical potential computation to all points
    grid_data$potential <- apply(
      grid_data[, c("x", "y")],  1, compute_potential,
      fun =  fun, x0 = xlim[1], y0 =  ylim[1]
    )

    # Remove points where potential couldn't be computed
    grid_data <- grid_data[!is.na(grid_data$potential), ]

    return(grid_data)
  }
)

#' @rdname geom_potential
#' @format NULL
#' @usage NULL
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





compute_potential <- function(point, fun, x0, y0) {
  x <- point[1]
  y <- point[2]

  # Path 1: Integrate F_x from x0 to x with y = y0
  F_x_func <- function(s) {
    sapply(s, function(si) fun(c(si, y0))[1])
  }

  # Perform the integration for F_x
  integral_x <- integrate(F_x_func, lower = x0, upper = x)$value

  # Path 2: Integrate F_y from y0 to y with x = x
  F_y_func <- function(t) {
    sapply(t, function(ti) fun(c(x, ti))[2])
  }

  # Perform the integration for F_y
  integral_y <- integrate(F_y_func, lower = y0, upper = y)$value

  # Sum the integrals to get the potential
  f_xy <- integral_x + integral_y

  return(f_xy)
}





# Verify if the vector field is conservative
verify_potential <- function(point, fun, tol) {
  # Compute the Jacobian matrix numerically

  jacobian_matrix <- numDeriv::jacobian(fun, point)

  # Extract partial derivatives
  df1_dy <- jacobian_matrix[1, 2]  # ∂F_x/∂y
  df2_dx <- jacobian_matrix[2, 1]  # ∂F_y/∂x

  # Check if df1_dy is approximately equal to df2_dx
  symmetric <- abs(df1_dy - df2_dx) <= tol

  return(symmetric)
}


