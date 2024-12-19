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
#' @param x_lim,y_lim Numeric vectors of length 2 defining the domain limits on the x/y-axis.
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
#' x_lim <- c(7, 10)
#' y_lim <- c(7, 10)
#'
#' # Create the potential function heatmap
#' ggplot() +
#'   geom_potential(fun = fun, x_lim = x_lim, y_lim = y_lim)
#'
#' @export
geom_potential <- function(mapping = NULL, data = NULL,
                           stat = StatPotential, geom = GeomPotential,
                           ...,
                           inherit.aes = TRUE,
                           show.legend = NA,
                           position = "identity",
                           fun,
                           x_lim = NULL,
                           y_lim = NULL,
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
      x_lim = x_lim,
      y_lim = y_lim,
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

  compute_group = function(data, scales, fun = NULL, x_lim = NULL, y_lim = NULL, n = 10, tolerance = 1e-6, ...) {
    # Ensure the vector field function is provided
    if (is.null(fun)) {
      stop("Parameter `fun` must be provided to compute the potential function.")
    }

    # Verify if the vector field is conservative
    verify_potential <- function(point, tolerance = tolerance) {
      # Compute the Jacobian matrix numerically
      jacobian_matrix <- jacobian(fun, point)

      # Extract partial derivatives
      df1_dy <- jacobian_matrix[1, 2]  # ∂F_x/∂y
      df2_dx <- jacobian_matrix[2, 1]  # ∂F_y/∂x

      # Check if df1_dy is approximately equal to df2_dx
      symmetric <- abs(df1_dy - df2_dx) <= tolerance

      return(symmetric)
    }

    # Generate grid if not provided
    if (is.null(x_lim) || is.null(y_lim)) {
      if (nrow(data) > 0 && all(c("x", "y") %in% names(data))) {
        x_lim <- range(data$x, na.rm = TRUE)
        y_lim <- range(data$y, na.rm = TRUE)
      } else {
        stop("When using `StatPotential`, you must supply `x_lim` and `y_lim` if data is not provided.")
      }
    }

    # Generate grid
    grid_data <- expand.grid(
      x = seq(x_lim[1], x_lim[2], length.out = n),
      y = seq(y_lim[1], y_lim[2], length.out = n)
    )

    # Verify if the vector field is conservative
    grid_data$curl <- apply(grid_data[, c("x", "y")], 1, verify_potential)

    if (any(!grid_data$curl)) {
      warning("The provided vector field does not have a potential function everywhere within the specified domain.")
    }

    # Apply the numerical potential computation to all points
    grid_data$Potential <- apply(grid_data[, c("x", "y")], 1, compute_potential)

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

  # required_aes = c("x", "y", "fill"),

  default_aes = aes(
    alpha = 1
  ),

  draw_panel = function(data, panel_params, coord, ...) {
    GeomRaster$draw_panel(data, panel_params, coord, ...)
  },

  draw_key = draw_key_rect
)

