#' Create a Vector Field Plot Layer
#'
#' `geom_vector_field` generates a vector field plot layer using a
#' user-defined function to compute the vector components. This is particularly
#' useful for visualizing vector fields in a two-dimensional space.
#' @inheritParams ggplot2::geom_raster
#' @inheritParams ggplot2::stat_identity
#' @param fun A user-defined function that takes two arguments (x and y
#'   coordinates) and returns a list of two components: the x and y components
#'   of the vector field.
#' @param xlim,ylim A numeric vector of length 2 giving the x/y-axis limits.
#' @param n An integer specifying the number of grid points along each axis.
#' @param v Numeric vector specifying the direction vector components for
#'   calculating the directional derivative.
#' @param center Logical; if TRUE, centers the vector on the evaluated x/y
#'   location. If FALSE, the vector origin is on the evaluated x/y location.
#' @param normalize Logical; if TRUE, normalizes the vector to a length of unit
#'   1 and scales them to avoid overplotting based on grid density and plot
#'   range.
#' @param scale_length Numeric; a scaling factor applied to the vectors to
#'   adjust their length relative to the grid spacing. Defaults to 1.
#' @param arrow Arrow specification, as created by `grid::arrow()`.
#' @return A ggplot2 layer that can be added to a ggplot object to produce a
#'   vector field plot.
#' @name geom_vector_field
#' @rdname geom_vector_field
#' @examples
#'
#' # example user-defined function
#' f <- function(v) {
#'   x <- v[1]; y <- v[2]
#'   c(-y, x)
#' }
#'
#' # create a ggplot with the vector field layer
#' ggplot() +
#'   geom_vector_field(
#'     fun = f, xlim = c(-10, 10), ylim = c(-10, 10)
#'   )
#'
#' # various options
#' ggplot() +
#'   geom_vector_field(
#'     aes(color = after_stat(norm)),
#'     fun = f, xlim = c(-10, 10), ylim = c(-10, 10),
#'     arrow = arrow(length = unit(1, "mm"))
#'   )
#'
#' ggplot() +
#'   geom_vector_field(
#'     fun = f, xlim = c(-10, 10), ylim = c(-10, 10),
#'     n = 10, center = FALSE, normalize = FALSE,
#'     arrow = arrow(length = unit(1, "mm"))
#'   )
#'
#' ggplot() +
#'   geom_vector_field(
#'     fun = f, xlim = c(-10, 10), ylim = c(-10, 10),
#'     center = TRUE, color = "black",
#'     scale_length = .7
#'   )
#'
#' @section Computed variables:
#'
#'   \describe{
#'     \item{norm}{The magnitude of the vector field, computed as \eqn{\|\mathbf{w}\| = \sqrt{u^2 + v^2}}.}
#'     \item{divergence}{The divergence of the vector field, computed as \eqn{\nabla \cdot \mathbf{F} = \frac{\partial f_x}{\partial x} + \frac{\partial f_y}{\partial y}}.}
#'     \item{curl}{The curl of the vector field, computed as \eqn{\text{curl} \, \mathbf{F} = \frac{\partial f_y}{\partial x} - \frac{\partial f_x}{\partial y}}.}
#'     \item{laplacian}{The Laplacian of the vector field, computed as \eqn{\Delta \mathbf{F} = \frac{\partial^2 f_x}{\partial x^2} + \frac{\partial^2 f_x}{\partial y^2} + \frac{\partial^2 f_y}{\partial x^2} + \frac{\partial^2 f_y}{\partial y^2}}.}
#'     \item{directional_derivative}{The directional derivative of the vector field, computed as \eqn{D_{\mathbf{v}} \mathbf{F} = \frac{\partial F_x}{\partial x} v_x + \frac{\partial F_x}{\partial y} v_y + \frac{\partial F_y}{\partial x} v_x + \frac{\partial F_y}{\partial y} v_y}.}
#'   }
NULL

#' @rdname geom_vector_field
#' @export
geom_vector_field <- function(mapping = NULL, data = NULL, stat = "vectorfield",
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, fun, xlim, ylim, v = c(1, 2), n = 16,
                              center = TRUE, normalize = TRUE,
                              scale_length = 1.5,
                              arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                              ...) {
  if (is.null(data)) data <- ensure_nonempty_data(data)

  length_var <- extract_after_stat(mapping, "length")

  layer(
    stat = StatVectorField,
    data = data,
    mapping = mapping,
    geom = GeomVectorField,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      v = v,
      center = center,
      normalize = normalize,
      scale_length = scale_length,
      arrow = arrow,
      na.rm = na.rm,
      length_var = length_var,
      ...
    )
  )
}

#' @rdname geom_vector_field
#' @export
stat_vector_field <- function(mapping = NULL, data = NULL, geom = "segment",
                              position = "identity", na.rm = FALSE,
                              show.legend = NA, inherit.aes = TRUE,
                              fun, xlim, ylim, v = c(1, 2), n = 16,
                              center = TRUE, normalize = TRUE,
                              scale_length = 1.5,
                              arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                              ...) {
  if (is.null(data)) data <- ensure_nonempty_data(data)

  length_var <- extract_after_stat(mapping, "length")

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
      v = v,
      center = center,
      normalize = normalize,
      scale_length = scale_length,
      arrow = arrow,
      na.rm = na.rm,
      length_var = length_var,
      ...
    )
  )
}


#' @rdname geom_vector_field
#' @export
StatVectorField <- ggproto("StatVectorField", Stat,

                           default_aes = aes(color = after_stat(norm)),

                           compute_group = function(data, scales, fun, xlim, ylim, v = c(1, 2), n, center, normalize, scale_length, length_var = NULL, ...) {

                             # Create a sequence of x and y values within the limits
                             grid <- expand.grid(
                               x = seq(xlim[1], xlim[2], length.out = n),
                               y = seq(ylim[1], ylim[2], length.out = n)
                             ) |> as.matrix()
                             vectors <- vectorize(fun)(grid)

                             # Create a data frame for geom_segment
                             data <- data.frame(
                               x = grid[, 1],
                               y = grid[, 2],
                               u = vectors[, 1],
                               v = vectors[, 2]
                             )

                             # Calculate norm (magnitude) of the vectors
                             data$norm <- sqrt(data$u ^ 2 + data$v ^ 2)

                             grad <- apply(grid, 1, numDeriv::grad, func = fun) |> t()
                             grad_u <- grad[, 1]
                             grad_v <- grad[, 2]

                             # Divergence
                             data$divergence <- grad_u + grad_v

                             # Curl
                             data$curl <- grad_v - grad_u

                             # Laplacian
                             hess_u <- apply(grid, 1, compute_laplacian, f = extract_component_function(f = fun, 1))
                             hess_v <- apply(grid, 1, compute_laplacian, f = extract_component_function(f = fun, 2))
                             data$laplacian <- hess_u + hess_v

                             # Directional Derivative
                             vx <- v[1]; vy <- v[2]
                             data$directional_derivative <- grad %*% (c(vx, vy) / sqrt(vx ^ 2 + vy ^ 2))

                             # Calculate value to scale vectors if needed
                             scale_values <- if (!is.null(length_var) && length_var %in% colnames(data)) {
                               data[[length_var]]
                             } else {
                               1
                             }

                             # message(length_var)
                             if (normalize) {
                               # Calculate the spacing between grid points
                               x_spacing <- (xlim[2] - xlim[1]) / (n - 1)
                               y_spacing <- (ylim[2] - ylim[1]) / (n - 1)
                               spacing <- min(x_spacing, y_spacing)

                               # if(!is.null(length_var))
                               scale_values <- scales::rescale(scale_values, c(0,1))

                               # Normalize the vectors based on the length aesthetic or scale values
                               data$u <- data$u / data$norm * scale_values * scale_length * spacing
                               data$v <- data$v / data$norm * scale_values * scale_length * spacing
                             } else{ ## this scales the vectors by the length_var
                               data$u <- data$u * scale_values
                               data$v <- data$v * scale_values
                               # data$u <- data$u / data$norm * scale_values
                               # data$v <- data$v / data$norm * scale_values
                             }

                             # Scale arrow size based on the length aesthetic
                             data$arrow_size <- ifelse(data$norm < 0.005, 0.005, 0.015)

                             if (center) {
                               # Calculate the half-length of the vectors
                               half_u <- data$u / 2
                               half_v <- data$v / 2

                               # Calculate the end points of the vectors
                               data$xend <- data$x + half_u
                               data$yend <- data$y + half_v

                               # Calculate the start points of the vectors
                               data$x <- data$x - half_u
                               data$y <- data$y - half_v
                             } else {
                               # Calculate the end points of the vectors
                               data$xend <- data$x + data$u
                               data$yend <- data$y + data$v
                             }

                             # data$color <- as.numeric(data$norm)

                             # print(head(data))

                             data
                           }
)



#' @rdname geom_vector_field
#' @export
GeomVectorField <- ggproto("GeomVectorField", GeomSegment,
                           draw_key = draw_key_length,

                           required_aes = c("x", "y", "xend", "yend"),

                           default_aes = aes(colour = "black", linewidth = 0.5, linetype = 1, alpha = 1, length = 1, arrow_size = 1),

                           setup_data = function(data, params) {
                             data$length <- data$length %||% 1
                             data
                           },

                           draw_panel = function(data, panel_params, coord, arrow = NULL, arrow_size = 1) {
                             data$arrow_size <- data$arrow_size %||% arrow_size
                             arrow <- modifyList(arrow, list(length = unit(data$arrow_size, "npc")))
                             GeomSegment$draw_panel(data, panel_params, coord, arrow = arrow)
                           }
)



#' Continuous Scale for Vector Length
#'
#' `scale_length_continuous` provides a continuous scale for the length
#' aesthetic in `geom_vector_field`.
#' @param name The name of the scale.
#' @param n.breaks The number of breaks.
#' @param guide The guide to be used for this scale, such as a legend or color bar.
#' Can be a string specifying the type of guide, or a guide object returned by
#' a call to a guide function (e.g., `guide_legend` or `guide_colorbar`). Use `"none"`
#' to hide the guide.
#' @param ... Additional parameters passed on to `continuous_scale`.
#' @export
#' @examples
#' f <- function(v) {
#'   x <- v[1]; y <- v[2]
#'   c(-y, x)
#' }
#' ggplot() +
#'   geom_vector_field(aes(length = after_stat(norm)), fun = f, xlim = c(-10, 10), ylim = c(-10, 10)) +
#'   scale_length_continuous()
scale_length_continuous <- function(name = waiver(), n.breaks = waiver(), guide = "none", ...) {
  breaks <- pretty
  labels <- function(x) format(x, scientific = FALSE)
  continuous_scale(
    aesthetics = "length", palette = identity,
    name = name, breaks = function(x) rev(breaks(x)), labels = labels, guide = guide, ...
  )
}
