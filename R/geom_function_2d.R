#' Create a 2D Function Visualization Layer of the Norm of a Vector Field
#'
#' `geom_function_2d` adds a layer to visualize 2D functions or vector fields in a `ggplot2` plot.
#'
#' @param mapping Aesthetic mappings, created using `aes()`. If `NULL`, defaults are used.
#' @param data Optional data frame to override the default data source.
#' @param stat Statistical transformation to use. Defaults to `StatFunction2d`.
#' @param geom Geom used for rendering. Defaults to `GeomFunction2d`.
#' @param ... Other arguments passed to the layer, such as additional parameters.
#' @param position Position adjustment for the layer. Defaults to `"identity"`.
#' @param fun A function that takes a matrix of x, y values and returns a matrix of dx, dy values.
#' @param xlim Numeric vector of length 2 specifying the x-range of the grid. Required if `fun` is provided.
#' @param ylim Numeric vector of length 2 specifying the y-range of the grid. Required if `fun` is provided.
#' @param n Number of points in the grid along each axis. Defaults to `11` in `stat_function_2d`.
#' @param na.rm Logical. Should missing values be removed? Defaults to `FALSE`.
#' @param show.legend Logical. Should this layer be included in the legends? `NA` includes if aesthetics are mapped.
#' @param inherit.aes If `FALSE`, overrides default aesthetics rather than combining them.
#'
#' @return A `ggplot2` layer.
#' @export
#'
#' @examples
#' f <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(y, -x)
#' }
#'
#' ggplot() +
#'   geom_function_2d(fun = f, xlim = c(-5, 5), ylim = c(-5, 5))
#'
#' ggplot() +
#'   geom_function_2d(fun = f, xlim = c(-5, 5), ylim = c(-5, 5)) +
#'   geom_vector_field(fun = f, xlim = c(-5, 5), ylim = c(-5, 5))



geom_function_2d <- function(mapping = NULL, data = NULL,
                              stat = StatFunction2d, geom = GeomFunction2d,
                              ...,
                              position = "identity",
                              fun = NULL,
                              xlim = NULL,
                              ylim = NULL,
                              n = NULL,
                              show.legend = NA,
                              inherit.aes = TRUE) {

  # If no data and no x,y aesthetics are specified, but we have fun, xlim, ylim,
  # we need dummy data to trigger compute_group().
  if (is.null(data) && !is.null(fun) && !is.null(xlim) && !is.null(ylim)) {
    data <- data.frame(x = NA_real_, y = NA_real_)
  }

  # Pass the parameters via `params` only
  layer(
    stat = stat,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      ...
    )
  )
}




#' @rdname geom_function_2d
#' @export
stat_function_2d <- function(mapping = NULL, data = NULL,
                              geom = GeomFunction2d, position = "identity",
                              ...,
                              na.rm = FALSE,
                              show.legend = NA,
                              inherit.aes = TRUE,
                              fun = NULL,
                              xlim = c(-1, 1),
                              ylim = c(-1, 1),
                              n = 11) {

  # Pass the parameters via `params` only
  layer(
    stat = StatFunction2d,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      ...
    )
  )
}


#' @rdname geom_function_2d
#' @export
StatFunction2d <- ggproto(
  "StatFunction2d",
  Stat,
  # required_aes = character(0), # No required aesthetics to allow flexibility
  default_aes = aes(x = NA, y = NA, fill = "black", alpha = 1),

  compute_group = function(data, scales, fun, xlim = NULL, ylim = NULL, n = NULL, ...) {

    # Scenario: Using a function to generate the vector field
    if (!is.null(fun)) {
      # If xlim and ylim provided, generate grid from those
      # If not provided, try to infer from data
      if (is.null(xlim) || is.null(ylim)) {
        if (nrow(data) > 0 && all(c("x", "y") %in% names(data))) {
          xlim <- xlim %||% range(data$x, na.rm = TRUE)
          ylim <- ylim %||% range(data$y, na.rm = TRUE)
        } else {
          stop("When using `fun` without specifying aes `x, y` from data, you must supply `xlim` and `ylim` or specify `x, y` using aes()")
        }
      }

      if (is.null(n)) n <- 11

      data <- expand.grid(
        x = seq(xlim[1], xlim[2], length.out = n),
        y = seq(ylim[1], ylim[2], length.out = n)
      )

      vectors <- vectorize(fun)(as.matrix(data))

      data$dx <- vectors[, 1]
      data$dy <- vectors[, 2]

      # # Compute divergence and curl
      # grad <- apply(data[, c("x", "y")], 1, function(v) numDeriv::grad(fun, v)) |> t()
      # grad_u <- grad[, 1]
      # grad_v <- grad[, 2]
      #
      # data$divergence <- grad_u + grad_v
      # data$curl <- grad_v - grad_u

    } else {
      # fun is NULL, expecting user-provided data with x,y and dx,dy or angle/distance
      if (!all(c("x", "y") %in% names(data))) {
        stop("`stat_vector()` requires `x` and `y` aesthetics or a `fun` with `xlim`/`ylim`.")
      }

      # If dx/dy are missing, try angle/distance
      if ((all(is.na(data$dx)) || all(is.na(data$dy))) &&
          (!is.na(data$distance[1]) && !is.na(data$angle[1]))) {
        data$dx <- data$distance * cos(data$angle)
        data$dy <- data$distance * sin(data$angle)
      }

      # Check again if dx,dy are available
      if (all(is.na(data$dx)) | all(is.na(data$dy))) {
        stop("Either dx/dy or distance/angle must be provided.")
      }
    }

    data$xend <- data$x + data$dx
    data$yend <- data$y + data$dy
    data$norm <- sqrt(data$dx^2 + data$dy^2)

    data <- data.frame(x = data$x, y = data$y, norm = data$norm)

    data
  }
)


#' @rdname geom_function_2d
#' @export
GeomFunction2d <- ggproto(
  "GeomFunction2d",
  Geom,

  default_aes = aes(color = "black", alpha = 1),

  draw_group = function(data, panel_params, coord, na.rm = FALSE, ...) {
    coords <- coord$transform(data, panel_params)

    # Determine grid dimensions from unique x and y values
    unique_x <- sort(unique(coords$x))
    unique_y <- sort(unique(coords$y))
    nrow_val <- length(unique_x)
    ncol_val <- length(unique_y)

    # Extract color and alpha from aesthetics
    base_color <- coords$colour[1] %||% "black"
    alpha_val <- coords$alpha[1]

    # Create a palette from white to the user-specified color
    pal <- grDevices::colorRampPalette(c("white", base_color))
    n_colors <- 256
    color_levels <- pal(n_colors)

    # Map norm values to indices in the color palette
    norm_indices <- round(scales::rescale(coords$norm, to = c(1, n_colors)))
    norm_indices[is.na(norm_indices)] <- 1

    # Create a matrix of colors corresponding to the norm values
    image_matrix <- matrix(
      color_levels[norm_indices],
      nrow = nrow_val,
      ncol = ncol_val,
      byrow = TRUE
    )

    raster_grob <- grid::rasterGrob(
      image = image_matrix,
      x = unit(0.5, "npc"),   # Centered in the plot space
      y = unit(0.5, "npc"),
      width = unit(1, "npc"),
      height = unit(1, "npc"),
      interpolate = TRUE,
      gp = grid::gpar(alpha = alpha_val)
    )

    raster_grob
  }
)

# GeomFunction2d <- ggproto(
#   "GeomFunction2d",
#   Geom,
#
#   default_aes = aes(color = "black", alpha = 1),
#
#   draw_group = function(data, panel_params, coord, na.rm = FALSE, ...){
#
#     coords <- coord$transform(data, panel_params)
#
#     image_matrix <- matrix(scales::rescale(coords$norm), nrow = length(unique(coords$x)), ncol = length(unique(coords$y)))
#
#     raster_grob <- grid::rasterGrob(
#       image = image_matrix,
#       x = unit(0.5, "npc"), # Centered in the plot space
#       y = unit(0.5, "npc"),
#       width = unit(1, "npc"),
#       height = unit(1, "npc"),
#       interpolate = TRUE,
#       gp = grid::gpar(alpha = coords$alpha[1])
#     )
#
#     return(raster_grob)
#
#   }
#
# )
