#' Create a Gradient Smoothed Field Layer
#'
#' `geom_gradient_smooth()` creates a ggplot2 layer that visualizes the gradient
#' of a scalar field computed from raw data. A linear model is fitted using the
#' supplied `formula` (default: `z ~ x + y + I(x^2) + I(y^2)`) on the raw data,
#' and the numerical gradient is computed using [numDeriv::grad()]. The computed
#' gradient field is then visualized using [GeomStream()].
#'
#' @inheritParams geom_stream_smooth
#' @param formula A formula specifying the linear model for the scalar field.
#'   Defaults to `z ~ x + y + I(x^2) + I(y^2)`.
#' @param max_it Maximum number of iterations for field integration (when used in streamlines).
#' @param T If `normalize = FALSE`, this controls the time length for growing streams.
#' @param L If `normalize = TRUE`, this controls the fixed length of streams or vectors.
#' @param tail_point Logical. If `TRUE`, draws the tail point of vectors/streams (default: `FALSE`).
#' @param eval_point Logical. If `TRUE`, marks the evaluation points used to fit gradients.
#' @param grid A user-supplied data frame or pattern (e.g., "hex") for specifying custom evaluation points.
#' @param ... Additional arguments passed to the layer.
#'
#' @section Aesthetics:
#' `geom_gradient_smooth()` supports the following aesthetics (required aesthetics are in **bold**):
#'
#'   - **`x`**: The x-coordinate of the data point.
#'   - **`y`**: The y-coordinate of the data point.
#'   - **`z`**: The scalar value used for computing the gradient.
#'   - `color`: The color used for the gradient vectors. Defaults depend on the
#'     selected `type`.
#'
#' @section Details:
#' **Gradient Calculation:**
#' A linear model is fitted using the provided `formula` and the raw data. The scalar
#' field defined by the model is then differentiated numerically with
#' [numDeriv::grad()] to yield gradient vectors.
#'
#' **Visualization:**
#' The resulting gradient field is visualized using [GeomStream()]. Since `z` is only
#' used internally, it is dropped from the final visual output.
#'
#' @return A ggplot2 layer that can be added to a ggplot object.
#'
#' @examples
#' \dontrun{
#' # Define several scalar field functions:
#'
#' # Example 1: f(x, y) = x^2 - y^2
#' f <- function(u) {
#'   x <- u[1]
#'   y <- u[2]
#'   x^2 - y^2
#' }
#'
#' # Example 2: g(x, y) = sin(x) * cos(y)
#' g <- function(u) {
#'   x <- u[1]
#'   y <- u[2]
#'   sin(x) * cos(y)
#' }
#'
#' # Example 3: h(x, y) = log(|x| + 1) + sqrt(|y|)
#' h <- function(u) {
#'   x <- u[1]
#'   y <- u[2]
#'   log(abs(x) + 1) + sqrt(abs(y))
#' }
#'
#' # Create a grid of evaluation points
#' grid_data <- expand.grid(
#'   x = seq(-5, 5, length.out = 30),
#'   y = seq(-5, 5, length.out = 30)
#' )
#'
#' # Compute the scalar field for f and plot its gradient
#' grid_data$z <- apply(grid_data, 1, f)
#'
#' ggplot(grid_data, aes(x = x, y = y, z = z)) +
#'   geom_gradient_smooth()
#'
#' # Compute and plot for g:
#' grid_data$z <- apply(grid_data, 1, g)
#' ggplot(grid_data, aes(x = x, y = y, z = z)) +
#'   geom_gradient_smooth()
#'
#' # Compute and plot for h:
#' grid_data$z <- apply(grid_data, 1, h)
#' ggplot(grid_data, aes(x = x, y = y, z = z)) +
#'   geom_gradient_smooth()
#' }
#' @name geom_gradient_smooth
#' @aliases geom_gradient_smooth stat_gradient_smooth
#' @export
NULL

#' @rdname geom_gradient_smooth
#' @export
geom_gradient_smooth <- function(
    mapping = NULL,
    data = NULL,
    stat = StatStreamField,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = TRUE,
    inherit.aes = TRUE,
    formula = z ~ x + y + I(x^2) + I(y^2),
    xlim = NULL,
    ylim = NULL,
    n = 11,
    max_it = 1000,
    T = NULL,
    L = NULL,
    center = TRUE,
    type = "vector",
    normalize = TRUE,
    tail_point = FALSE,
    eval_point = FALSE,
    grid = NULL,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    arrow = grid::arrow(angle = 30, length = grid::unit(0.02, "npc"), type = "closed")
) {

  n <- ensure_length_two(n)

  default_aes <- aes(x = x, y = y, z = z)

  if (is.null(mapping)) {
    mapping <- default_aes
  } else {
    mapping <- modifyList(default_aes, mapping)
  }

  default_color_aes <- if (type == "stream") {
    aes(color = after_stat(avg_spd))
  } else if (type == "vector") {
    aes(color = after_stat(norm))
  } else {
    cli::cli_abort("`type` must be either 'stream' or 'vector', not {.val {type}}")
  }

  user_fixed_color <- any(c("color", "colour") %in% names(list(...)))
  if (!user_fixed_color) {
    mapping <- modifyList(mapping, default_color_aes)
  }

  gradient_field <- function(u) {
    group_data <- try(get("data", envir = parent.frame()), silent = TRUE)
    local_scalar_field <- function(u) {
      model <- lm(formula, data = group_data)
      newdata <- data.frame(x = u[1], y = u[2])
      as.numeric(predict(model, newdata = newdata))
    }

    # Compute the numerical gradient.
    numDeriv::grad(func = local_scalar_field, x = u)
  }

  layer(
    stat = stat,
    geom = GeomStream,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = gradient_field,
      xlim = xlim,
      ylim = ylim,
      n = n,
      na.rm = na.rm,
      max_it = max_it,
      T = T,
      L = L,
      center = center,
      type = type,
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      grid = grid,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname geom_gradient_smooth
#' @export
stat_gradient_smooth <- function(
    mapping = NULL,
    data = NULL,
    geom = GeomStream,
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = TRUE,
    inherit.aes = TRUE,
    formula = z ~ x + y + I(x^2) + I(y^2),
    xlim = NULL,
    ylim = NULL,
    n = 11,
    max_it = 1000,
    T = NULL,
    L = NULL,
    center = TRUE,
    type = "vector",
    normalize = TRUE,
    tail_point = FALSE,
    eval_point = FALSE,
    grid = NULL,
    lineend = "butt",
    linejoin = "round",
    linemitre = 10,
    arrow = grid::arrow(angle = 30, length = grid::unit(0.02, "npc"), type = "closed")
) {

  n <- ensure_length_two(n)

  default_aes <- aes(x = x, y = y, z = z)

  if (is.null(mapping)) {
    mapping <- default_aes
  } else {
    mapping <- modifyList(default_aes, mapping)
  }

  default_color_aes <- if (type == "stream") {
    aes(color = after_stat(avg_spd))
  } else if (type == "vector") {
    aes(color = after_stat(norm))
  } else {
    cli::cli_abort("`type` must be either 'stream' or 'vector', not {.val {type}}")
  }

  user_fixed_color <- any(c("color", "colour") %in% names(list(...)))
  if (!user_fixed_color) {
    mapping <- modifyList(mapping, default_color_aes)
  }

  gradient_field <- function(u) {
    group_data <- try(get("data", envir = parent.frame()), silent = TRUE)

    local_scalar_field <- function(u) {
      model <- lm(formula, data = group_data)
      newdata <- data.frame(x = u[1], y = u[2])
      as.numeric(predict(model, newdata = newdata))
    }

    # Compute the numerical gradient.
    numDeriv::grad(func = local_scalar_field, x = u)
  }

  layer(
    stat = StatStreamField,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = gradient_field,
      xlim = xlim,
      ylim = ylim,
      n = n,
      na.rm = na.rm,
      max_it = max_it,
      T = T,
      L = L,
      center = center,
      type = type,
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      grid = grid,
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow = arrow,
      ...
    )
  )
}
