#' Gradient Field Layers for ggplot2
#'
#' These functions provide convenient ggplot2 layers for drawing gradient fields by computing the
#' gradient of a scalar field. A user-defined function (`fun`) specifies the behavior of the scalar
#' field by taking a numeric vector of length 2 (representing \eqn{(x, y)}) and returning a single numeric
#' value. The underlying [StatStreamField] computes the gradient via numerical differentiation (using
#' [numDeriv::grad()]) and [GeomStream] renders the resulting vectors.
#'
#' Two variants are provided:
#'
#' - **geom_gradient_field()** uses a default mapping that sets `color = after_stat(norm)`.
#' - **geom_gradient_field2()** uses a default mapping that sets `length = after_stat(norm)` (with `color`
#'   unmapped by default).
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams geom_stream
#'
#' @param mapping A set of aesthetic mappings created by [ggplot2::aes()]. Additional aesthetics such as
#'   `color`, `size`, `linetype`, and `alpha` can be defined. In **geom_gradient_field** the default mapping
#'   includes `color = after_stat(norm)`, whereas in **geom_gradient_field2** the default mapping includes
#'   `length = after_stat(norm)`.
#' @param data A data frame containing the input data.
#' @param stat The statistical transformation to use on the data for this layer. Defaults to [StatStreamField].
#' @param position Position adjustment, either as a string or the result of a call to a position adjustment function.
#' @param na.rm Logical. If `FALSE` (the default), missing values are removed with a warning.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes Logical. If `FALSE`, overrides the default aesthetics rather than combining with them.
#' @param fun A function that defines the scalar field. It should take a numeric vector of length 2 (representing
#'   \eqn{(x, y)}) and return a single numeric value. **(Required)**
#' @param xlim Numeric vector of length two. Specifies the limits of the x-axis domain. Defaults to `c(-1, 1)`.
#' @param ylim Numeric vector of length two. Specifies the limits of the y-axis domain. Defaults to `c(-1, 1)`.
#' @param n Integer. Grid resolution specifying the number of seed points along each axis. Higher values produce a
#'   denser gradient field. Defaults to `11`.
#' @param center Logical. If `TRUE`, centers the seed points so that the original (x, y) becomes the midpoint.
#'   Defaults differ between the variants.
#' @param normalize Logical. If `TRUE`, gradient vectors are normalized based on grid spacing. If `TRUE`,
#'   it is converted internally to `"vector"`. Default is `TRUE`.
#' @param tail_point Logical. If `TRUE`, a point is drawn at the tail of each gradient vector.
#'   Defaults differ between the variants.
#' @param eval_point Logical. If `TRUE`, a point is drawn at the evaluation point where the gradient was computed.
#'   Default is `FALSE`.
#' @param arrow A [grid::arrow()] specification to add arrowheads to the gradient vectors.
#'   In **geom_gradient_field** the default is a closed arrow with a 30° angle and length `0.02` npc;
#'   in **geom_gradient_field2** the default is `NULL`.
#' @param ... Other arguments passed on to [ggplot2::layer()].
#'
#' @return A ggplot2 layer that computes and plots a gradient field by numerically differentiating a scalar field.
#'
#' @examples
#' # Define a simple paraboloid scalar field function
#' paraboloid_field <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   x^2 + y^2
#' }
#'
#' # Using geom_gradient_field()
#' ggplot() +
#'   geom_gradient_field(fun = paraboloid_field)
#'
#' # Using geom_gradient_field2()
#' ggplot() +
#'   geom_gradient_field2(fun = paraboloid_field)
#'
#' @name geom_gradient_field
#' @aliases geom_gradient_field stat_gradient_field geom_gradient_field2 stat_gradient_field2
#' @export
NULL
geom_gradient_field <- function(mapping = NULL, data = NULL,
                                stat = StatStreamField,
                                position = "identity",
                                ...,
                                na.rm = FALSE,
                                show.legend = TRUE,
                                inherit.aes = TRUE,
                                fun,
                                xlim = NULL,
                                ylim = NULL,
                                n = 11,
                                center = TRUE,
                                normalize = TRUE,
                                tail_point = FALSE,
                                eval_point = FALSE,
                                arrow = grid::arrow(angle = 30,
                                                    length = grid::unit(0.02, "npc"),
                                                    type = "closed")) {
  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  default_mapping <- aes(color = after_stat(norm))

  if (!is.null(mapping)) {
    if (!"color" %in% names(mapping)) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    mapping <- default_mapping
  }

  if (missing(fun) || !is.function(fun)) {
    stop("Please provide a valid scalar function 'fun' that takes a numeric vector (x, y) and returns a single numeric value.")
  }

  gradient_fun <- function(v) {
    if (!is.numeric(v) || length(v) != 2) {
      stop("Input to the gradient function must be a numeric vector of length 2 (x, y).")
    }
    numDeriv::grad(func = fun, x = v)
  }
  if(normalize) normalize <- "vector"

  layer(
    stat = stat,
    geom = GeomStream,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = gradient_fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      method = "euler",
      na.rm = na.rm,
      dt = 1,
      max_it = 2,
      # L = 0.1,
      center = center,
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      arrow = arrow,
      ...
    )
  )
}


#' @rdname geom_gradient_field
#' @export
stat_gradient_field <- function(mapping = NULL, data = NULL,
                                geom = GeomStream,
                                position = "identity",
                                ...,
                                na.rm = FALSE,
                                show.legend = TRUE,
                                inherit.aes = TRUE,
                                fun,
                                xlim = NULL,
                                ylim = NULL,
                                n = 11,
                                center = TRUE,
                                normalize = TRUE,
                                tail_point = FALSE,
                                eval_point = FALSE,
                                arrow = grid::arrow(angle = 30,
                                                    length = grid::unit(0.02, "npc"),
                                                    type = "closed")) {

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  default_mapping <- aes(color = after_stat(norm))

  if (!is.null(mapping)) {
    if (!"color" %in% names(mapping)) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    mapping <- default_mapping
  }

  if (missing(fun) || !is.function(fun)) {
    stop("Please provide a valid scalar function 'fun' that takes a numeric vector (x, y) and returns a single numeric value.")
  }

  gradient_fun <- function(v) {
    if (!is.numeric(v) || length(v) != 2) {
      stop("Input to the gradient function must be a numeric vector of length 2 (x, y).")
    }
    numDeriv::grad(func = fun, x = v)
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
      fun = gradient_fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      method = "euler",
      na.rm = na.rm,
      dt = 1,
      # L = 0.1,
      center = center,
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname geom_gradient_field
#' @export
geom_gradient_field2 <- function(mapping = NULL, data = NULL,
                                stat = StatStreamField,
                                position = "identity",
                                ...,
                                na.rm = FALSE,
                                show.legend = TRUE,
                                inherit.aes = TRUE,
                                fun,
                                xlim = NULL,
                                ylim = NULL,
                                n = 11,
                                center = FALSE,
                                normalize = TRUE,
                                tail_point = TRUE,
                                eval_point = FALSE,
                                arrow = NULL) {
  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  default_mapping <- aes(length = after_stat(norm))

  if (!is.null(mapping)) {
    if (!"color" %in% names(mapping)) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    mapping <- default_mapping
  }

  if (missing(fun) || !is.function(fun)) {
    stop("Please provide a valid scalar function 'fun' that takes a numeric vector (x, y) and returns a single numeric value.")
  }

  gradient_fun <- function(v) {
    if (!is.numeric(v) || length(v) != 2) {
      stop("Input to the gradient function must be a numeric vector of length 2 (x, y).")
    }
    numDeriv::grad(func = fun, x = v)
  }
  if(normalize) normalize <- "vector"

  layer(
    stat = stat,
    geom = GeomStream,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = gradient_fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      method = "euler",
      na.rm = na.rm,
      dt = 1,
      max_it = 2,
      # L = 0.1,
      center = center,
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname geom_gradient_field
#' @export
stat_gradient_field2 <- function(mapping = NULL, data = NULL,
                                geom = GeomStream,
                                position = "identity",
                                ...,
                                na.rm = FALSE,
                                show.legend = TRUE,
                                inherit.aes = TRUE,
                                fun,
                                xlim = NULL,
                                ylim = NULL,
                                n = 11,
                                center = FALSE,
                                normalize = TRUE,
                                tail_point = TRUE,
                                eval_point = FALSE,
                                arrow = NULL) {
  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  default_mapping <- aes(length = after_stat(norm))

  if (!is.null(mapping)) {
    if (!"color" %in% names(mapping)) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    mapping <- default_mapping
  }

  if (missing(fun) || !is.function(fun)) {
    stop("Please provide a valid scalar function 'fun' that takes a numeric vector (x, y) and returns a single numeric value.")
  }

  gradient_fun <- function(v) {
    if (!is.numeric(v) || length(v) != 2) {
      stop("Input to the gradient function must be a numeric vector of length 2 (x, y).")
    }
    numDeriv::grad(func = fun, x = v)
  }
  if(normalize) normalize <- "vector"

  layer(
    stat = StatStreamField,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = gradient_fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      method = "euler",
      na.rm = na.rm,
      dt = 1,
      max_it = 2,
      # L = 0.1,
      center = center,
      normalize = normalize,
      tail_point = tail_point,
      eval_point = eval_point,
      arrow = arrow,
      ...
    )
  )
}
