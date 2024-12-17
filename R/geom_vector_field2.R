#' Create a Vector Field Plot Layer with Length Mapped to Norm
#'
#' `geom_vector_field2()` and `stat_vector_field2()` are extensions of `geom_vector_field()` and `stat_vector_field()`,
#' providing identical functionality but with different default aesthetics. These functions are particularly useful
#' for vector field visualizations where vector length is mapped to the vector's magnitude.
#'
#' **Default Behavior:**
#' - **Length** is automatically mapped to the vector norm (`length = after_stat(norm)`) so that vector magnitude is
#'   emphasized through the vector length in the plot.
#' - **Color** defaults to `NULL`, allowing the user to set a fixed color or another aesthetic if desired. By leaving
#'   `color` unset, it won't override the `geom_vector_field` default which maps `color = after_stat(norm)` unless you specify it.
#'
#' **Additional Parameters:**
#' - **center** is set to `FALSE` by default, so vectors start from the specified (`x`, `y`) coordinates rather than being centered.
#' - **tail_point** is set to `TRUE`, adding a small point at the tail of each vector to help indicate the starting point.
#' - **arrow** is set to `NULL` by default. Users can specify a custom arrow using `grid::arrow()` to add arrowheads.
#'
#' @inheritParams geom_vector_field
#' @param mapping Aesthetic mappings created by `aes()` or `aes_()`.
#'   These functions ensure that `length = after_stat(norm)` is mapped by default and leave `color` unset, allowing for user customization.
#' @param center Logical; if `TRUE`, centers the vectors on their grid points. Defaults to `FALSE` here.
#' @param tail_point Logical; if `TRUE`, a small point is added at the tail of each vector. Defaults to `TRUE`.
#' @param arrow Arrow specification created by `grid::arrow()`. Defaults to `NULL`.
#' @param ... Other arguments passed on to `geom_vector_field()` or `stat_vector_field()`.
#'
#' @return A `ggplot2` layer that can be added to a ggplot object to create a vector field plot.
#'
#' @examples
#' # Example user-defined vector field function
#' f <- function(v) {
#'   x <- v[1]; y <- v[2]
#'   c(x + y, y - x)
#' }
#'
#' # Create a vector field plot with geom_vector_field2:
#' ggplot() +
#'   geom_vector_field2(fun = f, x_lim = c(-5, 5), y_lim = c(-5, 5))
#'
#' @seealso
#' Use [geom_vector_field()] if you prefer to map vector magnitude using a different aesthetic such as `color`.
#'
#' @export
geom_vector_field2 <- function(
    mapping = NULL,
    data = NULL,
    stat = StatVector,
    geom = GeomVector,
    position = "identity",
    center = FALSE,
    normalize = TRUE,
    tail_point = TRUE,
    tail_point.size = 2,
    arrow = NULL,
    fun = NULL,
    x_lim = NULL,
    y_lim = NULL,
    n = NULL,
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {

  # Check if x and y are in the mapping
  mapping_defines_xy <- !is.null(mapping) && all(c("x", "y") %in% names(mapping))

  # If no data and no x,y aesthetics are specified, but fun, x_lim, y_lim are provided,
  # we need dummy data to trigger compute_group().
  if (is.null(data) && !mapping_defines_xy && !is.null(fun) && !is.null(x_lim) && !is.null(y_lim)) {
    data <- data.frame(x = NA_real_, y = NA_real_)
  }

  # Default aesthetics: length maps to norm, and color is left unset (NULL).
  base_mapping <- aes(length = after_stat(norm), color = NULL)

  # Merge user-provided mappings with the base defaults
  if (is.null(mapping)) {
    mapping <- base_mapping
  } else {
    mapping <- modifyList(base_mapping, mapping)
  }

  layer(
    stat = stat,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      center = center,
      normalize = normalize,
      tail_point = tail_point,
      tail_point.size = tail_point.size,
      arrow = arrow,
      fun = fun,
      x_lim = x_lim,
      y_lim = y_lim,
      n = n,
      ...
    )
  )
}

#' @rdname geom_vector_field2
#' @export
stat_vector_field2 <- function(
    mapping = NULL,
    data = NULL,
    geom = GeomVector,
    stat = StatVector,
    position = "identity",
    center = FALSE,
    normalize = TRUE,
    tail_point = TRUE,
    tail_point.size = 2,
    arrow = NULL,
    fun = NULL,
    x_lim = c(-10, 10),
    y_lim = c(-10, 10),
    n = 10,
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {
  # Default aesthetics: length maps to norm, and color is left unset (NULL).
  base_mapping <- aes(length = after_stat(norm), color = NULL)

  # Merge user-provided mappings with the base defaults
  if (is.null(mapping)) {
    mapping <- base_mapping
  } else {
    mapping <- modifyList(base_mapping, mapping)
  }

  layer(
    stat = stat,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      center = center,
      normalize = normalize,
      tail_point = tail_point,
      tail_point.size = tail_point.size,
      arrow = arrow,
      fun = fun,
      x_lim = x_lim,
      y_lim = y_lim,
      n = n,
      ...
    )
  )
}
