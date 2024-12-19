#' Create a Gradient Field Plot Layer with Length Mapped to Norm
#'
#' `geom_gradient_field2` and `stat_gradient_field2` are extensions of `geom_gradient_field` and `stat_gradient_field`,
#' providing identical functionality but with different default aesthetics. These functions are particularly useful
#' for gradient field visualizations where vector length is mapped to the vector's magnitude.
#'
#' **Default Behavior:**
#' - **Length** is automatically mapped to the vector norm (`length = after_stat(norm)`), emphasizing the vector
#'   magnitude through the length of each vector in the plot.
#' - **Color** is set to `"black"` by default, ensuring a uniform appearance across vectors. Users can override
#'   this if they wish to map another variable to `color`.
#'
#' **Additional Parameters:**
#' - **center** is set to `FALSE` by default, so vectors start from the specified (`x`, `y`) coordinates.
#' - **tail_point** is set to `TRUE` by default, adding a small point at the tail of each vector to help indicate the starting point.
#' - **arrow** is set to `NULL`, but users can specify an arrow using `grid::arrow()` to add custom arrowheads.
#'
#' @inheritParams geom_gradient_field
#' @param mapping Aesthetic mappings created by `aes()` or `aes_()`.
#'   These functions ensure that `length = after_stat(norm)` is mapped by default, and `color` is set to `"black"` unless otherwise specified.
#' @param tail_point Logical; if `TRUE`, adds a small point at the tail of each vector to help indicate the starting point.
#' @param ... Other arguments passed on to `geom_gradient_field()` or `stat_gradient_field()`.
#' @return A `ggplot2` layer that can be added to a ggplot object to create a gradient field plot.
#'
#' @examples
#' library(ggvfields)
#'
#' paraboloid_field <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   x^2 + y^2
#' }
#'
#' saddle_field <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   x^3 - 3 * x * y^2
#' }
#'
#' ggplot() +
#'   geom_gradient_field2(fun = paraboloid_field)
#'
#' ggplot() +
#'   geom_gradient_field2(fun = saddle_field)
#'
#' @seealso
#' Use [geom_gradient_field()] if you prefer to map vector magnitude using a different aesthetic such as `color`.
#'
#' @export
geom_gradient_field2 <- function(mapping = NULL, data = NULL,
                                 stat = "identity", geom = "vector",
                                 ...,
                                 position = "identity",
                                 na.rm = FALSE,
                                 show.legend = NA,
                                 inherit.aes = TRUE,
                                 fun,
                                 xlim = NULL,
                                 ylim = NULL,
                                 n = 16,
                                 center = FALSE,
                                 normalize = TRUE,
                                 tail_point = TRUE,
                                 arrow = NULL
                                 ) {

  # Define the base aesthetic mappings
  base_mapping <- aes(length = after_stat(norm), color = NULL)

  # Modify the user-specified mapping if provided
  if (is.null(mapping)) {
    mapping <- base_mapping
  } else {
    mapping <- modifyList(base_mapping, mapping)
  }

  geom_gradient_field(
    mapping = mapping,
    data = data,
    stat = stat,
    geom = geom,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    fun = fun,
    xlim = xlim,
    ylim = ylim,
    n = n,
    center = center,
    normalize = normalize,
    arrow = arrow,
    tail_point = tail_point,
    ...
  )
}

#' @rdname geom_gradient_field2
#' @export
stat_gradient_field2 <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    geom = "vector",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    fun,
    xlim = NULL,
    ylim = NULL,
    n = 16,
    center = FALSE,
    normalize = TRUE,
    tail_point = TRUE,
    arrow = NULL,
    ...
) {

  # Define the base aesthetic mappings
  base_mapping <- aes(length = after_stat(norm), color = NULL)

  # Modify the user-specified mapping if provided
  if (is.null(mapping)) {
    mapping <- base_mapping
  } else {
    mapping <- modifyList(base_mapping, mapping)
  }

  stat_gradient_field(
    mapping = mapping,
    data = data,
    stat = stat,
    geom = geom,
    position = position,
    na.rm = na.rm,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    fun = fun,
    xlim = xlim,
    ylim = ylim,
    n = n,
    center = center,
    normalize = normalize,
    arrow = arrow,
    tail_point = tail_point,
    ...
  )
}
