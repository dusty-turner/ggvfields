#' Create a Vector Field Plot Layer with Length Mapped to Norm
#'
#' `geom_vector_field2` and `stat_vector_field2` are extensions of `geom_vector_field` and `stat_vector_field`,
#' providing identical functionality but with different default aesthetics. These functions are particularly useful
#' for vector field visualizations where vector length is mapped to the vector's magnitude.
#'
#' **Default Behavior:**
#' - **Length** is automatically mapped to the vector norm (`length = after_stat(norm)`), meaning that vector
#'   magnitude is emphasized through the length of each vector in the plot.
#' - **Color** is set to `"black"` by default, ensuring a uniform appearance across vectors. Users can override
#'   this if they wish to map another variable to `color`.
#'
#' **Additional Parameters:**
#' - **center** is set to `FALSE` by default, so vectors start from the specified (`x`, `y`) coordinates.
#' - **tail_point** is set to `TRUE`, adding a small point at the tail of each vector to help indicate the starting point.
#' - **arrow** is set to `NULL`, but users can specify an arrow using `grid::arrow()` to add custom arrowheads.
#'
#' @inheritParams geom_vector_field
#' @param mapping Aesthetic mappings created by `aes()` or `aes_()`.
#'   These functions ensure that `length = after_stat(norm)` is mapped by default, and `color` is set to `"black"` unless otherwise specified.
#' @param ... Other arguments passed on to `geom_vector_field()` or `stat_vector_field()`.
#' @return A `ggplot2` layer that can be added to a ggplot object to create a vector field plot.
#'
#' @examples
#' # Example user-defined vector field function
#' f <- function(v) {
#'   x <- v[1]; y <- v[2]
#'   c(x + y, y - x)
#' }
#'
#' # Create a vector field plot with geom_vector_field2
#' ggplot() +
#'   geom_vector_field2(fun = f, xlim = c(-5, 5), ylim = c(-5, 5))
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
    tail_point = TRUE,
    arrow = NULL,
    ...
) {

  base_mapping <- aes(length = after_stat(norm), color = NULL)

  if (is.null(mapping)) {
    mapping <- base_mapping
  } else {
    mapping <- modifyList(base_mapping, mapping)
  }

  geom_vector_field(
    mapping = mapping,
    data = data,
    stat = stat,
    geom = geom,
    position = position,
    center = center,
    tail_point = tail_point,
    arrow = arrow,
    ...
  )
}


#' @rdname geom_vector_field2
#' @export
stat_vector_field2 <- function(
    mapping = NULL,
    data = NULL,
    geom = GeomVector,
    position = "identity",
    ...
) {

  base_mapping <- aes(length = after_stat(norm), color = NULL)

  if (is.null(mapping)) {
    mapping <- base_mapping
  } else {
    mapping <- modifyList(base_mapping, mapping)
  }

  stat_vector_field(
    mapping = mapping,
    data = data,
    geom = geom,
    position = position,
    center = center,
    tail_point = tail_point,
    arrow = arrow,
    ...
  )
}
