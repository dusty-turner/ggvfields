#' Create a Vector Field Plot Layer with Length Mapped to Norm
#'
#' `geom_vector_field2` behaves the same as `geom_vector_field` but has different default aesthetics.
#' - **Length** is automatically mapped to `norm`.
#' - **Color** is set to `"black"` by default.
#'
#' @inheritParams geom_vector_field
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
#' @export
geom_vector_field2 <- function(
    mapping = NULL,
    data = NULL,
    stat = StatVector,
    geom = GeomVector,
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    fun,
    xlim = c(-10, 10),
    ylim = c(-10, 10),
    n = 16,
    center = TRUE,
    normalize = TRUE,
    arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
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
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    fun,
    xlim = c(-10, 10),
    ylim = c(-10, 10),
    n = 16,
    center = TRUE,
    normalize = TRUE,
    arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
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
    ...
  )
}
