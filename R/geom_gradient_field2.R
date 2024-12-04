#' Create a Gradient Field Plot Layer with Length Mapped to Norm
#'
#' `geom_gradient_field2` and `stat_gradient_field2` are extensions of `geom_gradient_field` and `stat_gradient_field`,
#' providing identical functionality but with different default aesthetics. These functions are particularly useful
#' for gradient field visualizations where vector length is mapped to the vector's magnitude.
#'
#' **Default Behavior:**
#' - **Length** is automatically mapped to the vector norm (`length = after_stat(norm)`), emphasizing the vector's magnitude through its length in the plot.
#' - **Color** is set to `"black"` by default, ensuring a uniform appearance across vectors. Users can override this by specifying a different aesthetic mapping.
#' - **Center** is set to `FALSE` by default, so vectors start from the specified (`x`, `y`) coordinates.
#' - **Arrowheads** are set to `NULL` by default, allowing users to add custom arrowheads if desired.
#'
#' **Additional Parameters:**
#' - **`tail_point`**: Logical; if `TRUE`, adds a small point at the tail of each vector to indicate the starting point.
#' - **`arrow`**: Arrow specification, created by `grid::arrow()`, to add arrowheads to vectors. Default is `NULL`.
#'
#' @inheritParams geom_gradient_field
#' @param mapping Aesthetic mappings created by `aes()` or `aes_()`.
#'   These functions ensure that `length = after_stat(norm)` is mapped by default, and `color` is set to `"black"` unless otherwise specified.
#' @param tail_point Logical; if `TRUE`, adds a small point at the tail of each vector to help indicate the starting point.
#' @param ... Other arguments passed on to `geom_gradient_field()`, such as additional aesthetic mappings.
#'
#' @return A `ggplot2` layer that can be added to a ggplot object to create a gradient field plot.
#'
#' @examples
#' library(ggplot2)
#'
#' # Define scalar functions
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
#' # Create a gradient field plot using geom_gradient_field2
#' ggplot() +
#'   geom_gradient_field2(
#'     fun = paraboloid_field,
#'     xlim = c(-5, 5),
#'     ylim = c(-5, 5),
#'     n = 20,
#'     aes(color = after_stat(norm)),  # Override default color if desired
#'     tail_point = TRUE,
#'     arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
#'   ) +
#'   coord_fixed() +
#'   theme_minimal() +
#'   labs(title = "Gradient Field of Scalar Function x² + y²")
#'
#' # Create a gradient field plot using geom_gradient_field2 with saddle_field
#' ggplot() +
#'   geom_gradient_field2(
#'     fun = saddle_field,
#'     xlim = c(-2, 2),
#'     ylim = c(-2, 2),
#'     n = 20,
#'     aes(color = after_stat(norm)),  # Override default color if desired
#'     tail_point = TRUE,
#'     arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
#'   ) +
#'   coord_fixed() +
#'   theme_minimal() +
#'   labs(title = "Gradient Field of Scalar Function x³ - 3xy²")
#'
#' @seealso
#' Use [geom_gradient_field()] if you prefer to map vector magnitude using a different aesthetic such as `color`.
#'
#' @export
geom_gradient_field2 <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    geom = "vector",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    fun,
    xlim = c(-10, 10),
    ylim = c(-10, 10),
    n = 16,
    center = FALSE,
    normalize = TRUE,
    tail_point = TRUE,
    arrow = NULL,
    ...
) {

  # Define default aesthetic mappings
  base_mapping <- aes(length = after_stat(norm), color = "black")

  # If user provides a mapping, combine it with the base mapping
  if (is.null(mapping)) {
    mapping <- base_mapping
  } else {
    # Override base_mapping with user-specified aesthetics, except for length and color
    # Only set length and color if they are not already mapped
    if (!"length" %in% names(mapping)) {
      mapping$length <- after_stat(norm)
    }
    if (!"color" %in% names(mapping)) {
      mapping$color <- "black"
    }
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
    geom = "gradient_field2",
    position = "identity",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE,
    fun,
    xlim = c(-10, 10),
    ylim = c(-10, 10),
    n = 16,
    center = FALSE,
    normalize = TRUE,
    tail_point = TRUE,
    arrow = NULL,
    ...
) {

  # Define default aesthetic mappings
  base_mapping <- aes(length = after_stat(norm), color = "black")

  # If user provides a mapping, combine it with the base mapping
  if (is.null(mapping)) {
    mapping <- base_mapping
  } else {
    # Override base_mapping with user-specified aesthetics, except for length and color
    if (!"length" %in% names(mapping)) {
      mapping$length <- after_stat(norm)
    }
    if (!"color" %in% names(mapping)) {
      mapping$color <- "black"
    }
  }

  geom_gradient_field2(
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
    tail_point = tail_point,
    arrow = arrow,
    ...
  )
}
