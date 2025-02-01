#' Geom Vector Field 2
#'
#' `geom_vector_field2()` creates a ggplot2 layer that visualizes a vector field by generating
#' streamlines based on a user-defined function. It leverages the underlying
#' [StatStreamField] and [GeomStream] to compute and render the streamlines, respectively.
#'
#' @param mapping Aesthetic mappings created by [ggplot2::aes()]. By default, it inherits
#'   aesthetics from the ggplot object. The default mapping sets `color` to `after_stat(null)`
#'   and `length` to `after_stat(norm)`.
#' @param data A data frame or other object, as in [ggplot2::layer()]. If `NULL`, the layer
#'   uses the plot's data.
#' @param stat The statistical transformation to use on the data for this layer. Defaults to
#'   [`StatStreamField`].
#' @param position Position adjustment, either as a string, or the result of a call to a
#'   position adjustment function. Defaults to `"identity"`.
#' @param ... Other arguments passed on to [ggplot2::layer()] and the underlying
#'   [StatStreamField] and [GeomStream]. These are often used to set aesthetics like `color = "red"`
#'   or `size = 1.5`.
#' @param na.rm Logical. If `FALSE` (default), removes missing values with a warning.
#'   If `TRUE`, silently removes missing values.
#' @param show.legend Logical. Should this layer be included in the legends? `NA`, the default,
#'   includes it if any aesthetics are mapped. `FALSE` never includes it, and `TRUE` always includes
#'   it.
#' @param inherit.aes Logical. If `FALSE`, overrides the default aesthetics, rather than
#'   combining with them. This is most useful for helper functions that define both data and
#'   aesthetics, and should not inherit behavior from the main ggplot call.
#' @param fun A function that defines the vector field. It should take a numeric vector of
#'   length 2 (representing \((x, y)\) coordinates) and return a numeric vector of length 2
#'   \((dx, dy)\) indicating the direction of the vector at that point. **(Required)**
#' @param xlim Numeric vector of length two. Specifies the limits of the x-axis domain.
#'   Defaults to `c(-1, 1)`.
#' @param ylim Numeric vector of length two. Specifies the limits of the y-axis domain.
#'   Defaults to `c(-1, 1)`.
#' @param n Integer. Grid resolution specifying the number of seed points along each axis.
#'   Higher values produce a denser vector field. Defaults to `11`.
#' @param center Logical. If `TRUE`, centers the seed points around the midpoint of the domain.
#'   Useful for symmetric flows. **Now defaults to `FALSE`.**
#' @param normalize Logical. If set to TRUE stream lengths are normalized based on grid spacing.
#'   If set to `FALSE`, a default arc length is used. Default is `TRUE`.
#' @param arrow A [grid::arrow()] specification to add arrowheads to the streamlines, indicating
#'   direction. **Now defaults to `NULL`.**
#' @param geom The geometric object used to draw the streamline.
#'
#' @return A ggplot2 **Layer** object that can be added to a plot. It computes the streamlines
#'   based on the specified vector field function and visualizes them.
#'
#' @examples
#' # Define a simple rotational vector field function
#' rotational_field <- function(u) {
#'   x <- u[1]
#'   y <- u[2]
#'   c(-y, x)  # Circular flow around the origin
#' }
#'
#' # Create a ggplot with the vector field using geom_vector_field2
#' ggplot() +
#'   geom_vector_field2(
#'     fun = rotational_field
#'   )
#'
#' @export
geom_vector_field2 <- function(mapping = NULL, data = NULL,
                               stat = StatStreamField,
                               position = "identity",
                               ...,
                               na.rm = FALSE,
                               show.legend = TRUE,
                               inherit.aes = TRUE,
                               fun,
                               xlim = c(-1, 1),
                               ylim = c(-1, 1),
                               n = 11,
                               center = FALSE,
                               normalize = TRUE,
                               arrow = NULL) {

  # Define default mapping for geom_vector_field2
  default_mapping <- ggplot2::aes(color = after_stat(NULL), length = after_stat(norm))

  # Merge user-provided mapping with default mapping.
  # User mapping takes precedence.
  if (!is.null(mapping)) {
    if (!("color" %in% names(mapping))) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    mapping <- default_mapping
  }

  # Ensure data is not empty and n is of length two (if needed)
  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  # Normalize flag: if TRUE, set to "vector" (as expected by the underlying stat)
  if (normalize) normalize <- "vector"

  ggplot2::layer(
    stat = stat,
    geom = GeomStream,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      method = "euler",
      na.rm = na.rm,
      dt = 1,
      max_it = 2,
      center = center,
      normalize = normalize,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname geom_vector_field2
#' @export
stat_vector_field2 <- function(mapping = NULL, data = NULL,
                               geom = GeomStream,
                               position = "identity",
                               ...,
                               na.rm = FALSE,
                               show.legend = TRUE,
                               inherit.aes = TRUE,
                               fun,
                               xlim = c(-1, 1),
                               ylim = c(-1, 1),
                               n = 11,
                               center = FALSE,
                               normalize = TRUE,
                               arrow = NULL) {

  # Define default mapping for stat_stream_field2
  default_mapping <- ggplot2::aes(color = after_stat(NULL), length = after_stat(norm))

  # Merge user-provided mapping with default mapping.
  # User mapping takes precedence.
  if (!is.null(mapping)) {
    if (!("color" %in% names(mapping))) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    mapping <- default_mapping
  }

  # Ensure data is not empty and ensure n is of the proper length if required
  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  # If normalize is TRUE, convert it to "vector" as expected by the underlying stat
  if (normalize) normalize <- "vector"

  ggplot2::layer(
    stat = StatStreamField,
    geom = geom,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      method = "euler",
      na.rm = na.rm,
      dt = 1,
      max_it = 2,
      center = center,
      normalize = normalize,
      arrow = arrow,
      ...
    )
  )
}
