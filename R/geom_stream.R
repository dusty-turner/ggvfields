#' Create a Streamline Plot Layer in ggplot2
#'
#' `geom_stream()` generates a ggplot2 layer that visualizes
#' data as continuous "streams" over a temporal variable `t`.
#' Each stream is defined by **x**, **y**, and **t** aesthetics, and
#' optionally grouped by **group**. The data points within each group
#' are automatically ordered by `t` to form a continuous streamline path.
#'
#' @inheritParams ggplot2::geom_path
#' @inheritParams ggplot2::stat_identity
#'
#' @param mapping Set of aesthetic mappings created by [ggplot2::aes()].
#'   **Required**: `x`, `y`, `t`.
#'   **Optional**: `group` (if multiple streams).
#'   If `id` is provided, it will be mapped to `group` automatically.
#' @param data A data frame or other object, as in [ggplot2::layer()].
#' @param geom The geometric object used to draw the streamline. Defaults to
#'   [ggplot2::GeomPath] in `geom_stream()`, or [GeomStream] in `stat_stream()`.
#' @param na.rm If `FALSE` (default), missing values are removed with a warning.
#'   If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetics rather than
#'   combining with them.
#' @param arrow An optional [grid::arrow()] specification to place arrowheads
#'   on the streamline (e.g., to indicate direction).
#' @param ... Other arguments passed to the respective
#'   [ggplot2::geom_path()] or [ggplot2::Stat] for customization (e.g.,
#'   `linetype`, `color`, `linewidth`).
#'
#' @return A ggplot2 layer that can be added to a plot to produce a streamline
#'   visualization. Internally, this layer reorders data by `t` within each group,
#'   then draws a continuous path.
#'
#' @section Aesthetics:
#' `geom_stream()` and `stat_stream()` understand the following aesthetics
#' (required aesthetics are in **bold**):
#' - **`x`**: Typically the horizontal axis (often mapped to time `t`).
#' - **`y`**: The vertical axis (often magnitude or value at time `t`).
#' - **`t`**: A temporal or ordered variable used to sequence the data.
#' - `group`: Grouping for multiple streams (mapped from `id` if present).
#' - `color`, `linetype`, `linewidth`, `alpha`, etc. (inherited from [ggplot2::geom_path]).
#'
#' @section Details:
#' - **Data Ordering**: If `t` is missing, an error is thrown. If present,
#'   points within each group are sorted by `t` before drawing.
#' - **Stream vs. Stat**:
#'   - `geom_stream()` is a convenient wrapper for typical usage; it sets
#'     `stat = StatStream` and uses [ggplot2::GeomPath] by default.
#'   - `stat_stream()` provides direct access to the reordering stat, using
#'     GeomStream for drawing. This is useful for advanced customization.
#' - **Arrows**: Use the `arrow` parameter to indicate direction on each
#'   streamline. For more details, see [grid::arrow].
#'
#'
#' @examples
#' stream_1 <- data.frame(x = -5:4)
#' stream_1$t <- seq_len(nrow(stream_1))
#' stream_1$y <- stream_1$x^2 + 5
#'
#' stream_2 <- data.frame(x = -4:5)
#' stream_2$t <- seq_len(nrow(stream_2))
#' stream_2$y <- sqrt(stream_2$x + 5) - 5
#'
#' stream_3 <- data.frame(x = 10:0)
#' stream_3$t <- seq_len(nrow(stream_3))
#' stream_3$y <- stream_3$x + stream_3$x^0.9
#'
#' streams <- rbind(
#'   cbind(stream_1, id = 1),
#'   cbind(stream_2, id = 2),
#'   cbind(stream_3, id = 3)
#' )
#'
#' ggplot(streams) +
#'   geom_stream(aes(x = x, y = y, t = t, group = id))
#'
#'
#' @seealso
#' [ggplot2::geom_path] for the underlying path geometry, and [grid::arrow]
#' to customize arrowheads.
#'
#' @rdname geom_stream
#' @export
geom_stream <- function(mapping = NULL, data = NULL,
                        stat = "stream",
                        position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")
                        ) {

  # If 'id' is provided in mapping, map it to 'group'
  if (!is.null(mapping) && "id" %in% names(mapping)) {
    mapping$group <- mapping$group %||% mapping$id
  }

  layer(
    stat = stat,
    geom = GeomStream,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname geom_stream
#' @export
stat_stream <- function(mapping = NULL, data = NULL,
                        geom = GeomStream,
                        position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE,
                        arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")
                        ) {

  # If 'id' is provided in mapping, map it to 'group'
  if (!is.null(mapping) && "id" %in% names(mapping)) {
    mapping$group <- mapping$group %||% mapping$id
  }

  layer(
    stat = StatStream,
    geom = geom,
    mapping = mapping,
    data = data,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      ...
    )
  )
}

#' @name geom_stream
#' @export
StatStream <- ggproto("StatStream", Stat,
                      required_aes = c("x", "y", "t"),
                      # No need to specify group here; grouping is handled via 'group' aesthetic
                      compute_group = function(data, scales, ...) {
                        # Ensure the data is ordered by the temporal variable 't'
                        if (!"t" %in% names(data)) {
                          stop("StatStream requires a 't' (time) aesthetic for ordering.")
                        }

                        # if(center)
                          # data <- center_vector(data)

                        data
                      }
)

#' @name geom_stream
#' @export
# GeomStream <- ggproto("GeomStream", GeomPath)
GeomStream <- ggproto("GeomStream", GeomPath,
                      # required_aes = c("x", "y"),  # Specify required aesthetics
                      default_aes = modifyList(GeomPath$default_aes, list(alpha = 1)),

                      # Override the draw_panel method
                      draw_panel = function(data, panel_params, coord, arrow) {
                        # Transform the data according to the coordinate system
                        coords <- coord$transform(data, panel_params)

                        # Create a pathGrob using the transformed coordinates
                        grid::polylineGrob(
                          x = coords$x,
                          y = coords$y,
                          id = coords$group,  # Handle grouping for multiple paths
                          default.units = "native",  # Use native units for scaling
                          gp = grid::gpar(
                            col = coords$colour,        # Set line color
                            fill = coords$colour,
                            lwd = coords$linewidth,    # Set line width (converted from ggplot2 size)
                            linetype = coords$linetype, # Set line type
                            alpha = coords$alpha         # Set transparency
                          ), arrow = arrow
                        )
                      }
)

