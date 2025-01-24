#' Create a Stream Plot Layer
#'
#' `geom_stream()` adds a layer to a ggplot that connects data points in the order specified by a temporal variable `t`.
#' It is ideal for visualizing trajectories, movement patterns, or any sequential data where the order of points is crucial.
#' Additionally, an optional `id` aesthetic allows for the plotting of multiple distinct streams within the same plot.
#'
#' This geom leverages [`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html) internally to draw the paths,
#' ensuring compatibility with ggplot2's extensive theming and customization options. By specifying the `t` aesthetic,
#' users can control the sequencing of points to accurately represent the flow of the stream.
#'
#' @inheritParams ggplot2::geom_path
#' @param arrow An optional arrow specification created with [`grid::arrow()`](https://www.rdocumentation.org/packages/grid/versions/3.6.2/topics/arrow).
#'   This adds arrowheads to the end of the paths, enhancing the visual indication of direction.
#'   Setting `arrow = NULL` removes arrowheads. Default is `grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")`.
#' @param ... Additional aesthetic mappings or parameters. Use `id` to specify grouping for multiple streams.
#'
#' @return A `ggplot2` layer that can be added to a ggplot object to produce a stream plot.
#'
#' @section Aesthetics:
#' \describe{
#'   \item{x}{The x-coordinate of the points.}
#'   \item{y}{The y-coordinate of the points.}
#'   \item{t}{A numeric or date/time variable that determines the order of points.}
#'   \item{id}{(Optional) A categorical variable that groups points into separate streams. Each unique `id` represents a distinct stream.}
#'   \item{colour, color, size, linetype, alpha}{Additional aesthetics inherited from [`geom_path()`](https://ggplot2.tidyverse.org/reference/geom_path.html).}
#' }
#'
#' @section Key Notes:
#' - **Ordering by Time (`t`):** The `t` aesthetic is essential for determining the sequence in which points are connected. Ensure that `t` accurately represents the temporal or sequential order of your data.
#' - **Grouping with `id`:** When plotting multiple streams, the `id` aesthetic groups points into separate paths. Each unique `id` will result in an independent stream within the same plot.
#' - **Arrowheads for Directionality:** Utilizing the `arrow` parameter can help indicate the direction of the stream, which is particularly useful for representing flows or movements.
#' - **Compatibility:** Since `geom_stream()` is built upon `geom_path()`, all customization options available to `geom_path()` are also applicable to `geom_stream()`.
#'
#' @examples
#' \dontrun{}
#'
#' @rdname geom_stream
#' @export
geom_stream <- function(mapping = NULL, data = NULL,
                        stat = "stream", position = "identity",
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
    geom = GeomPath,
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
                        # geom = "path",
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

#' @rdname StatStream
#' @keywords internal
StatStream <- ggproto("StatStream", Stat,
                      required_aes = c("x", "y", "t"),
                      # No need to specify group here; grouping is handled via 'group' aesthetic
                      compute_group = function(data, scales, ...) {
                        # Ensure the data is ordered by the temporal variable 't'
                        if (!"t" %in% names(data)) {
                          stop("StatStream requires a 't' (time) aesthetic for ordering.")
                        }

                        data <- data[order(data$t), ]

                        # Return the ordered data without any additional processing
                        data
                      }
)

#' @rdname GeomStream
#' @export
GeomStream <- ggproto("GeomStream", GeomPath)
