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


                        data$length <- sqrt((diff(range(data$x)))^2 + (diff(range(data$y)))^2)

                        # if(center)
                          # data <- center_vector(data)
                        data
                      }
)

#' @keywords internal
draw_key_length <- function(data, params, size) {
  # data$length <- scales::rescale(x = data$length, to = c(min(data$length),7))
  # print("draw_key_length")
  # print(data)

  # x0 <- unit(0.1, "npc")
  x0 <- unit(0.05, "npc")
  y0 <- unit(0.5, "npc")

  length_value <- data$length
  # x1 <- x0 + unit(length_value, "cm")
  # y1 <- y0
  x1 <- rev(x0 + unit(length_value, "cm"))
  y1 <- rev(y0)

  grid::segmentsGrob(
    x0 = x0, y0 = y0,
    x1 = x1, y1 = y1,
    gp = grid::gpar(
      # col   = scales::alpha(data$colour, data$alpha),
      # fill  = scales::alpha(data$fill,   data$alpha),
      # lwd   = data$linewidth,
      # lty   = data$linetype
    )
  )

}



#' @name geom_stream
#' @export
# GeomStream <- ggproto("GeomStream", GeomPath)
GeomStream <- ggproto("GeomStream", GeomPath,
                      # required_aes = c("x", "y"),  # Specify required aesthetics
                      default_aes = modifyList(GeomPath$default_aes, list(alpha = 1, length = after_stat(1))),

                      setup_data = function(data, params){


                        data$original_length <- data$length

                        ##### here goes nothing below

                        # Initialize vectors to store transformed coordinates
                        new_x <- data$x
                        new_y <- data$y

                        # Get unique groups
                        unique_groups <- unique(data$group)

                        # Loop through each group to calculate new coordinates for t = 2
                        for (g in unique_groups) {
                          # Subset data for the current group
                          group_data <- data[data$group == g, ]

                          # Identify first and second points
                          first_point <- group_data[group_data$t == 1, ]
                          second_point <- group_data[group_data$t == 2, ]

                          # Calculate differences
                          dx <- second_point$x - first_point$x
                          dy <- second_point$y - first_point$y

                          # Original distance
                          orig_dist <- second_point$original_length


                          # Desired length
                          desired_length <- second_point$length

                          # Handle zero original distance to avoid division by zero
                          unit_dx <- dx / orig_dist
                          unit_dy <- dy / orig_dist

                          # Calculate transformed differences
                          trans_dx <- unit_dx * desired_length * .01
                          trans_dy <- unit_dy * desired_length * .01

                          # Compute new coordinates
                          new_x_val <- first_point$x + trans_dx
                          new_y_val <- first_point$y + trans_dy

                          # Update the new_x and new_y vectors
                          idx <- which(data$group == g & data$t == 2)
                          new_x[idx] <- new_x_val
                          new_y[idx] <- new_y_val
                        }

                        # Create the transformed data frame
                        data_transformed <- data
                        data_transformed$x <- new_x
                        data_transformed$y <- new_y

                        # Display Transformed data

                        data <- data_transformed
                        data


                      },

                      # Override the draw_group method
                      draw_panel = function(data, panel_params, coord, arrow) {

                       # Transform the data according to the coordinate system
                        coords <- coord$transform(data, panel_params)

                        ##### here goes nothing above

                        coords$dx <- 0
                        coords$dy <- 0

                        # 2) For each group (which has exactly 2 rows),
                        #    set dx, dy in the second row based on
                        #    (second x - first x), (second y - first y)
                        unique_groups <- unique(coords$group)
                        for(g in unique_groups) {
                          idx <- which(coords$group == g)
                          if (length(idx) == 2) {
                            coords$dx[idx[2]] <- coords$x[idx[2]] - coords$x[idx[1]]
                            coords$dy[idx[2]] <- coords$y[idx[2]] - coords$y[idx[1]]
                          }
                        }
                        # print(coords)

                        norms <- sqrt(coords$dx^2 + coords$dy^2)
                        # norms[norms == 0] <- 1  # Avoid division by zero
                        coords$dx <- coords$dx / norms
                        coords$dy <- coords$dy / norms

                        coords$dx <- ifelse(is.nan(coords$dx), 0, coords$dx)
                        coords$dy <- ifelse(is.nan(coords$dy), 0, coords$dy)


                        print("data before polylinegrob")
                        print(coords)


                        # coords$new_x <- ifelse(
                        #   coords$t == 2,
                        #   grid::unit(coords$x, "npc") + grid::unit(coords$length * coords$dx, "cm"),
                        #   grid::unit(coords$x, "npc") + grid::unit(coords$length * coords$dx, "cm")
                        # )
                        # coords$new_x <- ifelse(
                        #   coords$t == 2,
                        #   grid::unit(lead(coords$x), "npc") + grid::unit(coords$length * coords$dx, "cm"),
                        #   grid::unit(coords$x, "npc") + grid::unit(coords$length * coords$dx, "cm")
                        # )


                        # 3) Create new_x, new_y with the same offset logic
                        #    as segmentsGrob: x1 = x0 + length*dx in cm, etc.

                        coords$lag_x <- ifelse(coords$t == 2, lag(coords$x), coords$x)
                        coords$lag_y <- ifelse(coords$t == 2, lag(coords$y), coords$y)




                        coords$new_x <- grid::unit(coords$lag_x, "npc") + grid::unit(coords$length * coords$dx, "cm")
                        # coords$new_x <- grid::unit(coords$x, "npc") + grid::unit(coords$length * coords$dx, "cm")
                        coords$new_y <- grid::unit(coords$lag_y, "npc") + grid::unit(coords$length * coords$dy, "cm")

                        # Result

                        print(coords)

                        # Create a pathGrob using the transformed coordinates
                        grid::polylineGrob(
                          x = coords$new_x,
                          y = coords$new_y,
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

                        # grid::segmentsGrob(
                        #   x0 = unit(coords$x, "npc"), y0 = unit(coords$y, "npc"),
                        #   x1 = unit(coords$x, "npc") + unit(coords$length * coords$dx, "cm"),
                        #   y1 = unit(coords$y, "npc") + unit(coords$length * coords$dy, "cm"),
                        #   gp = grid::gpar(
                        #     col   = coords$colour,
                        #     fill  = coords$fill,
                        #     alpha = coords$alpha,
                        #     lty   = coords$linetype,  # user’s mapped linetype
                        #     lwd   = coords$linewidth  # user’s mapped thickness
                        #   ),
                        #   arrow = arrow
                        # )


                      },
                      draw_key = draw_key_length
)

#' Create a Continuous Scale for Vector Length
#'
#' [scale_length_continuous()] provides a continuous scale for controlling the
#' length aesthetic in a ggplot. This is particularly useful when working with
#' vector plots where vector lengths are mapped to a continuous scale.
#'
#' @param max_range The maximum value to which the input is rescaled. Numeric
#'   scalar specifying the upper bound of the output range. Should be between 0
#'   and 1.
#' @param ... Other arguments passed to [continuous_scale()].
#' @export
#' @examples
#'
#' #ggplot() +
#' #  geom_vector_field2(fun = efield_maker(), xlim = c(-2, 2), ylim = c(-2, 2)) +
#' #  scale_length_continuous(trans = "log10")
# scale_length_continuous <- function(max_range = 0.5, ...) {
#
# args <- list(...)
#
# if (any(grepl("trans|transform", names(args), ignore.case = TRUE))) {
#   cli::cli_warn(c(
#     "!" = "Applying a log style transformation with {.fn scale_length_continuous} may yield negative length values for norms below 1.",
#     ">" = "This may potentially reverse the direction of the vector(s)."
#   ))
# }
#
# continuous_scale(
#   aesthetics = "length",
#   palette = scales::rescale_pal(range = c(.01, max_range)),
#   ...
# )
#
# }
scale_length_continuous <- function(max_range = 0.5, ...) {

  args <- list(...)

  if (any(grepl("trans|transform", names(args), ignore.case = TRUE))) {
    cli::cli_warn(c(
      "!" = "Applying a log style transformation with {.fn scale_length_continuous} may yield negative length values for norms below 1.",
      ">" = "This may potentially reverse the direction of the vector(s)."
    ))
  }

  scale <- continuous_scale(
    aesthetics = "length",
    palette = scales::rescale_pal(range = c(.05, max_range)),
    ...
  )

  # Return only the scale if max_range is at its default value
  # if (max_range <= 0.5) {
  #   return(scale)
  # }

  # For larger max_range, combine scale with theme modification
  adjusted_width <- unit(max(0.5, max_range * 1.1), "cm")

  # adjusted_width <- unit(10, "cm")


    # scale
  list(
    scale,
    theme(legend.key.width = adjusted_width)
  )
}
