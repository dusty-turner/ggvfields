#' Create a Streamline Plot Layer in ggplot2
#'
#' `geom_stream()` generates a ggplot2 layer that visualizes data as continuous
#' "streams" over a temporal variable `t`. Each stream is defined by **x**,
#' **y**, and **t** aesthetics, and optionally grouped by **group**. The data
#' points within each group are automatically ordered by `t` to form a
#' continuous streamline path.
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
#' @param arrow An optional [grid::arrow()] specification to place arrowheads on
#'   the streamline (e.g., to indicate direction).
#' @param ... Other arguments passed to the respective [ggplot2::geom_path()] or
#'   [ggplot2::Stat] for customization (e.g., `linetype`, `color`, `linewidth`).
#'
#' @return A ggplot2 layer that can be added to a plot to produce a streamline
#'   visualization. Internally, this layer reorders data by `t` within each
#'   group, then draws a continuous path.
#'
#' @section Aesthetics: `geom_stream()` and `stat_stream()` understand the
#'   following aesthetics (required aesthetics are in **bold**):
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
#'   `stat = StatStream` and uses [ggplot2::GeomPath] by default.
#'   - `stat_stream()` provides direct access to the reordering stat, using
#'   GeomStream for drawing. This is useful for advanced customization.
#' - **Arrows**: Use the `arrow` parameter to indicate direction on each
#'   streamline. For more details, see [grid::arrow].
#'
#'
#' @examples
#' stream_1 <- data.frame(
#'   x = c(0, 3),
#'   y = c(0, 0),
#'   t = 0:1
#' )
#'
#' stream_2 <- data.frame(
#'   x = c(1, 1),
#'   y = c(1, 5),
#'   t = 0:1
#' )
#'
#' stream_3 <- data.frame(
#'   x = c(2, 5),
#'   y = c(2, 6),
#'   t = 0:1
#' )
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
#' @seealso [ggplot2::geom_path] for the underlying path geometry, and
#' [grid::arrow] to customize arrowheads.
#'
#' @rdname geom_stream
#' @export
geom_stream <- function(mapping = NULL, data = NULL,
  stat = StatStream,
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

  default_aes = aes(x = NA, y = NA, length = 1,
                    color = "black", fill = "black",
                    linewidth = 1, linetype = 1, alpha = 1),

  compute_group = function(data, scales, ...) {
    # Ensure the data is ordered by the temporal variable 't'
    if (!"t" %in% names(data)) {
      stop("StatStream requires a 't' (time) aesthetic for ordering.")
    }

    data$norm <- sqrt((diff(range(data$x)))^2 + (diff(range(data$y)))^2)

    data

    }
)

#' @keywords internal
draw_key_length <- function(data, params, size) {

  x0 <- unit(0.05, "npc")
  y0 <- unit(0.5, "npc")

  length_value <- data$length
  x1 <- x0 + unit(length_value, "cm")
  y1 <- y0

  grid::segmentsGrob(
    x0 = x0, y0 = y0,
    x1 = x1, y1 = y1,
    gp = grid::gpar(
      col   = scales::alpha(data$colour, data$alpha),
      lwd   = data$linewidth,
      lty   = data$linetype
    )
  )

}


#' @name geom_stream
#' @export
GeomStream <- ggproto("GeomStream", GeomPath,
  # required_aes = c("x", "y"),
  default_aes = modifyList(
    GeomPath$default_aes,
    list("alpha" = 1, "linewidth" = 1.1, "length" = after_stat(NA_real_), fx = NA, fy = NA)
  ),

  setup_data = function(data, params){

    data <- data[!is.infinite(data$t), ]
    # we want to remove all points whose f(u) = c(0,0)
    if("l" %in% names(data)) {

      group_of_zero_fun <- which( vapply(
        split(data, data$group),
        function(df) abs(df[nrow(df),"l"]) < 1e-6,
        logical(1)
      ) )

      # remove zero-length groups
      if (length(group_of_zero_fun) > 0) {
        data <- subset(data, group != group_of_zero_fun)
      }

    }

    data

  },

  # Override the draw_group method
  draw_panel = function(data, panel_params, coord, tail_point = FALSE, eval_point = FALSE, arrow) {
# browser()
    if ("length" %in% names(data) && all(!is.na(data$length)) && "avg_spd" %in% names(data)) {
      # if (!any(is.na(data$length))) {

      # Suppose your data frame is called df
      df_out <- data  # Make a copy so we can modify in place

      # Loop over each unique group
      # Loop over each group
      for (g in unique(df_out$group)) {
        # Identify the two rows for group g
        idx <- which(df_out$group == g)
        # row 1 = start, row 2 = end
        i1 <- idx[1]
        i2 <- idx[2]

        # If df_out$length[i2] is not NA (or if you prefer checking avg_spd)
        # Extract the start and end coords
        x1 <- df_out$x[i1]
        y1 <- df_out$y[i1]
        x2 <- df_out$x[i2]
        y2 <- df_out$y[i2]

        # Compute dx, dy
        dx <- x2 - x1
        dy <- y2 - y1

        # The direction angle (from start to end)
        angle <- atan2(dy, dx)

        # Suppose we want to set the new length to df_out$avg_spd[i2]
        new_len <- df_out$avg_spd[i2]

        # If new_len is NA, skip or handle differently
        # We'll proceed if it's not NA
        if (!is.na(new_len)) {
          # Compute new dx, dy
          new_dx <- new_len * cos(angle)
          new_dy <- new_len * sin(angle)

          # Update the end coords
          df_out$x[i2] <- x1 + new_dx
          df_out$y[i2] <- y1 + new_dy
        }
      }

      data <- df_out

    }

    coords <- coord$transform(data, panel_params)

    # used for tail_point
    orig_coords <- coords

    # used for eval_point
    data_for_eval_coords <- data
    data_for_eval_coords$x <- data_for_eval_coords$x0
    data_for_eval_coords$y <- data_for_eval_coords$y0
    coords_for_eval_point <- coord$transform(data_for_eval_coords, panel_params)

    # keep track of new fx/fy distance from x/y
    coords$offset_x <- 0
    coords$offset_y <- 0

    if (all(!is.na(data$length))) {

      unique_groups <- unique(coords$group)

      for(g in unique_groups) {
        idx <- which(coords$group == g)

        x1 <- coords$x[idx[1]]
        y1 <- coords$y[idx[1]]
        x2 <- coords$x[idx[2]]
        y2 <- coords$y[idx[2]]

        dx <- coords$x[idx[2]] - coords$x[idx[1]]
        dy <- coords$y[idx[2]] - coords$y[idx[1]]

        dist <- sqrt(dx^2 + dy^2)

        angle <- atan2(dy, dx)

        # Desired length in cm, from the second row's 'length'
        # desired_length <- coords$avg_spd[idx[2]]
        desired_length <- coords$length[idx[2]]

        coords$x[idx[2]] <- coords$x[idx[1]]
        coords$y[idx[2]] <- coords$y[idx[1]]

        coords$offset_x[idx[1]] <- 0
        coords$offset_x[idx[2]] <- desired_length * cos(angle)

        coords$offset_y[idx[1]] <- 0
        coords$offset_y[idx[2]] <- desired_length * sin(angle)

      }

    }

    grobs <- list()
    stream_grob <- grid::nullGrob()
    tail_point_grob <- grid::nullGrob()
    eval_point_grob <- grid::nullGrob()

    # Create a pathGrob using the transformed coordinates
    # stream_grob <- grid::pathGrob(
    stream_grob <- grid::polylineGrob(
      x = grid::unit(coords$x, "npc") + grid::unit(coords$offset_x, "cm"),
      y = grid::unit(coords$y, "npc") + grid::unit(coords$offset_y, "cm"),
      id = coords$group,  # Handle grouping for multiple paths
      default.units = "native",  # Use native units for scaling
      gp = grid::gpar(
      #   col =  alpha(coords[!duplicated(coords$group), "colour"], coords[!duplicated(coords$group), "alpha"]),
 col =  coords[!duplicated(coords$group), "colour"],
 # gpar(col = alpha(munched$colour, munched$alpha)[start],
        fill = coords[!duplicated(coords$group), "colour"],
        lwd = coords[!duplicated(coords$group), "linewidth"],
        linetype = coords[!duplicated(coords$group), "linetype"]
        # alpha = coords[!duplicated(coords$group), "alpha"]
      ), arrow = arrow
    )

    if (tail_point) {

      first_coords <- orig_coords[!duplicated(orig_coords$group),]

      tail_point_grob <- grid::pointsGrob(
        x = grid::unit(first_coords$x, "npc"),
        y = grid::unit(first_coords$y, "npc"),
        pch = 16,
        # solid circle; change as needed
        size = unit(coords$size %||% 2, "mm"),
        gp = grid::gpar(col = first_coords$colour,
                        alpha = first_coords$alpha)
      )
    }

    if (eval_point) {

      first_coords_for_eval_point <- coords_for_eval_point[!duplicated(coords_for_eval_point$group),]

      eval_point_grob <- grid::pointsGrob(
        x = grid::unit(first_coords_for_eval_point$x, "npc"),
        y = grid::unit(first_coords_for_eval_point$y, "npc"),
        pch = 16,
        # solid circle; change as needed
        size = unit(coords$size %||% 2, "mm"),
        gp = grid::gpar(col = first_coords_for_eval_point$colour,
                        alpha = first_coords_for_eval_point$alpha)
      )
    }

    # Combine the line and points grobs so that both are drawn.
    grobs <- list(stream_grob, tail_point_grob, eval_point_grob)
    grobs <- Filter(Negate(is.null), grobs)  # Remove NULL entries
    return(grid::grobTree(do.call(grid::gList, grobs)))

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
scale_length_continuous <- function(max_range = 0.5, ...) {

  args <- list(...)

  if ("guide" %in% names(args)) {
    guide_val <- args$guide
    args$guide <- NULL
  } else {
    guide_val <- guide_legend(reverse = TRUE)
  }


  if (any(grepl("trans|transform", names(args), ignore.case = TRUE))) {
    cli::cli_warn(c(
      "!" = "Applying a log style transformation with {.fn scale_length_continuous} may yield negative length values for norms below 1.",
      ">" = "This may potentially reverse the direction of the vector(s)."
    ))
  }

  scale <- do.call(continuous_scale, c(
    list(
      aesthetics = "length",
      palette = scales::rescale_pal(range = c(.05, max_range)),
      guide = guide_val
    ),
    args
  ))

  # Return only the scale if max_range is at its default value
  if (max_range <= 0.5) {
    return(scale)
  }

  # For larger max_range, combine scale with theme modification
  adjusted_width <- unit(max(0.5, max_range * 1.1), "cm")

  scale <-
    list(
      scale,
      theme(legend.key.width = adjusted_width)
    )
  scale
}

