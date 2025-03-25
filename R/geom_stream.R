#' Create a Streamline Plot Layer in ggplot2
#'
#' `geom_stream()` generates a ggplot2 layer that visualizes data as continuous
#' streams over a temporal variable **`t`**. Each stream is defined by the
#' required aesthetics **`x`**, **`y`**, and **`t`**, and optionally grouped by
#' **`group`** (or mapped from `id`). Within each group, data points are
#' automatically ordered by `t` to form a continuous path.
#'
#' There are two variants:
#'
#' - `geom_stream()`: A convenient wrapper that sets `stat = StatStream` and uses
#' [ggplot2::GeomPath] by default.
#' - `stat_stream()`: Provides direct access to the reordering stat (i.e. `StatStream`)
#' for advanced customization, using [GeomStream] for drawing.
#'
#' @param mapping A set of aesthetic mappings created by \code{ggplot2::aes()}.
#'   **Required:** Must include **`x`**, **`y`**, and **`t`**; additionally,
#'   **`group`** is used to differentiate streams (if not provided, `id` will be mapped to
#'   `group` automatically).
#' @param data A data frame or other object, as in \code{grid::layer()}.
#' @param geom The geometric object used to render the streamline (only used in
#'   `stat_stream()`; defaults to [GeomStream]).
#' @param stat The statistical transformation to use on the data for this layer;
#'   defaults to [StatStream].
#' @param position Position adjustment, either as a string or the result of a
#'   position adjustment function.
#' @param na.rm Logical. If `FALSE` (the default), missing values are removed
#'   with a warning. If `TRUE`, missing values are silently removed.
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes Logical. If `FALSE`, overrides the default aesthetics
#'   rather than combining with them.
#' @param arrow An optional [grid::arrow()] specification to place arrowheads on
#'   the streamline.
#' @param arrow.fill An optional parameter specifying the color of the arrow
#'   head. Defaults to `NULL` and inherets fill/alpha of `aes()`.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
#' @param ... Other arguments passed to the underlying layers for further
#'   customization.
#'
#' @section Aesthetics: `geom_stream()` and `stat_stream()` understand the
#'   following aesthetics (required aesthetics are in **bold**):
#'
#'   - **`x`**: Horizontal position.
#'   - **`y`**: Vertical position.
#'   - `t`: Temporal or ordered variable used to sequence data points.
#'        If not provided, a default sequence ranging from 0 to 1 is automatically generated for each group (or for all data if no grouping is provided).
#'   - `group`: Grouping variable for multiple streams (automatically mapped from `id` if absent).
#'   - `color`/`colour`: Color of the stream. If not provided, it is automatically mapped to the computed `t` value.
#'   - `linetype`: Type of line used to draw the stream.
#'   - `linewidth`: Thickness of the stream line.
#'   - `alpha`: Transparency of the stream.
#'
#' @section Details:
#' - **Data Ordering**: If `t` is not provided, a default sequence (from 0 to 1) is generated per group (or for the entire dataset if no grouping is specified).
#'   Points within each group are then sorted by `t` prior to drawing the
#'   stream.
#'
#' @return A ggplot2 layer that can be added to a plot to produce a streamline
#'   visualization.
#'
#' @section Computed Variables:
#'
#'   These are calculated by the 'stat' part of layers and can be accessed with
#'   delayed evaluation.
#'
#' \describe{
#'   \item{norm}{This variable is calculated as the Euclidean distance derived
#'      from the ranges of the \code{x} and \code{y} values. It serves as a
#'      normalization factor for vector lengths when the \code{normalize}
#'      parameter is active.}
#'
#'   \item{avg_spd}{Represents the average speed, which is defined as the length
#'      of the stream divided by the time it took to traverse that distance.}
#' }
#'
#' @examples
#'
#' n <- 25
#' s <- seq(0, 1, length.out = n+1)[-(n+1)]
#' df <- data.frame( "t" = s, "x" = cos(2*pi*s), "y" = sin(2*pi*s) )
#'
#' ggplot(df) +
#'   geom_stream(aes(x, y, t = t)) +
#'   coord_equal()
#'
#' ggplot(df) +
#'   geom_stream(aes(x, y, t = t, alpha = t), size = 5) +
#'   coord_equal()
#'
#' ggplot(df) +
#'   geom_path(aes(x, y, alpha = t), size = 5) +
#'   coord_equal()
#'
#'
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
#' ggplot(stream_1) +
#'   geom_stream(aes(x = x, y = y, t = t))
#'
#' # set group aes if multiple vectors
#' ggplot(streams) +
#'   geom_stream(aes(x = x, y = y, t = t, group = id))
#'
#' @aliases geom_stream stat_stream StatStream GeomStream
#' @name geom_stream
#' @export
NULL

#' @rdname geom_stream
#' @export
geom_stream <- function(mapping = NULL, data = NULL,
  stat = StatStream,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  arrow.fill = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")
) {

  dots <- list(...)

  default_mapping <- ggplot2::aes(t = after_stat(t), color = after_stat(t))

  # If mapping is provided, check if color/colour is in mapping OR fixed color in dots.
  if (!is.null(mapping)) {
    if (!( "color" %in% names(mapping) ||
           "colour" %in% names(mapping) ||
           "color" %in% names(dots) ||
           "colour" %in% names(dots) )) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    # No mapping provided: if a fixed color was supplied, omit the default color mapping.
    if ("color" %in% names(dots) || "colour" %in% names(dots)) {
      mapping <- ggplot2::aes(t = after_stat(t))
    } else {
      mapping <- default_mapping
    }
  }

  # Map 'id' to 'group' if provided.
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
      arrow.fill = arrow.fill,
      type = "stream",
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
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
  arrow.fill = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")
) {

  dots <- list(...)

  default_mapping <- ggplot2::aes(t = after_stat(t), color = after_stat(t))

  # If mapping is provided, check if color/colour is in mapping OR fixed color in dots.
  if (!is.null(mapping)) {
    if (!( "color" %in% names(mapping) ||
           "colour" %in% names(mapping) ||
           "color" %in% names(dots) ||
           "colour" %in% names(dots) )) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    # No mapping provided: if a fixed color was supplied, omit the default color mapping.
    if ("color" %in% names(dots) || "colour" %in% names(dots)) {
      mapping <- ggplot2::aes(t = after_stat(t))
    } else {
      mapping <- default_mapping
    }
  }

  # Map 'id' to 'group' if provided.
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
      type = "stream",
      lineend = lineend,
      linejoin = linejoin,
      linemitre = linemitre,
      arrow.fill = arrow.fill,
      ...
    )
  )
}

#' @rdname geom_stream
#' @format NULL
#' @usage NULL
#' @export
StatStream <- ggproto("StatStream", Stat,
  required_aes = c("x", "y"),

  optional_aes = c("id", "t"),

  default_aes = aes(x = NA, y = NA, length = 1,
                    color = "black", fill = "black",
                    linewidth = 1, linetype = 1, alpha = 1),

  compute_group = function(data, scales, ...) {

    if (!"t" %in% names(data)) {
      data$t <- if(nrow(data) == 1) 0 else seq(0, 1, length.out = nrow(data))
    }

    # Order the data by t to ensure the points are connected in the intended sequence
    data <- data[order(data$t), , drop = FALSE]

    # Compute the normalization factor using the ranges of x and y
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
      # col   = scales::alpha(data$colour, 1),
      col   = scales::alpha(data$colour, data$alpha),
      lwd   = data$linewidth,
      lty   = data$linetype
    )
  )

}


#' @rdname geom_stream
#' @format NULL
#' @usage NULL
#' @export
GeomStream <- ggproto("GeomStream", GeomPath,
  # required_aes = c("x", "y"),
  default_aes = modifyList(
    GeomPath$default_aes,
    list("alpha" = 1, "linewidth" = 1, "length" = after_stat(NA_real_), fx = NA, fy = NA, z = NA)
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

      if (length(group_of_zero_fun) > 0) {
        data <- subset(data, !(group %in% group_of_zero_fun))
      }
    }

    data

  },

  # Override the draw_group method
  draw_panel = function(data, panel_params, coord, tail_point = FALSE, eval_point = FALSE, arrow, type, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", linemitre = 10) {

    # prep grobs for future use
    grobs <- list()
    stream_grob <- grid::nullGrob()
    tail_point_grob <- grid::nullGrob()
    eval_point_grob <- grid::nullGrob()

    ## do all work for stream grob seperate from vector grob.  have to munch here
    if(type == "stream"){

      fix_linewidth <- function(data, name) {
        if (is.null(data$linewidth) && !is.null(data$size)) {
          deprecate_soft0("3.4.0", I(paste0("Using the `size` aesthetic with ", name)), I("the `linewidth` aesthetic"))
          data$linewidth <- data$size
        }
        data
      }

      data <- fix_linewidth(data, snake_class(self))
      if (!anyDuplicated(data$group)) {
        cli::cli_inform(c(
          "{.fn {snake_class(self)}}: Each group consists of only one observation.",
          i = "Do you need to adjust the {.field group} aesthetic?"
        ))
      }

      # must be sorted on group
      data <- data[order(data$group), , drop = FALSE]
      munched <- coord_munch(coord, data, panel_params)

      # Silently drop lines with less than two points, preserving order
      rows <- stats::ave(seq_len(nrow(munched)), munched$group, FUN = length)
      munched <- munched[rows >= 2, ]
      if (nrow(munched) < 2) return(zeroGrob())

      # Work out whether we should use lines or segments
      attr <- ggplot2:::dapply(munched, "group", function(df) {
        linetype <- ggplot2:::unique0(df$linetype)
        ggplot2:::data_frame0(
          solid = length(linetype) == 1 && (identical(linetype, "solid") || linetype == 1),
          constant = nrow(ggplot2:::unique0(df[, names(df) %in% c("alpha", "colour", "linewidth", "linetype")])) == 1,
          .size = 1
        )
      })
      solid_lines <- all(attr$solid)
      constant <- all(attr$constant)
      if (!solid_lines && !constant) {
        cli::cli_abort("{.fn {snake_class(self)}} can't have varying {.field colour}, {.field linewidth}, and/or {.field alpha} along the line when {.field linetype} isn't solid.")
      }

      # Work out grouping variables for grobs
      n <- nrow(munched)
      group_diff <- munched$group[-1] != munched$group[-n]
      start <- c(TRUE, group_diff)
      end <-   c(group_diff, TRUE)

      # munched$fill <- arrow.fill %||% munched$colour
      # munched$fill <- munched$colour
      munched$fill <- if (!is.null(arrow.fill)) arrow.fill else munched$colour

      n <- nrow(munched)
      group_diff <- munched$group[-1] != munched$group[-n]
      start <- c(TRUE, group_diff)
      end <-   c(group_diff, TRUE)


      arrow <- ggplot2:::repair_segment_arrow(arrow, munched$group)

      stream_grob <- grid::segmentsGrob(
        munched$x[!end], munched$y[!end], munched$x[!start], munched$y[!start],
        default.units = "native", arrow = arrow,
        gp =   grid::gpar(
          col = alpha(munched$colour, munched$alpha)[!end],
          fill = alpha(munched$fill, munched$alpha)[!end],
          lwd = munched$linewidth[!end],
          lty = munched$linetype[!end],
          lineend = lineend,
          linejoin = linejoin,
          linemitre = linemitre
        )
      )

    }


    if(type == "vector"){

      ## need to undo normalizing if length is mapped - this happens under the following conditions
      if ("length" %in% names(data) && all(!is.na(data$length)) && "avg_spd" %in% names(data)) {

        df_out <- data  # Make a copy so we can modify in place

        # Loop over each group
        for (g in unique(df_out$group)) {
          # Identify the two rows for group g
          idx <- which(df_out$group == g)
          # row 1 = start, row 2 = end
          i1 <- idx[1]
          i2 <- idx[2]

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

    ## once data is transformed to coords then renormalize the data
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

    if(type == "vector"){

      stream_grob <- grid::polylineGrob(
        x = grid::unit(coords$x, "npc") + grid::unit(coords$offset_x, "cm"),
        y = grid::unit(coords$y, "npc") + grid::unit(coords$offset_y, "cm"),
        id = coords$group,  # Handle grouping for multiple paths
        default.units = "native",  # Use native units for scaling
        gp = grid::gpar(
          col =  coords[!duplicated(coords$group), "colour"],
          fill = coords[!duplicated(coords$group), "colour"],
          lwd = coords[!duplicated(coords$group), "linewidth"],
          linetype = coords[!duplicated(coords$group), "linetype"],
          alpha = coords[!duplicated(coords$group), "alpha"]
        ), arrow = arrow
      )
    }

    if (tail_point) {

      first_coords <- orig_coords[!duplicated(orig_coords$group),]

      tail_point_grob <- grid::pointsGrob(
        x = grid::unit(first_coords$x, "npc"),
        y = grid::unit(first_coords$y, "npc"),
        pch = 16,
        # solid circle; change as needed
        size = unit(coords$size %||% 1.35, "mm"),
        gp = grid::gpar(col = first_coords$colour, alpha = first_coords$alpha)
        # gp = grid::gpar(col = first_coords$colour, alpha = 1)
      )
    }

    if (eval_point) {

      first_coords_for_eval_point <- coords_for_eval_point[!duplicated(coords_for_eval_point$group),]

      eval_point_grob <- grid::pointsGrob(
        x = grid::unit(first_coords_for_eval_point$x, "npc"),
        y = grid::unit(first_coords_for_eval_point$y, "npc"),
        pch = 16, # solid circle
        size = unit(coords$size %||% 1.35, "mm"),
        gp = grid::gpar(col = first_coords_for_eval_point$colour,
                        alpha = first_coords_for_eval_point$alpha)
      )
    }

    # Combine the line and points grobs so that both are drawn.
    grobs <- list(stream_grob, tail_point_grob, eval_point_grob)
    grobs <- Filter(Negate(is.null), grobs)  # Remove NULL entries
    return(grid::grobTree(do.call(grid::gList, grobs)))

  },
  # draw_key = draw_key_length
  draw_key = function(data, params, size) {
    if (!is.na(data$length[1])) {
      draw_key_length(data, params, size)
    } else {
      ggplot2::draw_key_path(data, params, size)
    }
  }

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
#' @param ... Other arguments passed to \code{continuous_scale()}.
#'
#' @return If \code{max_range} is less than or equal to 0.5 (the default), a
#'   continuous scale object (typically of class \code{"ScaleContinuous"})
#'   mapping the \code{length} aesthetic is returned. If \code{max_range} is
#'   greater than 0.5, a list is returned with two components:
#'   \itemize{
#'     \item the continuous scale object, and
#'     \item a theme modification (a \code{theme} object) that adjusts the legend key width based
#'           on the value of \code{max_range}.
#'   }
#'
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
