#' Add a Wiggly Stream Geom Layer
#'
#' `geom_wigglystream()` creates a geom layer that visualizes stream-like data with wiggly paths. This is useful for representing flows, trajectories, or any data that benefits from a dynamic, flow-based visualization.
#'
#' @inheritParams ggplot2::geom_path
#' @param fun A function that defines the stream's dynamics. It should accept two arguments (`x` and `y`) and return a list with components `dx` and `dy`, representing the derivatives in the x and y directions, respectively.
#' @param xlim Numeric vector of length two. The x-axis limits for the stream computation. Default is `c(-1, 1)`.
#' @param ylim Numeric vector of length two. The y-axis limits for the stream computation. Default is `c(-1, 1)`.
#' @param n Integer. Number of initial stream points to generate across the grid. Default is `11`.
#' @param max_it Integer. Maximum number of iterations for the numerical integration. Controls how far each streamline is computed. Default is `1000`.
#' @param dt Numeric. Time step size for the numerical integration. Smaller values lead to more precise integrations but increase computation time. Default is `0.0025`.
#' @param L Numeric. Maximum arc length for each streamline. Determines how long each streamline can grow before stopping. Default is `1`.
#' @param center Logical. If `TRUE`, centers the streamlines around the midpoint of the initial grid. Useful for symmetric visualizations. Default is `FALSE`.
#' @param method Character. Integration method to use (e.g., `"rk4"` for Runge-Kutta 4). Determines the numerical solver's accuracy and stability. Default is `"rk4"`.
#' @param arrow A `grid::arrow` object specifying arrow properties for stream ends. Defaults to a closed arrow with a 30-degree angle and length of `0.02` npc units. Customize to change the appearance of stream arrows.
#' @param ... Additional arguments passed on to the underlying `geom_path()`. These are often aesthetics, used to set aesthetic attributes to fixed values, such as `color = "red"` or `size = 1.5`.
#'
#' @return A `ggplot2` layer object that can be added to a ggplot. It renders wiggly streamlines based on the provided data and parameters.
#'
#' @details
#' The `geom_wigglystream` function integrates seamlessly with `ggplot2`, allowing users to add dynamic and visually appealing streamlines to their plots. It leverages the `StatWigglyStream` for computing the stream data and `GeomWigglyStream` for rendering the paths with optional arrows indicating flow direction.
#'
#' Users can customize the behavior and appearance of the streamlines through various parameters:
#'
#' - **Stream Dynamics (`fun`):** Define how the stream evolves by specifying a function that calculates the derivatives at each point.
#' - **Integration Parameters (`dt`, `max_it`, `method`):** Control the numerical integration's precision and computational effort.
#' - **Visual Parameters (`color`, `size`, `arrow`):** Enhance the plot's aesthetics by adjusting colors, line widths, and arrow properties.
#'
#' **Example Use Cases:**
#'
#' - **Fluid Flow Visualization:** Representing the flow of liquids or gases in a confined space.
#' - **Trajectory Mapping:** Showing the paths of moving objects or agents over time.
#' - **Gradient Fields:** Visualizing vector fields in mathematical or physical contexts.
#'
#' @seealso
#' - `\code{\link{StatWigglyStream}}`: Statistical transformation used by `geom_wigglystream`.
#' - `\code{\link{GeomWigglyStream}}`: Geometric object responsible for rendering the streamlines.
#' - `\code{\link[ggplot2]{geom_path}}`: Base geom that `geom_wigglystream` builds upon.
#'
#' @examples
#'
#' f <- efield_maker()
#' f <- function(u) c(-u[2], u[1])
#' ggplot() + geom_vector_field(fun = f, xlim = c(-2,2), ylim = c(-2,2))
#' ggplot() + geom_wigglystream(fun = f, xlim = c(-2,2), ylim = c(-2,2), center = TRUE)
#' ggplot() +
#'   geom_vector_field(fun = f, xlim = c(-2,2), ylim = c(-2,2)) +
#'   geom_wigglystream(fun = f, xlim = c(-2,2), ylim = c(-2,2), center = TRUE)
#'
#' @rdname geom_wigglystream
#' @export
geom_wigglystream <- function(mapping = NULL, data = NULL,
                            stat = "wigglystream", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = TRUE,
                            inherit.aes = TRUE,
                            fun,
                            xlim = c(-1, 1),
                            ylim = c(-1, 1),
                            n = 11,
                            max_it = 1000,
                            dt = .0025,
                            L = NULL,
                            center = FALSE,
                            method = "rk4",
                            arrow = grid::arrow(angle = 30,
                                                length = unit(0.02, "npc"),
                                                type = "closed")
                            ) {

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)
  layer(
    stat = StatWigglyStream,
    data = data,
    mapping = mapping,
    geom = GeomWigglyStream,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      method = method,
      na.rm = na.rm,
      max_it = max_it,
      dt = dt,
      L = L,
      center = center,
      arrow = arrow,
      ...
    )
  )
}


#' @rdname geom_wigglystream
#' @export
# Stat function for stream plot computation
stat_wigglystream <- function(mapping = NULL, data = NULL,
                            stat = "wigglystream", position = "identity",
                            ...,
                            na.rm = FALSE,
                            show.legend = TRUE,
                            inherit.aes = TRUE,
                            fun,
                            xlim = c(-1, 1),
                            ylim = c(-1, 1),
                            n = 11,
                            max_it = 1000,
                            dt = .0025,
                            L = NULL,
                            center = FALSE,
                            method = "rk4",
                            arrow = grid::arrow(angle = 30,
                                                length = unit(0.02, "npc"),
                                                type = "closed")
                            ) {

  if (is.null(data)) data <- ensure_nonempty_data(data)
  layer(
    stat = StatWigglyStream,
    data = data,
    mapping = mapping,
    geom = GeomWigglyStream,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fun = fun,
      xlim = xlim,
      ylim = ylim,
      n = n,
      method = method,
      na.rm = na.rm,
      max_it = max_it,
      dt = dt,
      L = L,
      center = center,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname geom_wigglystream
#' @format NULL
#' @usage NULL
#' @export
# Define the StatWigglyStream object and computation for stream plots
StatWigglyStream <- ggproto("StatWigglyStream", Stat,

                          default_aes = aes(group = after_stat(pt), color = after_stat(t)),

                          compute_group = function(data, scales, fun, xlim, ylim, n, method, max_it, dt, L, center, ...) {

                            n_grid <- n[1]

                            # if(is.null(L)) L <- (min(diff(xlim), diff(ylim)) / (n_grid - 1))
                            if(is.null(L)) L <- (min(diff(xlim), diff(ylim)) / (n_grid - 1)) * 0.9

                            grid <- expand.grid(x = seq(xlim[1],xlim[2],len=n_grid), y = seq(ylim[1],ylim[2],len=n_grid) ) |> as.matrix()
                            df <- data.frame()


                            for (i in 1:nrow(grid)) {
                              df <- rbind(
                                df,
                                transform(
                                  ode_stepper(grid[i,], L = L, center = center, method = "euler", max_it = max_it, dt = dt, f_wrapper = f_wrapper()),
                                  "pt" = i)
                              )
                            }

                            # print(head(df))

                            df

                          }
)

#' @rdname geom_wigglystream
#' @format NULL
#' @usage NULL
#' @export
# Geom prototype for stream plot
GeomWigglyStream <- ggproto("GeomWigglyStream", GeomPath)
# GeomWigglyStream <- ggproto("GeomWigglyStream", GeomPath,
#                             required_aes = c("x", "y"),
#
#                             default_aes = aes(
#                               colour = "black",
#                               line_width = 0.5,
#                               linetype = 1,
#                               alpha = 1
#                             ),
#
#                             draw_panel = function(data, panel_params, coord, ...) {
#                               GeomPath$draw_panel(data, panel_params, coord, ...)
#                             },
#
#                             draw_key = draw_key_path
# )
