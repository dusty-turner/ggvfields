#' Create a Stream Field Layer in ggplot2
#'
#' `geom_stream_field()` creates a ggplot2 layer that evaluates a user-defined
#' function \eqn{f(x, y) \to (dx, dy)} over a grid of \((x, y)\) values, then
#' numerically integrates those derivatives to form streamlines. The resulting
#' coordinates are passed to [GeomStream] for plotting, producing a streamlined
#' visualization of vector fields.
#'
#' @inheritParams ggplot2::geom_path
#'
#' @param fun A function of two variables, `fun(x, y)`, returning a
#'   two-element vector \((dx, dy)\). This defines the local "flow" direction.
#' @param xlim,ylim Numeric vectors of length two specifying the domain limits
#'   in the \(\,x\)- and \(\,y\)-directions, respectively. Defaults to
#'   `c(-1, 1)` for both.
#' @param n Integer. Grid resolution (the number of seed points along each axis).
#'   Defaults to 11.
#' @param max_it Integer. Maximum number of integration steps per streamline.
#'   Controls how far each streamline can propagate. Defaults to 1000.
#' @param dt Numeric. Time-step size for integration. Smaller values produce more
#'   precise streamlines at the expense of computation time. Default `0.0025`.
#' @param L Numeric. Maximum arc length for each streamline. A streamline stops
#'   once its length exceeds this value. If `NULL`, a suitable default is derived
#'   from the grid spacing. Default `NULL`.
#' @param center Logical. If `TRUE`, center the seed points around the midpoint
#'   of the domain (useful for symmetric flows). Default `FALSE`.
#' @param normalize Logical; if `TRUE`, normalizes each vector to a unit length before
#'   applying any scaling. This can help prevent overplotting in dense plots and
#'   ensures consistent visual representation.
#' @param method Character. Integration method (e.g., `"rk4"` for Runge-Kutta 4,
#'   or `"euler"`). Defaults to `"rk4"`.
#' @param arrow A [grid::arrow()] specification for adding arrowheads to each
#'   streamline. By default, a closed arrow with 30-degree angle and length
#'   `0.02` npc is used.
#' @param geom The geometric object used to draw the streamline. Defaults to
#'   [ggplot2::GeomPath] in `geom_stream()`, or [GeomStream] in `stat_stream()`.
#' @param ... Other arguments passed on to [ggplot2::layer()] and the
#'   underlying geometry/stat. Often used to set aesthetics like `color = "red"`
#'   or `size = 1.5`.
#'
#' @return A ggplot2 **Layer** object that can be added to a plot. It computes
#'   the streamlines over the specified domain and draws them, optionally with
#'   arrowheads, to represent the direction of the flow.
#'
#' @details
#' - **Domain & Grid**: The domain is defined by `xlim` and `ylim`, and a regular
#'   2D grid of \eqn{x \times y} seed points is placed there (optionally centered).
#' - **Integration**: Each seed point is advanced using the chosen `method` and
#'   `dt`. Streamlines terminate upon exceeding `max_it` steps or length `L`.
#' - **Visualization**: The integrated (x, y) path is passed to a specialized
#'   geometry ([GeomStream] by default), allowing you to visualize vector fields,
#'   flow patterns, or trajectories.
#'
#' This approach can be used to illustrate fluid flows, gradient fields, or any
#' continuous mapping from \((x, y)\) to \((dx, dy)\).
#'
#' @section See Also:
#' - [StatStreamField] for the underlying statistical transformation.
#' - [GeomStream] for the geometry that renders the resulting paths.
#' - [ggplot2::geom_path] as the base geometry on which `GeomStream` is built.
#'
#' @examples
#'
#' # Define a function that returns (dx, dy)
#' f <- function(u) {
#'    x <- u[1]
#'    y <- u[2]
#'    c(-x^2 + y - 1, x - y^2 + 1)
#'    }
#'
#' ggplot() +
#'   geom_stream_field(fun = f)
#'
#' @name geom_stream_field
#' @export
geom_stream_field <- function(mapping = NULL, data = NULL,
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
                              max_it = 1000,
                              dt = .0025,
                              L = NULL,
                              center = FALSE,
                              normalize = FALSE,
                              method = "rk4",
                              arrow = grid::arrow(angle = 30,
                                                  length = unit(0.02, "npc"),
                                                  type = "closed")
) {

  # Define default mapping for geom_vector_field
  default_mapping <- aes(color = after_stat(max(l)/max_t))

  # Merge user-provided mapping with default mapping
  # User mapping takes precedence
  if (!is.null(mapping)) {
    if (!"color" %in% names(mapping)) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    mapping <- default_mapping
  }

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  layer(
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
      method = method,
      na.rm = na.rm,
      max_it = max_it,
      dt = dt,
      L = L,
      center = center,
      normalize = normalize,
      arrow = arrow,
      ...
    )
  )
}


#' @rdname geom_stream_field
#' @export
# Stat function for stream plot computation
stat_stream_field <- function(mapping = NULL, data = NULL,
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
                              max_it = 1000,
                              dt = .0025,
                              L = NULL,
                              center = FALSE,
                              normalize = FALSE,
                              method = "rk4",
                              arrow = grid::arrow(angle = 30,
                                                  length = unit(0.02, "npc"),
                                                  type = "closed")
) {

  # Define default mapping for geom_vector_field
  default_mapping <- aes(color = after_stat(max(l)/max_t))

  # Merge user-provided mapping with default mapping
  # User mapping takes precedence
  if (!is.null(mapping)) {
    if (!"color" %in% names(mapping)) {
      mapping <- modifyList(default_mapping, mapping)
    }
  } else {
    mapping <- default_mapping
  }

  if (is.null(data)) data <- ensure_nonempty_data(data)
  n <- ensure_length_two(n)

  layer(
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
      method = method,
      na.rm = na.rm,
      max_it = max_it,
      dt = dt,
      L = L,
      center = center,
      normalize = normalize,
      arrow = arrow,
      ...
    )
  )
}


#' @rdname geom_stream_field
#' @format NULL
#' @usage NULL
#' @export
StatStreamField <- ggproto("StatStreamField", Stat,

                            default_aes = aes(group = after_stat(pt)),

                            compute_group = function(data, scales, fun, xlim, ylim, n, method, max_it = 1000, dt, L, center, normalize, ...) {

                              n_grid <- n[1]

                              if(is.null(L)) L <- (min(diff(xlim), diff(ylim)) / (n_grid - 1)) * 0.9

                              grid <- expand.grid(x = seq(xlim[1],xlim[2],len=n_grid), y = seq(ylim[1],ylim[2],len=n_grid) ) |> as.matrix()
                              df <- data.frame()


                              for (i in 1:nrow(grid)) {
                                df <- rbind(
                                  df,
                                  transform(
                                    # ode_stepper(grid[i,], L = L, center = center, method = method, max_it = max_it, dt = dt, f_wrapper = f_wrapper()),
                                    ode_stepper(grid[i,], fun = fun, dt = dt, L = L, max_it = max_it, method = method, center = center),
                                    "pt" = i)
                                )
                              }

                              # df |> slice(25:26) |> print()
                                df$norm <- ave(df$l, df$pt, FUN = max)

                              if(normalize){
                                space_between <- (min(diff(xlim), diff(ylim)) / (n_grid - 1)) * .8

                                # df <-
                                #   df |>
                                #   group_by(pt) |>
                                #   mutate(norm = max(l)) |>
                                #   mutate(dx = ifelse(t == 1, x - lag(x), 0)) |>
                                #   mutate(dy = ifelse(t == 1, y - lag(y), 0)) |>
                                #   mutate(new_dx = dx / norm) |>
                                #   mutate(new_dy = dy / norm) |>
                                #   mutate(x = ifelse(t == 1, lag(x) + new_dx * space_between, x)) |>
                                #   mutate(y = ifelse(t == 1, lag(y) + new_dy * space_between, y)) |>
                                #   mutate(new_norm = ifelse(t == 1, sqrt((x-lag(x))^2 + (y-lag(y))^2), 0))

                                # Assuming 'df' is your data frame and 'space_between' is defined

                                # 1. Calculate 'norm' as the maximum of 'l' within each 'pt' group

                                # 2. Create lagged versions of 'x' and 'y' within each 'pt' group
                                df$lag_x <- ave(df$x, df$pt, FUN = function(x) c(NA, head(x, -1)))
                                df$lag_y <- ave(df$y, df$pt, FUN = function(y) c(NA, head(y, -1)))

                                # 3. Calculate 'dx' and 'dy'
                                df$dx <- ifelse(df$t == 1, df$x - df$lag_x, 0)
                                df$dy <- ifelse(df$t == 1, df$y - df$lag_y, 0)

                                # 4. Calculate 'new_dx' and 'new_dy'
                                df$new_dx <- df$dx / df$norm
                                df$new_dy <- df$dy / df$norm

                                # 5. Update 'x' and 'y' based on the conditions
                                df$x <- ifelse(df$t == 1, df$lag_x + df$new_dx * space_between, df$x)
                                df$y <- ifelse(df$t == 1, df$lag_y + df$new_dy * space_between, df$y)

                                # 6. Calculate 'new_norm'
                                df$new_norm <- ifelse(df$t == 1, sqrt((df$x - df$lag_x)^2 + (df$y - df$lag_y)^2), 0)

                                # (Optional) Remove temporary columns if no longer needed
                                df$lag_x <- NULL
                                df$lag_y <- NULL
                                df$dx <- NULL
                                df$dy <- NULL
                                df$new_dx <- NULL
                                df$new_dy <- NULL


                              }

                              # print(head(df))

                              df

                            }
)


