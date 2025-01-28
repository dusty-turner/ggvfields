#' Geom Vector Field
#'
#' `geom_vector_field()` creates a ggplot2 layer that visualizes a vector field by generating
#' streamlines based on a user-defined function. It leverages the underlying
#' [StatStreamField] and [GeomStream] to compute and render the streamlines, respectively.
#'
#' @param mapping Aesthetic mappings created by [ggplot2::aes()]. By default, it inherits
#'   aesthetics from the ggplot object. You can override or set specific aesthetics such as
#'   `color`, `size`, etc.
#' @param data A data frame or other object, as in [ggplot2::layer()]. If `NULL`, the layer
#'   uses the plot's data.
#' @param stat The statistical transformation to use on the data for this layer. Defaults to
#'   `StatStreamField`.
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
#'   aesthetics, and should not inherit behaviour from the main ggplot call.
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
#'   Useful for symmetric flows. Defaults to `TRUE`.
#' @param normalize Logical; if `TRUE`, normalizes each vector to a unit length before
#'   applying any scaling. This can help prevent overplotting in dense plots and
#'   ensures consistent visual representation.
#' @param arrow A [grid::arrow()] specification to add arrowheads to the streamlines, indicating
#'   direction. Defaults to a closed arrow with a 30-degree angle and length `0.02` npc.
#' @param geom The geometric object used to draw the streamline.
#'
#' @return A ggplot2 **Layer** object that can be added to a plot. It computes the streamlines
#'   based on the specified vector field function and visualizes them.
#'
#' @details
#' - **Vector Field Function (`fun`)**: The function should encapsulate the behavior of the vector field.
#'   For example, a rotational field can be defined as `function(u) { c(-u[2], u[1]) }`.
#' - **Integration Parameters**:
#'   - `dt`: Time-step size for numerical integration. Smaller values yield more precise streamlines.
#'   - `L`: Maximum arc length for each streamline. If `NULL`, a default based on grid spacing is used.
#'   - `method`: Numerical integration method, such as `"euler"` or `"rk4"` (Runge-Kutta 4).
#' - **Aesthetic Mappings**: Streamline aesthetics like `color`, `size`, and `linetype` can be customized
#'   via the `mapping` parameter or by setting them directly in the `geom_vector_field` call.
#'
#' @section Aesthetics:
#' `geom_vector_field()` understands the following aesthetics (optional):
#' - `color`: Color of the streamlines.
#' - `size`: Thickness of the streamlines.
#' - `linetype`: Line type of the streamlines.
#' - `alpha`: Transparency level of the streamlines.
#'
#' @examples
#' # Define a simple rotational vector field function
#' rotational_field <- function(u) {
#'   x <- u[1]
#'   y <- u[2]
#'   c(-y, x)  # Circular flow around the origin
#' }
#'
#' # Create a ggplot with the vector field using geom_vector_field
#' ggplot() +
#'   geom_vector_field(
#'     fun = rotational_field
#'     )
#'
#'
#' @export
geom_vector_field <- function(mapping = NULL, data = NULL,
                              stat = StatVectorField,
                              position = "identity",
                              ...,
                              na.rm = FALSE,
                              show.legend = TRUE,
                              inherit.aes = TRUE,
                              fun,
                              xlim = c(-1, 1),
                              ylim = c(-1, 1),
                              n = 11,
                              center = TRUE,
                              normalize = TRUE,
                              arrow = grid::arrow(angle = 30,
                                                  length = unit(0.02, "npc"),
                                                  type = "closed")
) {

  # Define default mapping for geom_vector_field
  default_mapping <- aes(color = after_stat(norm))

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
      method = "euler",
      na.rm = na.rm,
      dt = 1,
      L = .1,
      center = center,
      normalize = normalize,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname geom_vector_field
#' @export
#'
stat_vector_field <- function(mapping = NULL, data = NULL,
                              stat = StatVectorField,
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
                              center = TRUE,
                              normalize = TRUE,
                              arrow = grid::arrow(angle = 30,
                                                  length = unit(0.02, "npc"),
                                                  type = "closed")
) {

  # Define default mapping for geom_vector_field
  default_mapping <- aes(color = after_stat(norm))

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
      L = .1,
      center = center,
      normalize = normalize,
      arrow = arrow,
      ...
    )
  )
}


#' @rdname geom_vector_field
#' @format NULL
#' @usage NULL
#' @export
StatVectorField <- ggproto("StatVectorField", Stat,

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
                                   ode_stepper(grid[i,], fun = fun, dt = dt, L = L, max_it = max_it, method = method),
                                   "pt" = i)
                               )
                             }

                             # df |> slice(25:26) |> print()
                             df$norm <- ave(df$l, df$pt, FUN = max)

                             if(normalize){
                               space_between <- (min(diff(xlim), diff(ylim)) / (n_grid - 1)) * .8

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

                               # Remove temporary columns if no longer needed
                               df$lag_x <- NULL
                               df$lag_y <- NULL
                               df$dx <- NULL
                               df$dy <- NULL
                               df$new_dx <- NULL
                               df$new_dy <- NULL

                             }
                               if(center){

                                 # df <-
                                 # df |>
                                 #   group_by(pt) |>
                                 #   mutate(mid_x = ifelse(t == 1, (x - lag(x))/2, (lead(x) - x)/2)) |>
                                 #   mutate(mid_y = ifelse(t == 1, (y - lag(y))/2, (lead(y) - y)/2)) |>
                                 #   mutate(x = x - mid_x) |>
                                 #   mutate(y = y - mid_y)

                                 # 1. Create lag_x and lead_x within each 'pt' group
                                 df$lag_x <- ave(df$x, df$pt, FUN = function(x) c(NA, head(x, -1)))
                                 df$lead_x <- ave(df$x, df$pt, FUN = function(x) c(tail(x, -1), NA))

                                 df$lag_y <- ave(df$y, df$pt, FUN = function(y) c(NA, head(y, -1)))
                                 df$lead_y <- ave(df$y, df$pt, FUN = function(y) c(tail(y, -1), NA))

                                 # 2. Calculate 'mid_x' and 'mid_y' based on the condition 't == 1'
                                 df$mid_x <- ifelse(df$t == 1, (df$x - df$lag_x) / 2, (df$lead_x - df$x) / 2)
                                 df$mid_y <- ifelse(df$t == 1, (df$y - df$lag_y) / 2, (df$lead_y - df$y) / 2)

                                 # 3. Update 'x' and 'y' by subtracting 'mid_x' and 'mid_y'
                                 df$x <- df$x - df$mid_x
                                 df$y <- df$y - df$mid_y

                                 # 4. Remove temporary columns if no longer needed
                                 df$lag_x <- NULL
                                 df$lead_x <- NULL
                                 df$lag_y <- NULL
                                 df$lead_y <- NULL
                                 df$mid_x <- NULL
                                 df$mid_y <- NULL

                               }



                             # print(head(df))

                             df

                           }
)
