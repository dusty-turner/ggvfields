#' Create a Smoothed Vector Field Layer
#'
#' `geom_stream_smooth()` creates a ggplot2 layer that visualizes a smooth
#' vector field based on raw vector data. By default (`method = "gam"`) the
#' function fits a thin plate spline smoother to each of `fx` and `fy`; with
#' `method = "lm"` it fits a multivariate linear model (by default, using the
#' formula `cbind(fx, fy) ~ x * y`). The fitted model is used to
#' predict the vector displacements at any given location. It also handles
#' different input formats by converting polar coordinates or endpoint data to
#' vector displacements.
#'
#' @inheritParams geom_vector
#' @inheritParams geom_stream_field
#'
#' @param mapping A set of aesthetic mappings created by \code{ggplot2::aes()}.
#'   **Required:** Must include **`x`** and **`y`**; vector displacements are defined
#'   by **`fx`** and **`fy`**.
#' @param data A data frame containing the raw vector data.
#' @param stat The statistical transformation. `geom_stream_smooth()` always
#'   uses [StatStreamSmooth] and `geom_gradient_smooth()` [StatGradientSmooth],
#'   which fit the model; another value is ignored with a warning.
#' @param position Position adjustment, either as a string or the result of a
#'   position adjustment function.
#' @param formula A formula specifying the multivariate linear model used for
#'   smoothing when `method = "lm"`. Defaults to `cbind(fx, fy) ~ x * y`. It is
#'   ignored, with a warning if supplied, when `method = "gam"`.
#' @param eval_points A data frame of evaluation points, or `NULL`. When
#'   provided, it specifies the grid where the smoothing model is evaluated; if
#'   `NULL`, a grid is generated based on `n`.
#' @param n An integer vector specifying the grid resolution for smoothing.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
#' @param method Either `"gam"` (default), which fits `fx` and `fy` separately
#'   with the smooth `s(x, y)`, or `"lm"`, which fits `formula`. The smooth
#'   uses mgcv's default basis of 30 functions; with fewer than 30 distinct
#'   locations it uses half as many functions as locations, and it needs at
#'   least 5 locations.
#' @param ... Additional arguments passed to the layer. If a fixed parameter
#'   `color` is not provided, then `color = "blue"` is used.
#'
#' @section Aesthetics: `geom_stream_smooth()` supports the following aesthetics
#'   (required aesthetics are in **bold**):
#'
#'   - **`x`**: The x-coordinate of the vector's starting point.
#'   - **`y`**: The y-coordinate of the vector's starting point.
#'   - **`fx`**: The displacement along the x-axis.
#'   - **`fy`**: The displacement along the y-axis.
#'   - `color`: The fixed color for the vector. Defaults to `"blue"`.
#'   - `linewidth`: The thickness of the vector line.
#'   - `linetype`: The type of the vector line (e.g., solid or dashed).
#'   - `alpha`: The transparency level of the vector.
#'   - `arrow`: Specifies arrowheads for the vectors.
#'
#' @section Details:
#' **Data Conversion:**
#'   If `xend`/`yend` are missing or all `NA`, the function computes them. It
#'   first checks for vector displacements (`fx` and `fy`); if present, it
#'   computes \eqn{xend = x + fx,\quad yend = y + fy.} Otherwise, it checks for
#'   polar coordinates (`angle`, in radians, and `distance`) and computes
#'   \eqn{xend = x + distance \times \cos(angle),\quad yend = y + distance \times
#'   \sin(angle).} An error is thrown if neither set is available.
#'
#' **Smoothing:**
#'   The model is fitted once per group: a thin plate spline smoother for each
#'   component when `method = "gam"`, or the multivariate linear model given by
#'   `formula` when `method = "lm"`. It is then used to predict vector displacements at any
#'   specified grid point, generating a smooth approximation of the vector
#'   field.
#'
#' **Prediction Intervals:**
#' Two types of prediction intervals can be displayed:
#'   - **Ellipse:** Depicts the joint uncertainty (covariance) in the predicted `fx` and `fy`.
#'   - **Wedge:** Indicates the range of possible vector directions and magnitudes.
#'
#' @return A ggplot2 layer that can be added to a ggplot object to display a
#'   smoothed vector field.
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' \describe{
#'  \item{norm}{Computed as the Euclidean norm of the displacement,
#'     \eqn{\sqrt{fx^2 + fy^2}}, this variable is used to normalize and scale the
#'     vector lengths.}
#'
#'   \item{t}{The integration time or evaluation time at each computed point along
#'     the smoothed field (when applicable).}
#'
#'   \item{d}{The incremental distance between consecutive computed points.}
#'
#'   \item{l}{The cumulative arc length along the smoothed vector field, calculated
#'     as the cumulative sum of \code{d}.}
#' }
#'
#' @examples
#'
#' \donttest{
#' # Define a true vector field function
#' f <- function(u) {
#'   x <- u[1]; y <- u[2]
#'   c(x^2 - y^2, x^2 + y^2 - 2)
#' }
#'
#' # Alternative example function
#' f <- function(u) c(-u[2], u[1])
#'
#' # Visualize the vector field
#' ggplot() + geom_stream_field(fun = f, xlim = c(-2, 2), ylim = c(-2, 2))
#'
#' # Generate design points
#' set.seed(1)
#' n <- 50
#' df <- data.frame(x = runif(n, -2, 2), y = runif(n, -2, 2))
#'
#' # Sample function values at design points
#' fdf <- as.data.frame(t(apply(df, 1, f)))
#' colnames(fdf) <- c("fx", "fy")
#' df <- cbind(df, fdf)
#'
#' # Visualize raw vector field data
#' ggplot(df) + geom_vector(aes(x, y, fx = fx, fy = fy))
#'
#' # Add smoothed layer using the default smoother
#' ggplot(df) +
#'   geom_vector(aes(x, y, fx = fx, fy = fy)) +
#'   geom_stream_smooth()
#'
#' # Use a linear model
#' ggplot(df) +
#'   geom_vector(aes(x, y, fx = fx, fy = fy)) +
#'   geom_stream_smooth(method = "lm", formula = cbind(fx, fy) ~ x * y)
#'
#' # Use a more complex polynomial model
#' ggplot(df) +
#'   geom_vector(aes(x, y, fx = fx, fy = fy)) +
#'   geom_stream_smooth(
#'     method = "lm",
#'     formula = cbind(fx, fy) ~ poly(x, 2) * poly(y, 2),
#'     data = df
#'   )
#'
#' # Fit a linear model and use it for prediction
#' fhat <- function(u) {
#'   model <- lm(cbind(fx, fy) ~ x * y, data = df)
#'   predict(model, newdata = data.frame(x = u[1], y = u[2])) |> as.numeric()
#' }
#'
#' # Visualize estimated field with the raw vector field
#' ggplot(df) +
#'   geom_stream_field(fun = fhat, normalize = FALSE, color = "#3366FF") +
#'   geom_vector(aes(x, y, fx = fx, fy = fy))
#'
#' # Generate a hexagonal grid
#' hex_lattice <- grid_hex(xlim = c(-5, 5), ylim = c(-5, 5), d = 1)
#'
#' # Use the hexagonal grid in geom_stream_field
#' ggplot(data = df) +
#'   geom_vector(aes(x, y, fx = fx, fy = fy), color = "black", normalize = FALSE) +
#'   geom_stream_smooth(eval_points = hex_lattice)
#'
#' # user specified point
#'
#' eval_pts <- data.frame(x = c(0, 1), y = c(2, -1))
#'
#' ggplot(data = df) +
#'   geom_vector(aes(x, y, fx = fx, fy = fy), color = "black", normalize = FALSE) +
#'   geom_stream_smooth(eval_points = eval_pts)
#' }
#'
#' @name geom_stream_smooth
#' @aliases geom_stream_smooth stat_stream_smooth
#' @export
NULL

#' @rdname geom_stream_smooth
#' @export
geom_stream_smooth <- function(mapping = NULL,
  data = NULL,
   stat = StatStreamSmooth,
   position = "identity",
   ...,
   na.rm = FALSE,
   show.legend = NA,
   inherit.aes = TRUE,
   n = 11,
   xlim = NULL,
   ylim = NULL,
   normalize = TRUE,
   center = FALSE,
   type = "vector",
   formula = cbind(fx, fy) ~ x * y,
   method = "gam",
   eval_points = NULL,
   lineend = "butt",
   linejoin = "round",
   linemitre = 10,
   arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
) {

  call_args <- as.list(environment())

  warn_if_stat_ignored(stat, StatStreamSmooth, "stream_smooth", "geom_stream_smooth")

  construct_layer(
    stream_smooth_layer, call_args, list(...),
    stat = StatStreamSmooth,
    geom = GeomStream,
    formula_supplied = !missing(formula)
  )
}


#' @rdname geom_stream_smooth
#' @export
stat_stream_smooth <- function(mapping = NULL,
  data = NULL,
   geom = GeomStream,
   position = "identity",
   ...,
   na.rm = FALSE,
   show.legend = NA,
   inherit.aes = TRUE,
   n = 11,
   xlim = NULL,
   ylim = NULL,
   normalize = TRUE,
   center = FALSE,
   type = "vector",
   method = "gam",
   formula = cbind(fx, fy) ~ x * y,
   eval_points = NULL,
   lineend = "butt",
   linejoin = "round",
   linemitre = 10,
   arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
) {

  call_args <- as.list(environment())

  construct_layer(
    stream_smooth_layer, call_args, list(...),
    stat = StatStreamSmooth,
    formula_supplied = !missing(formula)
  )
}
