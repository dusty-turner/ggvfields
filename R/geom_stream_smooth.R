#' Create a Smoothed Vector Field Layer
#'
#' `geom_stream_smooth()` creates a ggplot2 layer that visualizes a smooth
#' vector field based on raw vector data. The function fits a multivariate
#' linear model (by default, using the formula `cbind(fx, fy) ~ x * y`) to
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
#' @param stat The statistical transformation to use on the data. Defaults to
#'   `"vector_smooth"`.
#' @param position Position adjustment, either as a string or the result of a
#'   position adjustment function.
#' @param formula A formula specifying the multivariate linear model used for
#'   smoothing. Defaults to `cbind(fx, fy) ~ x * y`.
#' @param eval_points A data frame of evaluation points, or `NULL`. When
#'   provided, it specifies the grid where the smoothing model is evaluated; if
#'   `NULL`, a grid is generated based on `n`.
#' @param n An integer vector specifying the grid resolution for smoothing.
#' @param lineend Line end style (round, butt, square).
#' @param linejoin Line join style (round, mitre, bevel).
#' @param linemitre Line mitre limit (number greater than 1).
#' @param method either "gam" (default) or "lm".
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
#'   polar coordinates (`angle` and `distance`) and computes \eqn{xend = x +
#'   distance \times \cos(angle \times 180/\pi),\quad yend = y + distance \times
#'   \sin(angle \times 180/\pi).} An error is thrown if neither set is
#'   available.
#'
#' **Smoothing:**
#'   The multivariate linear model is fitted using the provided `formula` and
#'   `data`. This model is then used to predict vector displacements at any
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
#' \dontrun{
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
#' n <- 20
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
#' # Add smoothed layer using default model
#' ggplot(df) +
#'   geom_vector(aes(x, y, fx = fx, fy = fy)) +
#'   geom_stream_smooth(formula = cbind(fx, fy) ~ x * y)
#'
#' # Use a more complex polynomial model
#' ggplot(df) +
#'   geom_vector(aes(x, y, fx = fx, fy = fy)) +
#'   geom_stream_smooth(formula = cbind(fx, fy) ~ poly(x, 2) * poly(y, 2), data = df)
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
   stat = StatStreamField,
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

  # Inspect the LHS of the formula. If it is cbind(xend, yend), then change it
  lhs <- formula[[2]]
  if (is.call(lhs) && identical(lhs[[1]], as.name("cbind"))) {
    lhs_vars <- sapply(as.list(lhs[-1]), as.character)
    if (all(lhs_vars %in% c("xend", "yend")) ||
        all(lhs_vars %in% c("distance", "angle"))) {
      # Change to use fx and fy instead
      formula[[2]] <- substitute(cbind(fx, fy))
    }
  }

  if (is.null(mapping)) mapping <- aes(x = x, y = y, fx = fx, fy = fy)

  n <- ensure_length_two(n)
  dots <- list(...)
  if (!("color" %in% names(dots))) {
    dots$color <- "blue"
  }

  # Define the vector field function. It retrieves the prepared data from the
  # parent environment (which comes from the stat after setup_data()).

  if(method == "gam") {

  vec_field <- function(u) {

    group_data <- try(get("data", envir = parent.frame()), silent = TRUE)
    s <- mgcv::s
    gam <- mgcv::gam
    # if (inherits(group_data, "try-error") || is.null(group_data)) {
    #   stop("Could not retrieve group data for vector field calculation.")
    # }
    # Fit the regression using the (possibly modified) formula.
      fx_mod <- gam(fx ~ s(x, y, bs = "tp"), data = group_data)
      fy_mod <- gam(fy ~ s(x, y, bs = "tp"), data = group_data)

      new_point <- data.frame(x = u[1], y = u[2])

      pred_fx <- predict(fx_mod, newdata = new_point[, c("x", "y")])
      pred_fy <- predict(fy_mod, newdata = new_point[, c("x", "y")])

      c(pred_fx, pred_fy)
    }
  }

  if(method == "lm"){

     vec_field <- function(u) {

       group_data <- try(get("data", envir = parent.frame()), silent = TRUE)

       # if (inherits(group_data, "try-error") || is.null(group_data)) {
       #   stop("Could not retrieve group data for vector field calculation.")
       # }
       # Fit the regression using the (possibly modified) formula.
       model <- lm(formula, data = group_data)
       newdata <- data.frame(x = u[1], y = u[2])
       as.numeric(predict(model, newdata = newdata))
     }
  }

  layer(
    stat = stat,
    data = data,
    mapping = mapping,
    geom = GeomStream,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(
      list(
        na.rm = na.rm,
        n = n,
        normalize = normalize,
        center = center,
        fun = vec_field,
        xlim = xlim,
        ylim = ylim,
        grid = eval_points,
        type = type,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre,
        arrow = arrow,
        ...
      ),
      dots
    )
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

  # Inspect the LHS of the formula. If it is cbind(xend, yend), then change it
  lhs <- formula[[2]]
  if (is.call(lhs) && identical(lhs[[1]], as.name("cbind"))) {
    lhs_vars <- sapply(as.list(lhs[-1]), as.character)
    if (all(lhs_vars %in% c("xend", "yend")) ||
        all(lhs_vars %in% c("distance", "angle"))) {
      # Change to use fx and fy instead
      formula[[2]] <- substitute(cbind(fx, fy))
    }
  }

  if (is.null(mapping)) mapping <- aes(x = x, y = y, fx = fx, fy = fy)

  n <- ensure_length_two(n)
  dots <- list(...)
  if (!("color" %in% names(dots))) {
    dots$color <- "blue"
  }

  # Define the vector field function. It retrieves the prepared data from the
  # parent environment (which comes from the stat after setup_data()).

  if(method == "gam") {

    vec_field <- function(u) {

      group_data <- try(get("data", envir = parent.frame()), silent = TRUE)
      s <- mgcv::s
      gam <- mgcv::gam
      # if (inherits(group_data, "try-error") || is.null(group_data)) {
      #   stop("Could not retrieve group data for vector field calculation.")
      # }
      # Fit the regression using the (possibly modified) formula.
      fx_mod <- gam(fx ~ s(x, y, bs = "tp"), data = group_data)
      fy_mod <- gam(fy ~ s(x, y, bs = "tp"), data = group_data)

      new_point <- data.frame(x = u[1], y = u[2])

      pred_fx <- predict(fx_mod, newdata = new_point[, c("x", "y")])
      pred_fy <- predict(fy_mod, newdata = new_point[, c("x", "y")])

      c(pred_fx, pred_fy)
    }
  }

  if(method == "lm"){

    vec_field <- function(u) {

      group_data <- try(get("data", envir = parent.frame()), silent = TRUE)

      # if (inherits(group_data, "try-error") || is.null(group_data)) {
      #   stop("Could not retrieve group data for vector field calculation.")
      # }
      # Fit the regression using the (possibly modified) formula.
      model <- lm(formula, data = group_data)
      newdata <- data.frame(x = u[1], y = u[2])
      as.numeric(predict(model, newdata = newdata))
    }
  }

  layer(
    stat = StatStreamField,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(
      list(
        na.rm = na.rm,
        n = n,
        normalize = normalize,
        center = center,
        fun = vec_field,
        xlim = xlim,
        ylim = ylim,
        grid = eval_points,
        type = type,
        lineend = lineend,
        linejoin = linejoin,
        linemitre = linemitre,
        arrow = arrow,
        ...
      ),
      dots
    )
  )
}
