#' Create a Smoothed Vector Field Layer
#'
#' `geom_stream_smooth` creates a ggplot2 layer that visualizes a smooth vector
#' field based on raw vector data. The function fits a multivariate linear model
#' (by default, using the formula `cbind(fx, fy) ~ x * y`) to predict the vector
#' displacements at any given location. It also handles different input formats
#' by converting polar coordinates or endpoint data to vector displacements.
#'
#' @inheritParams geom_vector
#' @inheritParams geom_stream_field
#' @inheritParams ggplot2::layer
#' @param formula A formula specifying the multivariate linear model used for
#'   smoothing. Defaults to `cbind(fx, fy) ~ x * y`.
#' @param ... Additional arguments passed on to the layer. In addition, if a
#'   fixed parameter `color` is not provided via `...`, then `color = "blue"` is
#'   used.
#'
#' @return A ggplot2 layer that can be added to a ggplot object to display a
#'   smoothed vector field.
#'
#' @details
#' **Data Conversion:**
#' If `xend`/`yend` are missing or all `NA`, then the function will attempt to
#' compute them. First it checks for vector displacements (`fx` and `fy`); if
#' these exist (and are not all missing), the endpoints are computed as:
#' \deqn{xend = x + fx,\quad yend = y + fy.} If not, the function looks for
#' polar coordinates (`angle` and `distance`) and computes: \deqn{xend = x +
#' distance \times \cos(angle \times 180/\pi),\quad yend = y + distance \times
#' \sin(angle \times 180/\pi).} If neither set is available, the function stops
#' with an error.
#'
#' **Smoothing:**
#' The multivariate linear model is fitted using the provided `formula` and
#' `data`. This model is then used to predict vector displacements at any
#' specified grid point, generating a smooth approximation of the vector field.
#'
#' @section Aesthetics: `geom_stream_smooth` supports the following aesthetics
#'   (required aesthetics are in **bold**):
#'
#' - **`x`**: The x-coordinate of the vector's starting point.
#' - **`y`**: The y-coordinate of the vector's starting point.
#' - **`fx`**: The displacement along the x-axis.
#' - **`fy`**: The displacement along the y-axis.
#' - `color`: The fixed color for the vector. Defaults to `"blue"`.
#' - `linewidth`: The thickness of the vector line.
#' - `linetype`: The type of the vector line (e.g., solid or dashed).
#' - `alpha`: The transparency level of the vector.
#' - `arrow`: Specifies arrowheads for the vectors.
#'
#' @examples
#'
#' # set true vector field
#' f <- function(u) {
#'   x <- u[1]; y <- u[2]
#'   c(x^2 - y^2, x^2 + y^2 - 2)
#' }
#' f <- function(u) c(-u[2], u[1])
#' ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2))
#'
#' # create design points
#' n <- 20
#' df <- matrix(runif(2*n, -2, 2), nrow = n)
#' df <- as.data.frame(df)
#' names(df) <- c("x", "y")
#'
#' # sample f at design points and add to df
#' fdf <- apply(df, 1, f) |> t() |> as.data.frame()
#' names(fdf) <- c("fx", "fy")
#' df <- cbind(df, fdf)
#'
#' # visualize
#' ggplot(df) + geom_vector(aes(x, y, fx = fx, fy = fy))
#'
#' # add smooth layer
#' ggplot(df) +
#'   geom_vector(aes(x, y, fx = fx, fy = fy)) +
#'   geom_stream_smooth(formula = cbind(fx, fy) ~ x * y)
#'
#' ggplot(df) +
#'   geom_vector(aes(x, y, fx = fx, fy = fy)) +
#'   geom_stream_smooth(formula = cbind(fx, fy) ~ poly(x,2) * poly(y,2), data = sample_points)
#'
#' fhat <- function(u) {
#'   model <- lm( cbind(fx, fy) ~ x * y, data = df )
#'   newdata <- data.frame(x = u[1], y = u[2])
#'   preds <- predict(model, newdata = newdata)
#'   as.numeric(preds)
#' }
#'
#' ggplot(df) +
#'   geom_stream_field(fun = fhat, normalize = FALSE, color = "#3366FF") +
#'   geom_vector(aes(x, y, fx = fx, fy = fy))
#'
#' @export
geom_stream_smooth <- function(mapping = NULL, data = NULL,
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
                               arrow = grid::arrow(angle = 20,
                                                   length = unit(0.015, "npc"),
                                                   type = "closed")
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

  n <- ensure_length_two(n)
  dots <- list(...)
  if (!("color" %in% names(dots))) {
    dots$color <- "blue"
  }

  # Define the vector field function. It retrieves the prepared data from the
  # parent environment (which comes from the stat after setup_data()).
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
        type = type,
        arrow = arrow
      ),
      dots
    )
  )
}
