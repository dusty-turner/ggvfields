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
#' generate_vectors <- function(v) {
#'   x <- v[1]
#'   y <- v[2]
#'   c(sin(x) + sin(y) + rnorm(1, 5, 1),
#'     sin(x) - sin(y) - rnorm(1, 5, 1))
#' }
#' # Set seed for reproducibility
#' set.seed(123)
#' # Create sample points and compute vectors
#' sample_points <- data.frame(
#'   x = runif(30, 0, 10),
#'   y = runif(30, 0, 10)
#' )
#' result <- t(apply(sample_points, 1, generate_vectors))
#' sample_points$xend <- result[, 1]
#' sample_points$yend <- result[, 2]
#' sample_points$fx <- sample_points$xend - sample_points$x
#' sample_points$fy <- sample_points$yend - sample_points$y
#' sample_points$distance <- sqrt(sample_points$fx^2 + sample_points$fy^2)
#' sample_points$angle <- atan2(sample_points$fy, sample_points$fx)
#' # Define evaluation points
#' eval_points <- data.frame(
#'   x = c(0, 7.5),
#'   y = c(10, 5)
#' )
#'
#' p <- ggplot() +
#'   geom_vector(data = sample_points,
#'               aes(x = x, y = y, fx = fx, fy = fy),
#'               color = "black")
#'
#' p + geom_stream_smooth(formula = cbind(fx, fy) ~ x * y, data = sample_points)
#' p + geom_stream_smooth(formula = cbind(fx, fy) ~ poly(x,2) * poly(y,2), data = sample_points)
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

  if (is.null(data)) {
    stop("Error in geom_stream_smooth: 'data' is required.")
  }
  if (missing(formula) || is.null(formula)) {
    stop("Error in geom_stream_smooth: 'formula' is required.")
  }

  # If xend/yend are missing or all NA, then try to compute them
  if ((!"xend" %in% names(data) || all(is.na(data$xend))) ||
      (!"yend" %in% names(data) || all(is.na(data$yend)))) {
    if ("fx" %in% names(data) && "fy" %in% names(data) &&
        !(all(is.na(data$fx)) || all(is.na(data$fy)))) {
      # Use fx and fy to compute endpoints.
      data$xend <- data$x + data$fx
      data$yend <- data$y + data$fy
    } else if ("angle" %in% names(data) && "distance" %in% names(data) &&
               !(all(is.na(data$angle)) || all(is.na(data$distance)))) {
      # Use angle and distance to compute endpoints.
      data$xend <- data$x + data$distance * cos(data$angle * 180 / pi)
      data$yend <- data$y + data$distance * sin(data$angle * 180 / pi)
    } else {
      stop("Either xend/yend or fx/fy or angle/distance must be provided.")
    }
  }

  if ((!"fx" %in% names(data) || all(is.na(data$fx))) ||
      (!"fy" %in% names(data) || all(is.na(data$fy)))) {
    data$fx <- data$xend - data$x
    data$fy <- data$yend - data$y
  }

  if (is.null(xlim)) {
    xlim <- range(data$x, na.rm = TRUE)
  }
  if (is.null(ylim)) {
    ylim <- range(data$y, na.rm = TRUE)
  }

  n <- ensure_length_two(n)

  dots <- list(...)
  if (!("color" %in% names(dots))) {
    dots$color <- "blue"
  }

  vec_field <- function(u, formula, data) {
    model <- lm(formula, data = data)
    newdata <- data.frame(x = u[1], y = u[2])
    preds <- predict(model, newdata = newdata)
    as.numeric(preds)
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
        args = list(formula = formula, data = data),
        arrow = arrow
      ),
      dots
    )
  )
}

