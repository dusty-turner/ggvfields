#' Geom Vector Field
#'
#' A wrapper around `geom_stream_field` with default parameters for creating vector fields.
#'
#' @param fun A function that defines the vector field. Must return a list with `dx` and `dy`.
#' @param dt Time step for integration. Default is `1`.
#' @param L Step size or relevant parameter for stream field calculation. Default is `0.1`.
#' @param method Numerical integration method. Default is `"euler"`.
#' @param mapping Aesthetic mappings. Default sets `color` to `NULL`.
#' @param n Number of streamlines or density parameter. Default is `3`.
#' @param center Logical indicating whether to center the stream field. Default is `TRUE`.
#' @param ... Additional arguments passed to `geom_stream_field`.
#'
#' @return A `ggplot2` layer.
#' @export
geom_vector_field <- function(fun,
                              dt = 1,
                              L = 0.1,
                              method = "euler",
                              mapping = aes(),
                              n = 11,
                              center = TRUE,
                              ...) {
  # validate_fun(fun)

  if (is.null(mapping$colour) && is.null(mapping$color)) {
    mapping <- modifyList(mapping, aes(color = NULL))
  }

  geom_stream_field(
    fun = fun,
    dt = dt,
    L = L,
    method = method,
    mapping = mapping,
    n = n,
    center = center,
    ...
  )
}
