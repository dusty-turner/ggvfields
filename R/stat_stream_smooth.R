# StatStreamSmooth and StatGradientSmooth fit a field to the data once per
# group and hand it to StatStreamField. they share this compute_group(), which
# lists every parameter so ggplot2 passes them through, and differ only in
# fit_field(), which returns the fitted field u -> c(fx, fy)
StatFittedField <- ggproto(
  "StatFittedField",
  StatStreamField,

  fit_field = function(self, data, formula, method) {
    cli::cli_abort("{.fn fit_field} must be defined by a subclass.")
  },

  compute_group = function(self, data, scales, formula, method = "gam",
                           xlim, ylim, n, max_it = 1000,
                           tol = sqrt(.Machine$double.eps), T = NULL, L = NULL,
                           center, type, normalize, grid, ...) {

    fun <- self$fit_field(data, formula, method)

    ggproto_parent(StatStreamField, self)$compute_group(
      data, scales, fun = fun, xlim = xlim, ylim = ylim, n = n,
      max_it = max_it, tol = tol, T = T, L = L,
      center = center, type = type, normalize = normalize, grid = grid
    )
  }
)

#' @rdname geom_stream_smooth
#' @format NULL
#' @usage NULL
#' @export
StatStreamSmooth <- ggproto(
  "StatStreamSmooth",
  StatFittedField,

  fit_field = function(self, data, formula, method) {
    fit_vector_field(data, formula, method)
  }
)

#' @rdname geom_gradient_smooth
#' @format NULL
#' @usage NULL
#' @export
StatGradientSmooth <- ggproto(
  "StatGradientSmooth",
  StatFittedField,

  fit_field = function(self, data, formula, method) {
    fit_gradient_field(data, formula)
  }
)

# returns the function u -> c(fx, fy) of a vector field fitted to data, which
# has columns x, y, fx and fy
#' @keywords internal
fit_vector_field <- function(data, formula, method) {

  if (method == "gam") {

    k <- gam_basis_size(data)
    fx_mod <- mgcv::gam(fx ~ s(x, y, bs = "tp", k = k), data = data)
    fy_mod <- mgcv::gam(fy ~ s(x, y, bs = "tp", k = k), data = data)

    return(function(u) {
      new_point <- data.frame(x = u[1], y = u[2])
      c(predict(fx_mod, newdata = new_point), predict(fy_mod, newdata = new_point))
    })

  }

  if (method == "lm") {

    model <- lm(formula, data = data)

    return(function(u) {
      as.numeric(predict(model, newdata = data.frame(x = u[1], y = u[2])))
    })

  }

  cli::cli_abort("{.arg method} must be {.val gam} or {.val lm}, not {.val {method}}.")

}

# returns the function u -> gradient of a scalar field fitted to data, which
# has columns x, y and z
#' @keywords internal
fit_gradient_field <- function(data, formula) {

  model <- lm(formula, data = data)

  scalar_field <- function(u) {
    as.numeric(predict(model, newdata = data.frame(x = u[1], y = u[2])))
  }

  function(u) numDeriv::grad(func = scalar_field, x = u)

}
