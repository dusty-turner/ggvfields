#' @rdname geom_stream_smooth
#' @format NULL
#' @usage NULL
#' @export
StatStreamSmooth <- ggproto(
  "StatStreamSmooth",
  StatStreamField,

  compute_group = function(self, data, scales, formula, method = "gam",
                           xlim, ylim, n, max_it = 1000,
                           tol = sqrt(.Machine$double.eps), T = NULL, L = NULL,
                           center, type, normalize, grid, ...) {

    # fit once per group, then hand the fitted field to StatStreamField
    fun <- fit_vector_field(data, formula, method)

    ggproto_parent(StatStreamField, self)$compute_group(
      data, scales, fun = fun, xlim = xlim, ylim = ylim, n = n,
      max_it = max_it, tol = tol, T = T, L = L,
      center = center, type = type, normalize = normalize, grid = grid
    )
  }
)

#' @rdname geom_gradient_smooth
#' @format NULL
#' @usage NULL
#' @export
StatGradientSmooth <- ggproto(
  "StatGradientSmooth",
  StatStreamField,

  compute_group = function(self, data, scales, formula,
                           xlim, ylim, n, max_it = 1000,
                           tol = sqrt(.Machine$double.eps), T = NULL, L = NULL,
                           center, type, normalize, grid, ...) {

    # fit once per group, then hand the fitted gradient to StatStreamField
    fun <- fit_gradient_field(data, formula)

    ggproto_parent(StatStreamField, self)$compute_group(
      data, scales, fun = fun, xlim = xlim, ylim = ylim, n = n,
      max_it = max_it, tol = tol, T = T, L = L,
      center = center, type = type, normalize = normalize, grid = grid
    )
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
