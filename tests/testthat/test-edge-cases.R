# edge cases and argument checks of the stream, smoothing and helper functions

test_that("streams from a seed where the field is undefined are empty", {
  undefined <- function(u) c(NaN, NaN)
  for (m in c("rk4", "euler")) {
    s <- ode_stepper(c(x = 0, y = 0), undefined, T = 1e6, L = 1, method = m)
    expect_equal(nrow(s), 0, info = m)
  }
  s <- suppressWarnings(ode_stepper(c(x = 0, y = 0), undefined, T = 1e6, L = 1, method = "lsoda"))
  expect_lte(nrow(s), 1)
  expect_true(all(s$x == 0 & s$y == 0))
})

test_that("rk4 and euler streams stop at the edge of the field's domain", {
  # defined only for x >= 0; the stream heads for x < 0
  f <- function(u) if (isTRUE(u[1] >= 0)) c(-1, 0.3) else c(NaN, NaN)
  for (m in c("rk4", "euler")) {
    s <- ode_stepper(c(x = 0.5, y = 0), f, T = 1e6, L = 2, method = m)
    expect_gt(nrow(s), 1)
    expect_true(all(s$x >= 0), info = m)
    expect_true(all(is.finite(s$l)), info = m)
    expect_lt(max(s$l), 2)                        # it stops short of L
    expect_lt(min(s$x), 0.05)                     # close to the edge
  }
})

test_that("streams stop at a jump to a field that is infinite", {
  # an infinite field is not followed, and fun is never called at a point
  # that is not finite
  f <- function(u) {
    stopifnot(all(is.finite(u)))
    if (u[1] < 0) c(1, 0) else c(Inf, 0)
  }
  s <- ode_stepper(c(x = -0.5, y = 0), f, T = 1e6, L = 2, method = "rk4")
  expect_gt(nrow(s), 1)
  expect_true(all(is.finite(s$x)))
  expect_lte(max(s$x), 1e-3)
})

test_that("the length scale maps values with no positive maximum to 0", {
  rescale <- scale_length_continuous()$rescaler
  expect_equal(rescale(c(0, 0, 0)), c(0, 0, 0))
  expect_equal(rescale(c(NA_real_, NA_real_)), c(0, 0))
  expect_equal(rescale(c(-2, -1)), c(0, 0))
  expect_equal(rescale(c(1, 2, 4)), c(0.25, 0.5, 1))
})

test_that("a tiny predicted vector gives a wedge that is nearly a full circle", {
  q <- angle_quantiles(mu = c(1e-200, 0), Sigma = diag(2), theta0 = 0, alpha = 0.05)
  expect_equal(q, c(-0.95 * pi, 0.95 * pi), tolerance = 1e-3)
})

test_that("predict_theta_interval() checks its arguments", {
  S <- diag(2)
  expect_error(predict_theta_interval("a", 0, 1, 0, S, conf_level = .9), "'x'")
  expect_error(predict_theta_interval(0, 1:2, 1, 0, S, conf_level = .9), "'y'")
  expect_error(predict_theta_interval(0, 0, "a", 0, S, conf_level = .9), "'mux'")
  expect_error(predict_theta_interval(0, 0, 1, NULL, S, conf_level = .9), "'muy'")
  expect_error(predict_theta_interval(0, 0, 1, 0, diag(3), conf_level = .9), "'Sigma'")
  expect_error(predict_theta_interval(0, 0, 1, 0, S, conf_level = 1.5), "'conf_level'")
})

test_that("compute_prediction_endpoints() checks its arguments", {
  ok <- list(x = 0, y = 0, fx = 1, fy = 0, angle_lower = -0.1, angle_upper = 0.1)
  for (arg in names(ok)) {
    bad <- ok
    bad[[arg]] <- c(1, 2)
    expect_error(do.call(compute_prediction_endpoints, bad), arg, fixed = TRUE)
  }
})

test_that("geom_vector_smooth() needs fx and fy, or angle and norm", {
  d <- data.frame(x = runif(20), y = runif(20))
  expect_error(
    layer_data(ggplot(d, aes(x, y)) + geom_vector_smooth(method = "lm")),
    "'angle' and 'norm'"
  )
})

test_that("the kriging range falls back to its start when a fit fails", {
  set.seed(4)
  d <- data.frame(x = runif(40, 0, 3), y = runif(40, 0, 4))
  d$fx <- sin(d$x) + rnorm(40, 0, .1)
  d$fy <- cos(d$y) + rnorm(40, 0, .1)
  sp::coordinates(d) <- ~ x + y
  g <- gstat::gstat(NULL, id = "fx", formula = fx ~ 1, data = d)
  g <- gstat::gstat(g, id = "fy", formula = fy ~ 1, data = d)
  v <- gstat::variogram(g)
  xy <- sp::coordinates(d)
  start <- sqrt(sum(apply(xy, 2, function(z) diff(range(z)))^2)) / 3

  # with no variogram for fx, its fit fails and uses the start
  r_fy_only <- kriging_range(d, v[v$id == "fy", ])
  r_both <- kriging_range(d, v)
  expect_true(is.finite(r_fy_only) && r_fy_only > 0)
  expect_equal(kriging_range(d, v[0, ]), start)
  expect_false(isTRUE(all.equal(r_fy_only, r_both)))
})

test_that("helpers check their arguments and handle degenerate grids", {
  expect_error(ensure_length_two(1:3), "Length of 'n' must be 2")
  expect_equal(ensure_length_two(5), c(5, 5))

  expect_null(grid_spacing(data.frame(x = 1, y = 1)))
  expect_null(grid_spacing(data.frame(x = c(1, 1), y = c(2, 2))))
  expect_null(grid_spacing(data.frame(x = c(0, NA), y = c(0, 1))))
  expect_equal(grid_spacing(data.frame(x = c(0, 1, 3), y = 0)), 1)

  expect_error(matrix_to_df_with_names(1:3), "must be a matrix")
  expect_error(matrix_to_df_with_names(matrix(1:4, 2), c("a", "b", "c")), "same length")
})

test_that("gradient field layers check fun and the point they are given", {
  expect_error(geom_gradient_field(), "valid scalar function")
  expect_error(geom_gradient_field(fun = 3), "valid scalar function")

  l <- geom_gradient_field(fun = function(u) sum(u^2))
  grad <- l$stat_params$fun
  expect_equal(grad(c(1, 2)), c(2, 4), tolerance = 1e-6)
  expect_error(grad(1:3), "numeric vector of length 2")
})
