f_rotation <- function(u) c(-u[2], u[1])

stream_data <- function(...) {
  layer_data(
    ggplot() +
      geom_stream_field(fun = f_rotation, xlim = c(-1, 1), ylim = c(-1, 1), n = 3, ...)
  )
}

test_that("method selects the integrator", {
  expect_false(identical(stream_data(method = "rk4"), stream_data(method = "euler")))
  expect_false(identical(stream_data(method = "rk4"), stream_data(method = "lsoda")))

  # lsoda is the default
  expect_identical(stream_data(), stream_data(method = "lsoda"))
})

test_that("an unknown method is an error", {
  expect_error(geom_stream_field(fun = f_rotation, method = "nope"), "should be one of")
  expect_error(stat_stream_field(fun = f_rotation, method = "nope"), "should be one of")
})

test_that("rk4 follows the unit circle", {
  s <- ode_stepper(c(1, 0), f_rotation, T = 1e6, L = 1, method = "rk4", center = FALSE)
  n <- nrow(s)

  expect_equal(sqrt(s$x^2 + s$y^2), rep(1, n), tolerance = 1e-5)
  expect_equal(s$l[n], 1)
  expect_equal(s$t[n], 1, tolerance = 1e-3)
  expect_equal(c(s$x[n], s$y[n]), c(cos(1), sin(1)), tolerance = 1e-4)
})

test_that("fixed-step streams run to T take equal time steps", {
  s <- ode_stepper(c(1, 0), f_rotation, T = 1, L = 1e6, method = "rk4", center = FALSE)

  expect_equal(max(s$t), 1)
  expect_equal(diff(s$t), rep(1 / 25, nrow(s) - 1))
})

test_that("the integrators agree on stream lengths", {
  for (normalize in c(TRUE, FALSE)) {
    rk4 <- stream_data(method = "rk4", normalize = normalize)
    lsoda <- stream_data(method = "lsoda", normalize = normalize)

    expect_equal(
      as.numeric(tapply(rk4$l, rk4$group, max)),
      as.numeric(tapply(lsoda$l, lsoda$group, max)),
      tolerance = 1e-3
    )
  }
})

test_that("streams stop at a sink instead of running past it", {
  # sink at (1, 1); the seed is 0.1118 from it, so no stream can reach L = 1
  f_sink <- function(u) c(1, 1) - u
  u0 <- c(1.1, 1.05)
  dist <- sqrt(sum((u0 - 1)^2))

  for (m in c("lsoda", "rk4", "euler")) {
    s <- ode_stepper(u0, f_sink, T = 1e6, L = 1, method = m, center = FALSE)
    n <- nrow(s)
    expect_lt(sqrt(sum((c(s$x[n], s$y[n]) - 1)^2)), 1e-4)
    expect_lt(s$l[n], 0.2)

    # lsoda's path is made of every point the solver evaluated the field at,
    # some slightly off the trajectory, so only the fixed-step paths are exact
    if (m != "lsoda") {
      expect_equal(s$l[n], dist, tolerance = 1e-6, info = m)
      # the stream never moves away from the sink
      expect_true(all(diff(sqrt((s$x - 1)^2 + (s$y - 1)^2)) <= 1e-12), info = m)
    }
  }
})

test_that("rk4 follows a spiral into its sink", {
  f_spiral <- function(u) c(-u[1] - 3 * u[2], 3 * u[1] - u[2])
  rk4 <- ode_stepper(c(0.15, 0), f_spiral, T = 1e6, L = 1, method = "rk4", center = FALSE)
  lsoda <- ode_stepper(c(0.15, 0), f_spiral, T = 1e6, L = 1, method = "lsoda", center = FALSE)
  n <- nrow(rk4)

  # exact length of this spiral is 0.15 * sqrt(10); the polyline's chords are
  # a little shorter than the arc
  expect_equal(rk4$l[n], 0.15 * sqrt(10), tolerance = 2e-3)
  expect_equal(rk4$l[n], lsoda$l[nrow(lsoda)], tolerance = 1e-3)
  expect_lt(sqrt(rk4$x[n]^2 + rk4$y[n]^2), 1e-4)
})

test_that("a seed on a zero of the field gives a zero-length stream", {
  s <- ode_stepper(c(0, 0), f_rotation, T = 1e6, L = 1, method = "rk4", center = FALSE)

  expect_equal(nrow(s), 2)
  expect_equal(s$l, c(0, 0))
})
