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

  # rk4 is the default, also for layers that do not expose method
  expect_identical(stream_data(), stream_data(method = "rk4"))
  expect_identical(formals(get("compute_group", envir = StatStreamField))$method, "rk4")
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

test_that("lsoda stops at a sink wherever it is", {
  # the same sink at the origin and moved to (1, 1) must give the same stream;
  # lsoda used to stop only at sinks at the origin and otherwise ran the clock
  # to T = 1e6, which made avg_spd nearly 0
  at_origin <- ode_stepper(c(0.1, 0.05), function(u) -u,
                           T = 1e6, L = 1, method = "lsoda", center = FALSE)
  offset <- ode_stepper(c(1.1, 1.05), function(u) c(1, 1) - u,
                        T = 1e6, L = 1, method = "lsoda", center = FALSE)

  expect_lt(max(offset$t), 100)
  expect_equal(offset$avg_spd[1], at_origin$avg_spd[1], tolerance = 1e-4)
  expect_equal(max(offset$l), max(at_origin$l), tolerance = 1e-6)
})

test_that("rk4 and euler streams stop at a point charge instead of jumping over it", {
  # efield_maker() has a charge at (-1, -1); this seed flows straight into it
  f <- efield_maker()
  for (m in c("rk4", "euler")) {
    for (bound in c("length", "time")) {
      s <- if (bound == "length") {
        ode_stepper(c(-1.2, -1.2), f, T = 1e6, L = 1, method = m, center = FALSE)
      } else {
        ode_stepper(c(-1.2, -1.2), f, T = 0.5, L = 1e6, method = m, center = FALSE)
      }
      n <- nrow(s)
      info <- paste(m, bound)
      expect_lt(sqrt(sum((c(s$x[n], s$y[n]) + 1)^2)), 1e-3)
      expect_equal(s$l[n], sqrt(2 * 0.2^2), tolerance = 1e-3, info = info)
      expect_lt(n, 200)
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

test_that("avg_spd of streams cropped to the fastest is their drawn length over time", {
  # normalize = FALSE without T grows streams to L, then crops them all back to
  # the time of the fastest one; avg_spd must describe the cropped stream
  f <- function(u) c(1, 1) - u
  for (center in c(FALSE, TRUE)) {
    d <- layer_data(ggplot() + geom_stream_field(fun = f, xlim = c(0, 2), ylim = c(0, 2), n = 5,
                                                 normalize = FALSE, center = center))
    # every stream gets the same time: the time of the fastest one
    spans <- tapply(d$t, d$group, function(t) max(t) - min(t))
    for (s in split(d, d$group)) {
      s <- s[order(s$t), ]
      n <- nrow(s)
      if (n < 2) next
      expect_equal(s$avg_spd[1], s$l[n] / max(spans), info = paste("center =", center))
    }
  }
})

test_that("avg_spd of a stream that ends at a sink does not depend on the integrator", {
  # the stream reaches the sink at (1, 1) after length 0.1118, short of L = 1.
  # its speed is measured up to 99% of that length: for u' = (1, 1) - u the
  # distance to the sink is r0 * exp(-t), so 99% takes t = log(100)
  f_sink <- function(u) c(1, 1) - u
  u0 <- c(1.1, 1.05)
  r0 <- sqrt(sum((u0 - 1)^2))
  expected <- 0.99 * r0 / log(100)

  spd <- vapply(c("lsoda", "rk4"), function(m) {
    ode_stepper(u0, f_sink, T = 1e6, L = 1, method = m, center = FALSE)$avg_spd[1]
  }, numeric(1))

  # the integrators agree; the 99% time is read off the drawn polyline, so it is
  # close to, not exactly, the analytic value
  expect_equal(spd[["rk4"]], spd[["lsoda"]], tolerance = 1e-3)
  expect_equal(spd[["rk4"]], expected, tolerance = 1e-2)
})

test_that("avg_spd of a stream run to a time T is its length over T", {
  # this stream reaches the sink long before T = 50 and stops there
  f_sink <- function(u) c(1, 1) - u
  for (m in c("lsoda", "rk4")) {
    s <- ode_stepper(c(1.1, 1.05), f_sink, T = 50, L = 1e6, method = m, center = FALSE)
    expect_equal(s$avg_spd[1], max(s$l) / 50, info = m)
  }
})

test_that("streams of a constant field match its vectors (no sliver end segments)", {
  # the paper's Fig. 2.15: for a constant field, streams and vectors are the
  # same picture. rounding could leave a ~1e-16 final segment that turned the
  # arrowhead in an arbitrary direction
  for (f in list(c(3, 4), c(2, 1))) {
    for (center in c(TRUE, FALSE)) {
      d <- layer_data(ggplot() + geom_stream_field(fun = function(u) f, center = center))
      for (s in split(d, d$group)) {
        s <- s[order(s$t), ]
        seg <- sqrt(diff(s$x)^2 + diff(s$y)^2)
        expect_gt(min(seg), 1e-6)
        # every segment points along the field
        dir <- cbind(diff(s$x), diff(s$y)) / seg
        expect_equal(unname(dir[nrow(dir), ]), f / sqrt(sum(f^2)), tolerance = 1e-8)
      }
    }
  }
})
