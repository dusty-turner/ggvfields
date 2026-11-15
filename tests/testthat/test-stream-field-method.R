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
  # the same sink at the origin and moved to (1, 1) must give the same
  # stream, and the stream must stop when it reaches the sink rather than run
  # the clock on to T
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
  # for a constant field, streams and vectors are the same picture. every
  # segment, including the last one along which the arrowhead is drawn, must
  # point along the field, with no rounding-sized sliver at either end
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

test_that("a centered stream that can only move one way from its seed is drawn", {
  # the field is undefined for x < 0, so from a seed on x = 0 a stream can
  # only go one way; the layer must still be drawn, with that half stream
  forward_blocked <- function(u) c(-1, sqrt(u[1]))   # forward heads into x < 0
  backward_blocked <- function(u) c(1, sqrt(u[1]))   # backward heads into x < 0

  for (f in list(forward_blocked, backward_blocked)) {
    for (m in c("rk4", "lsoda")) {
      d <- suppressWarnings(layer_data(ggplot() + geom_stream_field(fun = f, method = m)))
      seeds_on_edge <- unique(d$group[d$x0 == 0])
      expect_length(seeds_on_edge, 11)

      s <- suppressWarnings(ode_stepper(c(x = 0, y = 0), f, T = 1e6, L = 0.2, method = m, center = TRUE))
      expect_true(any(s$x == 0 & s$y == 0), info = m)       # the seed is kept
      expect_equal(max(s$l), 0.1, tolerance = 1e-6, info = m) # one half, L / 2
      expect_true(is.finite(s$avg_spd[1]), info = m)
    }
  }
})

test_that("fields that index their argument by name work with every integrator", {
  # lsoda and the vector layers call fun with c(x = , y = ); rk4 and euler must too
  local({
    x <- 99; y <- 99   # a with(as.list(u), ...) field must not pick these up
    positional <- function(u) c(-u[2], u[1])
    by_name <- list(
      function(u) c(-u["y"], u["x"]),
      function(u) c(-u[["y"]], u[["x"]]),
      function(u) with(as.list(u), c(-y, x))
    )
    for (m in c("rk4", "euler", "lsoda")) {
      ref <- layer_data(ggplot() + geom_stream_field(fun = positional, method = m))
      for (f in by_name) {
        d <- layer_data(ggplot() + geom_stream_field(fun = f, method = m))
        expect_equal(d[, c("x", "y")], ref[, c("x", "y")], info = m)
      }
    }
  })
})

test_that("rk4 and euler streams cross jumps in the field", {
  # a particle crosses a line where the field jumps, as long as the field on
  # the far side carries it on; seeds on the line are drawn too
  seed <- c(x = -0.05, y = 0.1)
  for (m in c("rk4", "euler")) {
    # the direction turns 90 degrees at x = 0: down-right, then up-right
    s <- ode_stepper(seed, function(u) c(1, sign(u[1])), T = 1e6, L = 0.17, method = m)
    expect_equal(max(s$l), 0.17, tolerance = 1e-6, info = m)
    expect_equal(c(tail(s$x, 1), tail(s$y, 1)),
                 c(0.17 / sqrt(2) - 0.05, 0.05 + 0.17 / sqrt(2) - 0.05), tolerance = 1e-4, info = m)

    # the speed triples at x = 0
    s <- ode_stepper(seed, function(u) c(1, 0) * ifelse(u[1] > 0, 3, 1), T = 1e6, L = 0.17, method = m)
    expect_equal(c(tail(s$x, 1), tail(s$y, 1)), c(0.12, 0.1), tolerance = 1e-6, info = m)

    d <- layer_data(ggplot() + geom_stream_field(fun = function(u) c(1, sign(u[1])), method = m))
    expect_length(unique(d$group), 121)
  }
})

test_that("streams stop on a line the field converges on", {
  # both sides push towards x = 0, so a particle cannot cross it
  s <- ode_stepper(c(x = -0.05, y = 0), function(u) c(-sign(u[1]), 0.2), T = 1e6, L = 0.17, method = "rk4")
  expect_lt(abs(tail(s$x, 1)), 1e-6)
  expect_lt(max(s$l), 0.06)
})

test_that("geom_stream_field2 crops every stream to the time of the fastest one", {
  # streams are grown to L, then all cropped back to the fastest stream's
  # time, so length shows relative speed. in a rotation field the speed is the
  # radius, so length / radius is the same everywhere
  d <- layer_data(ggplot() + geom_stream_field2(fun = function(u) c(-u[2], u[1])))
  st <- do.call(rbind, lapply(split(d, d$group), function(s) {
    data.frame(r = sqrt(s$x[1]^2 + s$y[1]^2), l = max(s$l), span = max(s$t) - min(s$t))
  }))
  st <- st[st$l > 0, ]
  expect_equal(min(st$span), max(st$span))
  expect_equal(st$l / st$r, rep(st$l[1] / st$r[1], nrow(st)), tolerance = 1e-4)
  expect_lt(min(st$l), max(st$l) / 5)

  # vectors keep the same length
  v <- layer_data(ggplot() + geom_stream_field2(fun = function(u) c(-u[2], u[1]), type = "vector"))
  lens <- tapply(v$l, v$group, max)
  expect_equal(min(lens[lens > 0]), max(lens))
})
