# grid = "even": evenly spaced streamlines

rot <- function(u) c(-u[2], u[1])

min_distance_between_streams <- function(d) {
  ids <- unique(d$group)
  best <- Inf
  for (a in ids) for (b in ids[ids > a]) {
    A <- d[d$group == a, c("x", "y")]
    B <- d[d$group == b, c("x", "y")]
    dist <- sqrt(outer(A$x, B$x, "-")^2 + outer(A$y, B$y, "-")^2)
    best <- min(best, dist)
  }
  best
}

test_that("even streams stay apart and follow the field", {
  d <- expect_no_warning(layer_data(ggplot() +
    geom_stream_field(fun = rot, xlim = c(-1, 1), ylim = c(-1, 1), grid = "even", n = 11)))
  d_sep <- 2 / 10
  expect_gt(length(unique(d$group)), 3)

  # streams never come closer than about half the separation
  expect_gt(min_distance_between_streams(d), 0.45 * d_sep)

  # rotation streams are circles: the radius is nearly constant along each
  r <- sqrt(d$x^2 + d$y^2)
  spread <- tapply(r, d$group, function(z) diff(range(z)))
  expect_lt(max(spread), 0.01)

  # every point is in the plot
  expect_true(all(abs(d$x) <= 1 + 1e-9 & abs(d$y) <= 1 + 1e-9))
})

test_that("even streams fill the plot", {
  d <- layer_data(ggplot() +
    geom_stream_field(fun = function(u) c(1, 0.3 * sin(3 * u[1])), xlim = c(-1, 1),
                      ylim = c(-1, 1), grid = "even", n = 11))
  # every point of a fine grid is within about one separation of a stream
  probe <- expand.grid(x = seq(-0.9, 0.9, by = 0.1), y = seq(-0.9, 0.9, by = 0.1))
  gap <- apply(probe, 1, function(p) min(sqrt((d$x - p[1])^2 + (d$y - p[2])^2)))
  expect_lt(max(gap), 0.2 * 1.1)
})

test_that("streams run with the flow and carry speeds and times", {
  d <- layer_data(ggplot() +
    geom_stream_field(fun = function(u) c(2, 0), xlim = c(-1, 1), ylim = c(-1, 1), grid = "even"))
  for (g in unique(d$group)) {
    s <- d[d$group == g, ]
    expect_true(all(diff(s$x) > 0))
    expect_true(all(diff(s$t) > 0))
  }
  expect_equal(unique(round(d$avg_spd, 6)), 2)
})

test_that("even streams respect the domain and stop at sinks", {
  disk <- function(u) sum(u^2) <= 1
  d <- layer_data(ggplot() +
    geom_stream_field(fun = rot, grid = "even", n = 16, domain = disk))
  expect_true(all(d$x^2 + d$y^2 <= 1 + 1e-6))

  sink <- function(u) -u
  s <- expect_no_warning(layer_data(ggplot() + geom_stream_field(fun = sink, grid = "even")))
  expect_true(all(is.finite(s$x)))
})

test_that("grid = \"even\" works for gradient streams and needs streams", {
  f <- function(u) sum(u^2)
  d <- layer_data(ggplot() + geom_gradient_field(fun = f, type = "stream", grid = "even"))
  expect_gt(nrow(d), 0)

  expect_error(geom_stream_field(fun = rot, grid = "even", type = "vector"), "type = \"stream\"")
  expect_error(geom_gradient_field(fun = f, grid = "even"), "type = \"stream\"")
})
