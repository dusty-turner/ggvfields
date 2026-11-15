# domain restricts a field layer to a region

rot <- function(u) c(-u[2], u[1])
disk <- function(u) sum(u^2) <= 1

test_that("seeds outside the domain are dropped", {
  d <- layer_data(ggplot() + geom_vector_field(fun = rot, xlim = c(-1, 1), ylim = c(-1, 1),
                                               center = FALSE, domain = disk))
  seeds <- unique(d[c("x0", "y0")])
  expect_true(all(seeds$x0^2 + seeds$y0^2 <= 1 + 1e-12))
  full <- layer_data(ggplot() + geom_vector_field(fun = rot, xlim = c(-1, 1), ylim = c(-1, 1),
                                                  center = FALSE))
  expect_lt(nrow(d), nrow(full))
  # the domain does not change the default length, which comes from the grid
  expect_equal(max(d$l), max(full$l))
})

test_that("streams stop at the boundary of the domain", {
  half <- function(u) u[1] >= 0
  field <- function(u) c(-1, 0.2)   # flows toward the boundary x = 0
  for (m in c("rk4", "euler")) {
    d <- expect_no_warning(layer_data(ggplot() +
      geom_stream_field(fun = field, xlim = c(0, 1), ylim = c(0, 1), domain = half, method = m)))
    expect_gte(min(d$x), -1e-9)
  }
})

test_that("a field undefined outside the domain draws without warnings", {
  sqrt_field <- function(u) c(1, sqrt(u[1]))
  d <- expect_no_warning(layer_data(ggplot() +
    geom_stream_field(fun = sqrt_field, xlim = c(-1, 1), ylim = c(-1, 1),
                      domain = function(u) u[1] >= 0)))
  expect_true(all(d$x >= 0))
  expect_true(all(is.finite(d$avg_spd)))
})

test_that("domain works for gradient fields and the second variants", {
  f <- function(u) sum(u^2)
  for (layer in list(
    geom_gradient_field(fun = f, xlim = c(-1, 1), ylim = c(-1, 1), domain = disk),
    geom_gradient_field2(fun = f, xlim = c(-1, 1), ylim = c(-1, 1), domain = disk),
    geom_stream_field2(fun = rot, xlim = c(-1, 1), ylim = c(-1, 1), domain = disk),
    geom_vector_field2(fun = rot, xlim = c(-1, 1), ylim = c(-1, 1), domain = disk)
  )) {
    d <- layer_data(ggplot() + layer)
    expect_true(all(d$x0^2 + d$y0^2 <= 1 + 1e-12))
  }
})

test_that("a domain with no seeds draws nothing, and domain must be a function", {
  d <- expect_no_warning(layer_data(ggplot() + geom_stream_field(fun = rot, domain = function(u) FALSE)))
  expect_equal(nrow(d), 0)
  expect_error(geom_stream_field(fun = rot, domain = 3), "must be a function")
  expect_error(stat_vector_field(fun = rot, domain = "disk"), "must be a function")
})
