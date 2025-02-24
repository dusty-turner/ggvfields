library(testthat)
library(ggvfields)

# Define a conservative vector field.
# For a potential V(x,y)= x^2+y^2, the gradient is (2*x, 2*y).
f_conservative <- function(v) {
  c(2 * v[1], 2 * v[2])
}

# Helper: Build a ggplot object and extract computed data from the potential stat.
get_potential_data <- function(p) {
  pb <- ggplot_build(p)
  pb$data[[1]]
}

test_that("geom_potential computes potential and includes required aesthetics", {
  p <- ggplot() +
    geom_potential(fun = f_conservative, xlim = c(-1, 1), ylim = c(-1, 1), n = 21)

  d <- get_potential_data(p)

  # Check that required columns exist.
  expect_true(all(c("x", "y", "Potential", "curl") %in% names(d)))

  # For the conservative field, potential is defined up to an additive constant.
  # Here, the integration starts at (x0, y0) = (-1, -1) so that
  # the computed potential at (0, 0) should be:
  # V(0,0) - V(-1,-1) = (0^2+0^2) - ((-1)^2+(-1)^2) = 0 - 2 = -2.
  # Find the grid point nearest (0,0).
  idx <- which.min(abs(d$x - 0) + abs(d$y - 0))
  potential_at_0 <- d$Potential[idx]

  expect_equal(potential_at_0, -2, tolerance = 1e-2)
})

test_that("stat_potential computes potential and includes required aesthetics", {
  p <- ggplot() +
    stat_potential(fun = f_conservative, xlim = c(-1, 1), ylim = c(-1, 1), n = 21)

  d <- get_potential_data(p)
  expect_true(all(c("x", "y", "Potential", "curl") %in% names(d)))
})

test_that("geom_potential errors when fun is missing", {
  expect_error(
    ggplot() + geom_potential(),
    regexp = "Parameter `fun` must be provided"
  )
})

# Define a non-conservative vector field function.
# For example, f(v) = (y, 0) is not conservative.
f_nonconservative <- function(v) {
  c(v[2], 0)
}

test_that("geom_potential warns when vector field is non-conservative", {
  p <- ggplot() +
    geom_potential(fun = f_nonconservative, xlim = c(-1, 1), ylim = c(-1, 1), n = 21)

  expect_warning(
    ggplot_build(p),
    regexp = "does not have a potential function"
  )
})
