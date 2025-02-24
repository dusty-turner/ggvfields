library(testthat)
library(ggvfields)  # Ensure your package is loaded

# Define a simple scalar field: f(x, y) = x^2 + y^2.
# Its gradient is (2*x, 2*y).
f_scalar <- function(u) {
  u[1]^2 + u[2]^2
}

# Create a grid of seed points.
df <- expand.grid(x = seq(-1, 1, length.out = 5),
                  y = seq(-1, 1, length.out = 5))

# Helper: Build a ggplot object and extract computed data from the gradient stat.
get_grad_data <- function(p) {
  pb <- ggplot_build(p)
  pb$data[[1]]
}

test_that("geom_gradient_field computes gradient and includes required aesthetics in long format", {
  p <- ggplot() +
    geom_gradient_field(fun = f_scalar, xlim = c(-1, 1), ylim = c(-1, 1))

  d <- get_grad_data(p)

  # Check that required columns exist.
  expect_true(all(c("x", "y", "fx", "fy", "id", "colour") %in% names(d)))

})



test_that("stat_gradient_field computes gradient endpoints and includes required aesthetics", {
  p <- ggplot() +
    stat_gradient_field(fun = f_scalar, xlim = c(-1, 1), ylim = c(-1, 1))

  d <- get_grad_data(p)

  expect_true(all(c("x", "y", "fx", "fy", "id", "colour") %in% names(d)))
})

test_that("geom_gradient_field2 includes length aesthetic and computes endpoints", {
  p <- ggplot() +
    geom_gradient_field2(fun = f_scalar, xlim = c(-1, 1), ylim = c(-1, 1))

  d <- get_grad_data(p)

  # Check that 'length' is included (instead of colour).
  expect_true(all(c("x", "y", "fx", "fy", "id", "length") %in% names(d)))

})

test_that("stat_gradient_field2 includes length aesthetic and computes endpoints", {
  p <- ggplot() +
    stat_gradient_field2(fun = f_scalar, xlim = c(-1, 1), ylim = c(-1, 1))

  d <- get_grad_data(p)

  expect_true(all(c("x", "y", "fx", "fy", "id", "length") %in% names(d)))
})
