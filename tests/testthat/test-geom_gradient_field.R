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

test_that("geom_gradient_field2 maps the norm to length for vectors only", {
  for (layer_fun in list(geom_gradient_field2, stat_gradient_field2)) {
    l <- layer_fun(fun = f_scalar, type = "vector")
    expect_identical(rlang::as_label(l$mapping$length), "after_stat(norm)")
    expect_null(l$mapping$colour)

    # streams ignore length, so the default stream layer maps nothing
    expect_length(layer_fun(fun = f_scalar)$mapping, 0)
  }

  # a user mapping still wins
  l <- geom_gradient_field2(fun = f_scalar, aes(length = after_stat(norm) * 2), type = "vector")
  expect_identical(rlang::as_label(l$mapping$length), "after_stat(norm) * 2")
})

test_that("geom_gradient_field2 vectors match geom_vector_field2 on the gradient", {
  grad_fun <- function(u) numDeriv::grad(f_scalar, u)
  cols <- c("x", "y", "norm", "length")

  g <- layer_data(ggplot() + geom_gradient_field2(fun = f_scalar, type = "vector"))
  v <- layer_data(ggplot() + geom_vector_field2(fun = grad_fun))

  expect_equal(g[order(g$id, g$x, g$y), cols], v[order(v$id, v$x, v$y), cols],
               ignore_attr = TRUE)
})
