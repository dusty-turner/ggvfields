test_that("the default length follows the spacing of a supplied grid", {
  f <- function(u) c(-u[2], u[1])
  longest <- function(...) {
    d <- layer_data(ggplot() + geom_vector_field(fun = f, ...))
    max(tapply(d$l, d$group, max))
  }

  # a regular grid supplied as a data frame gives the built-in grid's length
  reg <- expand.grid(x = seq(-1, 1, length.out = 11), y = seq(-1, 1, length.out = 11))
  expect_equal(longest(grid = reg), longest())

  # dense and sparse grids get 0.85 times their spacing
  for (d in c(0.05, 0.5)) {
    g <- grid_hex(c(-1, 1), c(-1, 1), d)
    expect_equal(longest(grid = g), 0.85 * grid_spacing(g), tolerance = 1e-8)
  }
  expect_equal(grid_spacing(grid_hex(c(-1, 1), c(-1, 1), 0.05)), 0.05, tolerance = 1e-6)

  # an explicit L is used as given
  expect_equal(longest(grid = grid_hex(c(-1, 1), c(-1, 1), 0.05), L = 0.3), 0.3)
})
