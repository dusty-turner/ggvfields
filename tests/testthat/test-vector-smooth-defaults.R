smooth_data <- local({
  set.seed(1)
  n <- 40
  w <- data.frame(x = runif(n, -1, 1), y = runif(n, -1, 1))
  w$fx <- -w$y + rnorm(n, 0, .1)
  w$fy <- w$x + rnorm(n, 0, .1)
  w
})
smooth_layer <- function(...) {
  layer_data(ggplot(smooth_data, aes(x, y, fx = fx, fy = fy)) + geom_vector_smooth(...))
}

test_that("intervals are ellipses at eval_points and wedges on a grid by default", {
  expect_no_warning(d <- smooth_layer())
  expect_true(all(!is.na(d$min_angle)))

  expect_no_warning(d <- smooth_layer(eval_points = data.frame(x = 0, y = 0.5)))
  expect_true(all(!is.na(d$ellipse_width)))

  # asking for ellipses without eval_points still explains the fallback
  expect_warning(smooth_layer(pi_type = "ellipse"), "eval_points")
})

test_that("xlim and ylim set the range of the grid", {
  d <- smooth_layer(xlim = c(-3, 3), ylim = c(-2, 2), n = 5)
  expect_equal(range(d$x), c(-3, 3))
  expect_equal(range(d$y), c(-2, 2))

  d <- smooth_layer(n = 5)
  expect_equal(range(d$x), range(smooth_data$x))
})
