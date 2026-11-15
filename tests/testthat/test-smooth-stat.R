smooth_points <- local({
  set.seed(1)
  n <- 40
  w <- data.frame(x = runif(n, -1, 1), y = runif(n, -1, 1))
  w$fx <- -w$y
  w$fy <- w$x
  w$z <- w$x^2 + w$y^2
  w
})

test_that("the smoothing geoms always use their own stat", {
  vec <- ggplot(smooth_points, aes(x, y, fx = fx, fy = fy))
  grad <- ggplot(smooth_points, aes(x, y, z = z))

  expect_no_warning(ref <- layer_data(vec + geom_stream_smooth()))
  expect_warning(d <- layer_data(vec + geom_stream_smooth(stat = StatStreamField)), "is ignored")
  expect_equal(d, ref)

  expect_no_warning(ref <- layer_data(grad + geom_gradient_smooth()))
  expect_warning(d <- layer_data(grad + geom_gradient_smooth(stat = "stream_field")), "is ignored")
  expect_equal(d, ref)

  expect_no_warning(layer_data(vec + geom_vector_smooth()))
  expect_warning(geom_vector_smooth(stat = StatStreamField), "is ignored")
})
