test_that("mapped lengths are proportional to the mapped value", {
  d <- data.frame(x = 1:3, y = 0, fx = c(1, 2, 4), fy = 0)
  ld <- layer_data(ggplot(d, aes(x, y, fx = fx, fy = fy)) + geom_vector2())
  lens <- as.numeric(tapply(ld$length, ld$group, unique))
  expect_equal(lens, c(0.125, 0.25, 0.5))

  # the longest vector is max_range centimeters long
  ld <- layer_data(ggplot(d, aes(x, y, fx = fx, fy = fy)) + geom_vector2() +
                     scale_length_continuous(max_range = 1.5))
  expect_equal(max(ld$length), 1.5)

  # in a field, length / norm is the same for every vector
  v <- layer_data(ggplot() + geom_vector_field2(fun = function(u) c(-u[2], u[1]), n = 5))
  v <- v[v$norm > 0, ]
  expect_equal(v$length / v$norm, rep(v$length[1] / v$norm[1], nrow(v)))
})

test_that("a log length scale keeps every vector's direction", {
  f <- efield_maker()
  p <- ggplot() +
    geom_vector_field2(fun = f, xlim = c(-2, 2), ylim = c(-2, 2)) +
    scale_length_continuous(transform = "log10")
  d <- expect_no_warning(layer_data(p))

  expect_true(all(d$length > 0))
  expect_equal(max(d$length), 0.5)
  expect_equal(min(d$length), 0.05)
  # lengths grow with the norm
  o <- order(d$norm)
  expect_true(all(diff(d$length[o]) >= -1e-12))
})

test_that("the length rescaler maps negative transformed values to positive lengths", {
  rescale <- scale_length_continuous()$rescaler
  expect_equal(rescale(c(-1, 0, 1)), c(0.1, 0.55, 1))
  expect_equal(rescale(c(-2, -2)), c(1, 1))
  # non-negative values stay proportional
  expect_equal(rescale(c(0, 1, 2)), c(0, 0.5, 1))
})
