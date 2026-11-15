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
