# a vector in polar form takes its magnitude from `norm` (or `distance`)

set.seed(1)
d <- data.frame(x = runif(40), y = runif(40), a = runif(40, 0, 2 * pi), r = runif(40, .5, 1))
d$fx <- d$r * cos(d$a)
d$fy <- d$r * sin(d$a)

expect_same_layer <- function(p1, p2, cols) {
  a <- expect_no_warning(layer_data(p1))
  b <- expect_no_warning(layer_data(p2))
  expect_equal(a[cols], b[cols])
}

test_that("geom_vector() and geom_vector2() take norm as the polar magnitude", {
  expect_same_layer(
    ggplot(d) + geom_vector(aes(x, y, angle = a, norm = r)),
    ggplot(d) + geom_vector(aes(x, y, fx = fx, fy = fy)),
    c("x", "y", "xend", "yend", "norm")
  )
  expect_same_layer(
    ggplot(d) + geom_vector2(aes(x, y, angle = a, norm = r)),
    ggplot(d) + geom_vector2(aes(x, y, angle = a, distance = r)),
    c("x", "y", "xend", "yend", "length")
  )
  # the computed norm is the supplied magnitude
  out <- layer_data(ggplot(d) + geom_vector(aes(x, y, angle = a, norm = r)))
  expect_equal(unique(out$norm), d$r)
})

test_that("the smoothers take angle with norm or distance, without warnings", {
  expect_same_layer(
    ggplot(d) + geom_vector_smooth(aes(x, y, angle = a, norm = r), method = "lm"),
    ggplot(d) + geom_vector_smooth(aes(x, y, fx = fx, fy = fy), method = "lm"),
    c("x", "y", "fx", "fy")
  )
  cart <- ggplot(d) + geom_stream_smooth(aes(x, y, fx = fx, fy = fy), method = "lm")
  expect_same_layer(
    ggplot(d) + geom_stream_smooth(aes(x, y, angle = a, norm = r), method = "lm"),
    cart, c("x", "y")
  )
  expect_same_layer(
    ggplot(d) + geom_stream_smooth(aes(x, y, angle = a, distance = r), method = "lm"),
    cart, c("x", "y")
  )
  expect_same_layer(
    ggplot(d) + geom_stream_smooth(aes(x, y, angle = a, norm = r), method = "lm",
                                   formula = cbind(norm, angle) ~ x * y),
    cart, c("x", "y")
  )
})
