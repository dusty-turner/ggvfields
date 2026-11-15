wind <- function(n, seed = 1234) {
  set.seed(seed)
  d <- data.frame(x = rnorm(n), y = rnorm(n), dir = runif(n, -pi/2, pi/2), spd = rchisq(n, df = 2))
  d$fx <- d$spd * cos(d$dir)
  d$fy <- d$spd * sin(d$dir)
  d
}

test_that("the gam smoothers fit data with fewer than 30 locations", {
  d <- wind(10)

  expect_no_warning(
    s <- layer_data(ggplot(d, aes(x, y, fx = fx, fy = fy)) + geom_stream_smooth())
  )
  expect_gt(nrow(s), 0)

  expect_no_warning(
    v <- layer_data(ggplot(d, aes(x, y, fx = fx, fy = fy)) + geom_vector_smooth(pi_type = "wedge"))
  )
  expect_gt(nrow(v), 0)
  expect_true(all(is.finite(v$fx) & is.finite(v$fy)))
})

test_that("the basis size leaves residual degrees of freedom on small data", {
  for (n in c(5, 10, 29)) {
    d <- wind(n)
    k <- gam_basis_size(d)
    m <- mgcv::gam(fx ~ s(x, y, bs = "tp", k = k), data = d)
    expect_gt(m$df.residual, 1)
  }
})

test_that("the gam smoothers are unchanged with 30 or more locations", {
  d <- wind(40)
  expect_identical(gam_basis_size(d), 30)

  fit <- fit_vector_field(d, NULL, "gam")
  default <- mgcv::gam(fx ~ s(x, y, bs = "tp"), data = d)
  expect_equal(as.numeric(fit(c(0.1, 0.2))[1]),
               as.numeric(predict(default, newdata = data.frame(x = 0.1, y = 0.2))))
})

test_that("too few locations for a gam give a clear error", {
  expect_error(gam_basis_size(wind(4)), "at least 5 distinct locations")
})
