smooth_data <- function() {
  set.seed(1)
  d <- data.frame(x = runif(30, -1, 1), y = runif(30, -1, 1))
  d$fx <- -d$y + rnorm(30, sd = .1)
  d$fy <- d$x + rnorm(30, sd = .1)
  d$z <- d$x^2 + d$y^2 + rnorm(30, sd = .05)
  d
}

test_that("fit_vector_field returns the fitted lm field", {
  d <- smooth_data()
  formula <- cbind(fx, fy) ~ x * y

  fhat <- fit_vector_field(d, formula, "lm")
  expected <- predict(lm(formula, data = d), newdata = data.frame(x = .2, y = -.3))

  expect_equal(fhat(c(.2, -.3)), as.numeric(expected))
})

test_that("fit_vector_field returns the fitted gam field", {
  d <- smooth_data()

  fhat <- fit_vector_field(d, NULL, "gam")
  new_point <- data.frame(x = .2, y = -.3)
  expected <- c(
    predict(mgcv::gam(fx ~ s(x, y, bs = "tp"), data = d), newdata = new_point),
    predict(mgcv::gam(fy ~ s(x, y, bs = "tp"), data = d), newdata = new_point)
  )

  expect_equal(as.numeric(fhat(c(.2, -.3))), as.numeric(expected))
})

test_that("fit_vector_field rejects unknown methods", {
  expect_error(fit_vector_field(smooth_data(), NULL, "loess"), "must be")
})

test_that("fit_gradient_field returns the gradient of the fitted surface", {
  d <- smooth_data()

  ghat <- fit_gradient_field(d, z ~ x + y + I(x^2) + I(y^2))
  b <- coef(lm(z ~ x + y + I(x^2) + I(y^2), data = d))
  u <- c(.2, -.3)

  expect_equal(
    ghat(u),
    unname(c(b["x"] + 2 * b["I(x^2)"] * u[1], b["y"] + 2 * b["I(y^2)"] * u[2])),
    tolerance = 1e-6
  )
})

test_that("smoothers fit the model once per group", {
  d <- smooth_data()
  fits <- 0
  local_mocked_bindings(
    fit_vector_field = function(data, formula, method) {
      fits <<- fits + 1
      function(u) c(-u[2], u[1])
    }
  )

  layer_data(
    ggplot(d, aes(x, y, fx = fx, fy = fy)) +
      geom_stream_smooth(method = "lm", n = 4, type = "stream")
  )

  expect_equal(fits, 1)
})
