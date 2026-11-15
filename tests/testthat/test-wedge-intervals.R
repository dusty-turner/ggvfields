# the angle of a bivariate normal tip, by simulation
mc_offsets <- function(mu, Sigma, level) {
  set.seed(42)
  z <- matrix(rnorm(4e5), ncol = 2) %*% chol(Sigma) + rep(mu, each = 2e5)
  th0 <- atan2(mu[2], mu[1])
  d <- (atan2(z[, 2], z[, 1]) - th0 + pi) %% (2 * pi) - pi
  quantile(d, c((1 - level) / 2, 1 - (1 - level) / 2), names = FALSE)
}
pkg_offsets <- function(mu, Sigma, level) {
  r <- predict_theta_interval(0, 0, mu[1], mu[2], Sigma, conf_level = level)
  th0 <- atan2(mu[2], mu[1])
  (c(r$min_angle, r$max_angle) - th0 + pi) %% (2 * pi) - pi
}

test_that("the angle density is a density", {
  Sigma <- matrix(c(.3, .2, .2, .3), 2)
  expect_equal(integrate(function(t) angle_density(t, c(1, .5), Sigma), -pi, pi)$value, 1,
               tolerance = 1e-6)
})

test_that("wedges match the simulated angle distribution", {
  cases <- list(
    list(c(1, 0), diag(2) * 0.04, 0.95),
    list(c(1, 0), diag(2) * 0.64, 0.95),                  # large spread
    list(c(-1, 0.5), matrix(c(.3, .2, .2, .3), 2), 0.95),  # asymmetric
    list(c(0, -2), matrix(c(.5, .1, .1, .2), 2), 0.70)
  )
  for (cs in cases) {
    expect_equal(pkg_offsets(cs[[1]], cs[[2]], cs[[3]]), mc_offsets(cs[[1]], cs[[2]], cs[[3]]),
                 tolerance = 0.01)
  }
})

test_that("wedges are right for very small spread and degenerate inputs", {
  # sd 1e-4: the 95% interval is +- 1.96e-4 radians
  expect_equal(pkg_offsets(c(1, 0), diag(2) * 1e-8, 0.95), c(-1, 1) * qnorm(0.975) * 1e-4,
               tolerance = 1e-3)
  expect_equal(pkg_offsets(c(1, 0), matrix(0, 2, 2), 0.95), c(0, 0))
  expect_true(all(is.na(predict_theta_interval(0, 0, 0, 0, diag(2), conf_level = 0.95))))
})
