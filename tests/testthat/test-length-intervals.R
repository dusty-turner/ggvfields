# pi_type = "length": prediction intervals for the length of each vector

test_that("length quantiles match simulation", {
  set.seed(1)
  cases <- list(
    list(mu = c(1, 0.5), S = matrix(c(.04, .01, .01, .09), 2)),
    list(mu = c(0.2, 0), S = diag(2) * .05),
    list(mu = c(3, -2), S = matrix(c(.5, -.3, -.3, .4), 2))
  )
  for (cs in cases) {
    L <- t(chol(cs$S))
    v <- t(cs$mu + L %*% matrix(rnorm(2e5), 2))
    r <- sqrt(rowSums(v^2))
    q <- length_quantiles(cs$mu, cs$S, 0.05)
    expect_equal(q, unname(quantile(r, c(.025, .975))), tolerance = 0.01)
  }
})

test_that("the length distribution function is a distribution function", {
  mu <- c(1, -1); S <- matrix(c(.2, .05, .05, .1), 2)
  r <- c(0, .5, 1, 1.5, 2, 5, 50)
  F <- vapply(r, length_cdf, numeric(1), mu = mu, Sigma = S)
  expect_equal(F[1], 0)
  expect_equal(F[length(F)], 1, tolerance = 1e-6)
  expect_true(all(diff(F) >= 0))
})

pts <- local({
  set.seed(3)
  n <- 80
  d <- data.frame(x = runif(n, -1, 1), y = runif(n, -1, 1))
  d$fx <- 1 + 0.3 * d$y + rnorm(n, 0, 0.1)
  d$fy <- 0.5 + rnorm(n, 0, 0.1)
  d
})

test_that("length intervals contain the predicted length and follow conf_level", {
  ev <- data.frame(x = c(0, 0.5), y = c(0, -0.5))
  get <- function(level) layer_data(ggplot(pts, aes(x, y, fx = fx, fy = fy)) +
    geom_vector_smooth(eval_points = ev, pi_type = "length", conf_level = level))
  d95 <- expect_no_warning(get(0.95))
  d50 <- get(0.5)
  len <- sqrt(d95$fx^2 + d95$fy^2)
  expect_true(all(d95$length_lower < len & len < d95$length_upper))
  expect_true(all(d50$length_upper - d50$length_lower < d95$length_upper - d95$length_lower))
})

test_that("on a grid the length interval is scaled with its vector", {
  d <- layer_data(ggplot(pts, aes(x, y, fx = fx, fy = fy)) +
    geom_vector_smooth(pi_type = "length", n = 4))
  len <- sqrt(d$fx^2 + d$fy^2)
  # every vector is drawn at the same length, and its interval still contains it
  expect_equal(max(len) - min(len), 0, tolerance = 1e-12)
  expect_true(all(d$length_lower < len & len < d$length_upper))
})

test_that("length intervals work for every method and pi_type is checked", {
  ev <- data.frame(x = 0, y = 0)
  for (m in c("gam", "lm", "kriging")) {
    d <- layer_data(ggplot(pts, aes(x, y, fx = fx, fy = fy)) +
      geom_vector_smooth(method = m, eval_points = ev, pi_type = "length"))
    expect_true(is.finite(d$length_lower) && is.finite(d$length_upper), info = m)
  }
  expect_error(geom_vector_smooth(pi_type = "both"), "must be one of")
  expect_error(geom_vector_smooth(pi_type = c("wedge", "length")), "must be one of")
})
