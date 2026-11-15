test_that("the gam ellipse is a prediction interval: se.fit^2 plus residual variance", {
  set.seed(123)
  n <- 60
  pts <- data.frame(x = runif(n, -2, 2), y = runif(n, -2, 2))
  pts$fx <- sin(pts$x) + rnorm(n, 0, 0.5)
  pts$fy <- cos(pts$y) + rnorm(n, 0, 0.2)
  ev <- data.frame(x = 0, y = 1)

  d <- layer_data(
    ggplot(pts, aes(x, y, fx = fx, fy = fy)) +
      geom_vector_smooth(method = "gam", eval_points = ev, pi_type = "ellipse")
  )

  # the same fits and prediction variances, computed directly with mgcv
  fx_mod <- mgcv::gam(fx ~ s(x, y, bs = "tp"), data = pts)
  fy_mod <- mgcv::gam(fy ~ s(x, y, bs = "tp"), data = pts)
  var_fx <- predict(fx_mod, ev, se.fit = TRUE)$se.fit^2 + fx_mod$sig2
  var_fy <- predict(fy_mod, ev, se.fit = TRUE)$se.fit^2 + fy_mod$sig2

  # zero covariance, so the axes are the two variances
  scale <- 2 * sqrt(qchisq(0.95, df = 2))
  expect_equal(d$ellipse_width[1],  scale * sqrt(max(var_fx, var_fy)), ignore_attr = TRUE)
  expect_equal(d$ellipse_height[1], scale * sqrt(min(var_fx, var_fy)), ignore_attr = TRUE)
})
