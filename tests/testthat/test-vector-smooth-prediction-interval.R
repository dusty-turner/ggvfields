test_that("the gam ellipse is the joint prediction covariance of fx and fy", {
  set.seed(123)
  n <- 60
  pts <- data.frame(x = runif(n, -2, 2), y = runif(n, -2, 2))
  e1 <- rnorm(n); e2 <- rnorm(n)
  pts$fx <- sin(pts$x) + 0.5 * e1
  pts$fy <- cos(pts$y) + 0.3 * (0.7 * e1 + sqrt(1 - 0.7^2) * e2)  # correlated noise
  ev <- data.frame(x = 0, y = 1)

  d <- layer_data(
    ggplot(pts, aes(x, y, fx = fx, fy = fy)) +
      geom_vector_smooth(method = "gam", eval_points = ev, pi_type = "ellipse")
  )

  # the same fit and prediction covariance, computed directly with mgcv: the
  # covariance of the fitted vector plus the residual covariance, scaled by
  # n / (n - edf) for each component
  fit <- mgcv::gam(
    list(fx ~ s(x, y, bs = "tp"), fy ~ s(x, y, bs = "tp")),
    family = mgcv::mvn(d = 2), data = pts
  )
  Xp <- predict(fit, ev, type = "lpmatrix")
  lpi <- attr(Xp, "lpi")
  A <- matrix(0, 2, ncol(Xp))
  A[1, lpi[[1]]] <- Xp[1, lpi[[1]]]
  A[2, lpi[[2]]] <- Xp[1, lpi[[2]]]
  edf <- c(sum(fit$edf[lpi[[1]]]), sum(fit$edf[lpi[[2]]]))
  scale_df <- n / (n - edf)
  S <- solve(crossprod(fit$family$data$R)) * sqrt(outer(scale_df, scale_df))
  Sigma <- A %*% fit$Vp %*% t(A) + S
  ev_vals <- eigen(Sigma, symmetric = TRUE)$values

  scale <- 2 * sqrt(qchisq(0.95, df = 2))
  expect_equal(d$ellipse_width[1],  scale * sqrt(ev_vals[1]), ignore_attr = TRUE)
  expect_equal(d$ellipse_height[1], scale * sqrt(ev_vals[2]), ignore_attr = TRUE)

  # the correlated noise tilts the ellipse
  expect_gt(Sigma[1, 2], 0)
  expect_false(isTRUE(all.equal(d$ellipse_angle[1] %% 180, 0)))
})

test_that("the gam falls back to separate fits for data without noise", {
  set.seed(1)
  pts <- data.frame(x = runif(40, -1, 1), y = runif(40, -1, 1))
  pts$fx <- -pts$y
  pts$fy <- pts$x

  fit <- fit_joint_gam(pts)
  expect_null(fit$joint)
  p <- predict_joint_gam(fit, data.frame(x = .2, y = .3))
  expect_equal(c(p$fx, p$fy), c(-.3, .2), tolerance = 1e-4)
  expect_equal(p$cov_fx_fy, 0)

  expect_no_error(layer_data(
    ggplot(pts, aes(x, y, fx = fx, fy = fy)) + geom_vector_smooth(n = 3)
  ))
})

test_that("small data sets fall back to separate gam fits", {
  set.seed(2)
  pts <- data.frame(x = runif(7), y = runif(7))
  pts$fx <- 1 + rnorm(7, 0, .1)
  pts$fy <- rnorm(7, 0, .1)
  fit <- fit_joint_gam(pts)
  expect_null(fit$joint)
  expect_named(fit, c("fx", "fy"))
})

test_that("joint gam prediction regions have their stated coverage", {
  skip_on_cran()
  # new observations from a known field with correlated noise fall inside the
  # 95% ellipses about 95% of the time
  truth <- function(x, y) cbind(sin(y) + 0.3 * x, cos(x) - 0.2 * y)
  L <- t(chol(matrix(c(0.09, 0.072, 0.072, 0.09), 2)))
  inside <- c()
  for (r in 1:8) {
    set.seed(r)
    d <- data.frame(x = runif(300, -3, 3), y = runif(300, -3, 3))
    d[c("fx", "fy")] <- truth(d$x, d$y) + t(L %*% matrix(rnorm(600), 2))
    nd <- data.frame(x = runif(50, -2.5, 2.5), y = runif(50, -2.5, 2.5))
    obs <- truth(nd$x, nd$y) + t(L %*% matrix(rnorm(100), 2))
    p <- predict_joint_gam(fit_joint_gam(d), nd)
    inside <- c(inside, vapply(seq_len(nrow(nd)), function(i) {
      S <- matrix(c(p$var_fx[i], p$cov_fx_fy[i], p$cov_fx_fy[i], p$var_fy[i]), 2)
      r <- obs[i, ] - c(p$fx[i], p$fy[i])
      drop(r %*% solve(S, r)) <= qchisq(0.95, 2)
    }, logical(1)))
  }
  expect_gt(mean(inside), 0.92)
  expect_lt(mean(inside), 0.98)
})
