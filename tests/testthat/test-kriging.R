test_that("kriging does not depend on the units of x and y", {
  skip_if_not_installed("gstat")
  skip_if_not_installed("sp")
  f <- efield_maker(log = TRUE)
  set.seed(123)
  n <- 60
  pts <- data.frame(x = runif(n, -2, 2), y = runif(n, -2, 2))
  v <- t(apply(pts, 1, f))
  pts$fx <- v[, 1] + rnorm(n, 0, .2)
  pts$fy <- v[, 2] + rnorm(n, 0, .2)
  ev <- data.frame(x = c(0, 1, -1.5), y = c(1, -0.5, 0.3))

  krig <- function(k) {
    p <- pts; p$x <- p$x * k; p$y <- p$y * k
    e <- ev; e$x <- e$x * k; e$y <- e$y * k
    d <- suppressWarnings(layer_data(
      ggplot(p, aes(x, y, fx = fx, fy = fy)) +
        geom_vector_smooth(method = "kriging", eval_points = e)
    ))
    d <- d[!duplicated(d$id), ]
    d[order(d$id), c("fx", "fy", "ellipse_width", "ellipse_height")]
  }
  ref <- krig(1)
  expect_true(all(is.finite(as.matrix(ref))))
  for (k in c(1e-4, 100)) expect_equal(krig(k), ref, tolerance = 1e-6)
})
