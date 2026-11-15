test_that("crop_stream_time keeps l consistent with d", {
  d <- data.frame(t = 0:3, x = c(0, 1, 3, 6), y = 0, d = c(NA, 1, 2, 3), l = c(0, 1, 3, 6))

  expect_equal(crop_stream_time(d, T = 2.5)$l[4], 4.5)

  # cropping inside the first segment
  expect_equal(crop_stream_time(d, T = 0.5)$l[2], 0.5)
})

test_that("avg_spd is right for time-cropped, uncentered streams", {
  # speed is exactly 1 on the unit circle of the rotation field
  f <- function(u) c(-u[2], u[1])
  s <- ode_stepper(c(1, 0), f, T = 1, L = 1e6, center = FALSE)

  expect_equal(s$avg_spd[1], 1, tolerance = 1e-2)
})
