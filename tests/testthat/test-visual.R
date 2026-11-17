# visual snapshots, one or two per layer family. the plots are only drawn
# and compared off CRAN
skip_on_cran()
skip_if_not_installed("vdiffr")

rotation <- function(u) c(-u[2], u[1])
paraboloid <- function(u) u[1]^2 + u[2]^2

wind <- local({
  set.seed(1234)
  n <- 30
  d <- data.frame(x = rnorm(n), y = rnorm(n), dir = runif(n, -pi/2, pi/2), spd = rchisq(n, df = 2))
  d$fx <- d$spd * cos(d$dir)
  d$fy <- d$spd * sin(d$dir)
  d
})

test_that("vector layers draw as expected", {
  vdiffr::expect_doppelganger("geom_vector",
    ggplot(wind, aes(x, y, fx = fx, fy = fy)) + geom_vector())
  vdiffr::expect_doppelganger("geom_vector2",
    ggplot(wind, aes(x, y, fx = fx, fy = fy)) + geom_vector2())
  vdiffr::expect_doppelganger("stat_vector",
    ggplot(wind, aes(x, y, fx = fx, fy = fy)) + stat_vector())
})

test_that("vector field layers draw as expected", {
  vdiffr::expect_doppelganger("geom_vector_field",
    ggplot() + geom_vector_field(fun = rotation, n = 7))
  vdiffr::expect_doppelganger("geom_vector_field2",
    ggplot() + geom_vector_field2(fun = rotation, n = 7))
})

test_that("stream field layers draw as expected", {
  vdiffr::expect_doppelganger("geom_stream_field",
    ggplot() + geom_stream_field(fun = rotation, n = 7))
  vdiffr::expect_doppelganger("geom_stream_field2",
    ggplot() + geom_stream_field2(fun = rotation, n = 7))
  vdiffr::expect_doppelganger("geom_stream_field not normalized",
    ggplot() + geom_stream_field(fun = rotation, n = 5, normalize = FALSE))
  vdiffr::expect_doppelganger("geom_stream_field efield",
    ggplot() + geom_stream_field(fun = efield_maker(), xlim = c(-2, 2), ylim = c(-2, 2), n = 7))
})

test_that("gradient and potential layers draw as expected", {
  vdiffr::expect_doppelganger("geom_gradient_field stream",
    ggplot() + geom_gradient_field(fun = paraboloid, type = "stream", n = 7))
  vdiffr::expect_doppelganger("geom_gradient_field2 vector",
    ggplot() + geom_gradient_field2(fun = paraboloid, type = "vector", n = 7))
  vdiffr::expect_doppelganger("geom_potential",
    ggplot() + geom_potential(fun = function(u) c(-2 * u[1], -2 * u[2]), n = 21))
})

test_that("smoothing layers draw as expected", {
  vdiffr::expect_doppelganger("geom_vector_smooth gam ellipse",
    ggplot(wind, aes(x, y, fx = fx, fy = fy)) + geom_vector() +
      geom_vector_smooth(eval_points = data.frame(x = 0, y = 0)))
  vdiffr::expect_doppelganger("geom_vector_smooth length",
    ggplot(wind, aes(x, y, fx = fx, fy = fy)) + geom_vector() +
      geom_vector_smooth(method = "lm", eval_points = data.frame(x = 0, y = 0), pi_type = "length"))
  vdiffr::expect_doppelganger("geom_stream_smooth",
    ggplot(wind, aes(x, y, fx = fx, fy = fy)) + geom_stream_smooth(n = 5))
})
