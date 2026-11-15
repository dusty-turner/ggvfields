test_that("layers computed from a function can be added to a plot with its own data", {
  set.seed(1)
  df <- data.frame(lon = rnorm(20), lat = rnorm(20), u = rnorm(20), v = rnorm(20))
  base <- ggplot(df, aes(x = lon, y = lat, fx = u, fy = v)) + geom_vector()

  vf <- function(u) c(-u[2], u[1])
  sf <- function(u) u[1]^2 + u[2]^2
  layers <- list(
    geom_vector_field(fun = vf), stat_vector_field(fun = vf),
    geom_vector_field2(fun = vf), stat_vector_field2(fun = vf),
    geom_stream_field(fun = vf), stat_stream_field(fun = vf),
    geom_stream_field2(fun = vf), stat_stream_field2(fun = vf),
    geom_gradient_field(fun = sf), stat_gradient_field(fun = sf),
    geom_gradient_field2(fun = sf), stat_gradient_field2(fun = sf),
    geom_potential(fun = vf), stat_potential(fun = vf)
  )

  for (l in layers) {
    expect_false(l$inherit.aes)
    expect_no_error(suppressMessages(ggplot_build(base + l)))
  }
})
