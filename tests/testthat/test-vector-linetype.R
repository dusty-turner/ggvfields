test_that("vector layers pass linetype to the grob", {
  f <- function(u) c(-u[2], u[1])

  p <- ggplot() +
    geom_vector_field(fun = f, xlim = c(-1, 1), ylim = c(-1, 1), n = 3, linetype = "dashed")

  vector_grob <- layer_grob(p)[[1]]$children[[1]]
  expect_equal(unique(vector_grob$gp$lty), "dashed")
})
