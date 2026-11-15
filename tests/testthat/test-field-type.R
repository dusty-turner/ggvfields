test_that("every field layer rejects an unknown type with the same message", {
  f <- function(u) c(1, 1)
  s <- function(u) sum(u^2)
  expect_error(geom_stream_field(fun = f, type = "bad"), "must be \"stream\" or \"vector\"")
  expect_error(geom_gradient_field(fun = s, type = "bad"), "must be \"stream\" or \"vector\"")
  expect_error(geom_gradient_field2(fun = s, type = "bad"), "must be \"stream\" or \"vector\"")
  expect_error(geom_gradient_smooth(type = "bad"), "must be \"stream\" or \"vector\"")
})

test_that("geom_gradient_smooth keeps the user's color mapping", {
  l <- geom_gradient_smooth(aes(colour = after_stat(norm * 2)))
  expect_identical(rlang::as_label(l$mapping$colour), "after_stat(norm * 2)")

  # without one, the default depends on the type
  expect_identical(rlang::as_label(geom_gradient_smooth(type = "stream")$mapping$colour), "after_stat(avg_spd)")
  expect_identical(rlang::as_label(geom_gradient_smooth(type = "vector")$mapping$colour), "after_stat(norm)")
})
