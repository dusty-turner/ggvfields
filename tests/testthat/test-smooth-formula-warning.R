test_that("supplying a formula with method = 'gam' warns", {
  expect_warning(geom_stream_smooth(formula = cbind(fx, fy) ~ x + y), "ignored")
  expect_warning(stat_stream_smooth(formula = cbind(fx, fy) ~ x + y), "ignored")
  expect_warning(geom_vector_smooth(formula = cbind(fx, fy) ~ x + y), "ignored")
  expect_warning(stat_vector_smooth(formula = cbind(fx, fy) ~ x + y), "ignored")
})

test_that("the default formula and method = 'lm' do not warn", {
  expect_no_warning(geom_stream_smooth())
  expect_no_warning(geom_vector_smooth())
  expect_no_warning(geom_stream_smooth(method = "lm", formula = cbind(fx, fy) ~ x + y))
  expect_no_warning(geom_vector_smooth(method = "lm", formula = cbind(fx, fy) ~ x + y))
})
