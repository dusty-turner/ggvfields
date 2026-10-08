test_that("geom_stream_smooth accepts fixed aesthetics without warning", {
  expect_no_warning(geom_stream_smooth(colour = "red"))
  expect_no_warning(geom_stream_smooth(color = "red"))
  expect_no_warning(geom_stream_smooth(linewidth = 2))
  expect_no_warning(stat_stream_smooth(colour = "red"))

  expect_equal(geom_stream_smooth(colour = "red")$aes_params$colour, "red")
})

test_that("geom_stream_smooth is blue unless a colour is given", {
  expect_equal(geom_stream_smooth()$aes_params$colour, "blue")
  expect_equal(geom_stream_smooth(linewidth = 2)$aes_params$colour, "blue")
})
