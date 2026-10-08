test_that("geom_stream handles one observation per group", {
  d <- data.frame(x = 1:3, y = 1:3, id = 1:3)
  p <- ggplot(d) + geom_stream(aes(x, y, group = id))

  expect_message(ggplotGrob(p), "only one observation")
})
