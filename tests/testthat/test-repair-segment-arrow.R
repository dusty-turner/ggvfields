test_that("repair_segment_arrow keeps one arrowhead per stream", {
  # two streams: one with 3 segments, one with a single segment
  group <- c(1, 1, 1, 1, 2, 2)

  last <- repair_segment_arrow(grid::arrow(length = grid::unit(2, "mm")), group)
  expect_equal(as.numeric(last$length), c(0, 0, 2, 2))
  expect_equal(last$ends, c(2L, 2L, 2L, 2L))

  first <- repair_segment_arrow(grid::arrow(length = grid::unit(2, "mm"), ends = "first"), group)
  expect_equal(as.numeric(first$length), c(2, 0, 0, 2))

  both <- repair_segment_arrow(grid::arrow(length = grid::unit(2, "mm"), ends = "both"), group)
  expect_equal(as.numeric(both$length), c(2, 0, 2, 2))
  expect_equal(both$ends, c(1L, 3L, 2L, 3L))

  expect_null(repair_segment_arrow(NULL, group))
})
