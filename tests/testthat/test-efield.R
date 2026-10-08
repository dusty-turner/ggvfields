test_that("efield_maker forwards k and q_test", {
  pos <- rbind(c(-1, -1), c(1, 1))
  ch <- c(-1, 1)

  expect_equal(
    efield_maker(pos, ch, k = 5, q_test = 2)(c(0, 1)),
    efield(c(0, 1), pos, ch, k = 5, q_test = 2)
  )

  # defaults are unchanged
  expect_equal(efield_maker(pos, ch)(c(0, 1)), efield(c(0, 1), pos, ch))
})
