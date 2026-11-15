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

test_that("efield_maker() keeps the values it was made with", {
  fields <- list()
  for (q in c(-1, 1)) fields[[length(fields) + 1]] <- efield_maker(q_test = q)
  u <- c(0, 0.5)
  expect_equal(fields[[1]](u), -fields[[2]](u))
  expect_equal(fields[[1]](u), efield(u, rbind(c(-1, -1), c(1, 1)), c(-1, 1), q_test = -1))
})
