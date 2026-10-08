test_that("an undefined seed does not blank the stream layer", {
  # with n = 5 on [-2, 2]^2, two of the 25 seeds sit on the charges at
  # (-1, -1) and (1, 1), where the field is undefined
  for (normalize in c(TRUE, FALSE)) {
    p <- ggplot() +
      geom_stream_field(
        fun = efield_maker(), xlim = c(-2, 2), ylim = c(-2, 2), n = 5,
        normalize = normalize
      )

    expect_no_warning(d <- layer_data(p))
    expect_equal(length(unique(d$group)), 23)
  }
})
