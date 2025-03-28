library(testthat)
library(ggvfields)  # Assumes your package is loaded

# Test that geom_stream_smooth produces computed data with required aesthetics.
test_that("geom_stream_smooth layer produces computed data with expected aesthetics", {
  # Create a sample dataset.
  set.seed(123)
  df <- data.frame(
    x = runif(100, -2, 2),
    y = runif(100, -2, 2),
    fx = rnorm(100),
    fy = rnorm(100)
  )

  # Build a plot with a raw vector layer (for context) and a smoothed layer.
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_vector(aes(fx = fx, fy = fy), color = "black", normalize = FALSE) +
    geom_stream_smooth(
      data = df,
      # formula = cbind(fx, fy) ~ x * y,
      n = 5,
      normalize = TRUE,
      center = FALSE
    )

  # Build the plot.
  pb <- ggplot_build(p)

  # Assuming geom_stream_smooth is added as a separate layer (here, second layer)
  # Check that the computed data include x, y, t and group.
  d <- pb$data[[2]]
  expect_true(all(c("x", "y", "t", "group") %in% names(d)))
})

# Test that supplying eval_points affects the output.
test_that("geom_stream_smooth uses evaluation points when provided", {
  # Create a sample dataset.
  set.seed(456)
  df <- data.frame(
    x = runif(150, -2, 2),
    y = runif(150, -2, 2),
    fx = rnorm(150),
    fy = rnorm(150)
  )

  # Create a grid of evaluation points.
  eval_pts <- expand.grid(
    x = seq(-1, 1, length.out = 3),
    y = seq(-1, 1, length.out = 3)
  )

  # Build a plot using geom_stream_smooth with the eval_points parameter.
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_stream_smooth(
      data = df,
      formula = cbind(fx, fy) ~ x * y,
      eval_points = eval_pts,
      n = 5,
      normalize = TRUE,
      center = FALSE,
      arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
    )

  pb <- ggplot_build(p)
  d <- pb$data[[1]]

  # We expect the smoothing to be computed at the eval_points.
  # For example, the number of unique x-values in the computed data should be at least
  # as many as in the eval_pts grid (here, 3), though due to coordinate transforms it might be more.
  expect_true(length(unique(d$x)) >= 3)
})

# Test that geom_stream_smooth produces computed data with required aesthetics.
test_that("stat_stream_smooth layer produces computed data with expected aesthetics", {
  # Create a sample dataset.
  set.seed(123)
  df <- data.frame(
    x = runif(100, -2, 2),
    y = runif(100, -2, 2),
    fx = rnorm(100),
    fy = rnorm(100)
  )

  # Build a plot with a raw vector layer (for context) and a smoothed layer.
  p <- ggplot(df, aes(x = x, y = y)) +
    stat_vector(aes(fx = fx, fy = fy), color = "black", normalize = FALSE) +
    stat_stream_smooth(
      data = df,
      formula = cbind(fx, fy) ~ x * y,
      n = 5,
      normalize = TRUE,
      center = FALSE
    )

  # Build the plot.
  pb <- ggplot_build(p)

  # Assuming stat_stream_smooth is added as a separate layer (here, second layer)
  # Check that the computed data include x, y, t and group.
  d <- pb$data[[2]]
  expect_true(all(c("x", "y", "t", "group") %in% names(d)))
})

# Test that supplying eval_points affects the output.
test_that("stat_stream_smooth uses evaluation points when provided", {
  # Create a sample dataset.
  set.seed(456)
  df <- data.frame(
    x = runif(150, -2, 2),
    y = runif(150, -2, 2),
    fx = rnorm(150),
    fy = rnorm(150)
  )

  # Create a grid of evaluation points.
  eval_pts <- expand.grid(
    x = seq(-1, 1, length.out = 3),
    y = seq(-1, 1, length.out = 3)
  )

  # Build a plot using stat_stream_smooth with the eval_points parameter.
  p <- ggplot(df, aes(x = x, y = y)) +
    stat_stream_smooth(
      data = df,
      formula = cbind(fx, fy) ~ x * y,
      eval_points = eval_pts,
      n = 5,
      normalize = TRUE,
      center = FALSE,
      arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed")
    )

  pb <- ggplot_build(p)
  d <- pb$data[[1]]

  # We expect the smoothing to be computed at the eval_points.
  # For example, the number of unique x-values in the computed data should be at least
  # as many as in the eval_pts grid (here, 3), though due to coordinate transforms it might be more.
  expect_true(length(unique(d$x)) >= 3)
})

