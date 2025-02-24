library(testthat)
library(ggvfields)  # Ensure your package is loaded

# Create a small dataset with raw vector data.
set.seed(123)
df <- data.frame(
  x = runif(10, 0, 10),
  y = runif(10, 0, 10)
)
# Compute raw displacements
df$fx <- rnorm(10, 2, 0.5)
df$fy <- rnorm(10, -1, 0.5)
# Also compute endpoints for later comparison.
df$xend <- df$x + df$fx
df$yend <- df$y + df$fy

# Helper: Build a ggplot object and extract computed data from the smooth stat.
get_smooth_data <- function(plot_obj) {
  pb <- ggplot_build(plot_obj)
  # Assuming the smooth layer is the first (or only) layer.
  pb$data[[1]]
}

# 1. Basic test: when no eval_points are provided and pi_type is "ellipse",
# a warning should be issued and the smooth stat switches to "wedge".
test_that("geom_vector_smooth issues warning and switches pi_type when eval_points is NULL", {
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_vector_smooth(aes(fx = fx, fy = fy),
                       n = c(5, 5),
                       pi_type = "ellipse",
                       conf_level = 0.95,
                       formula = cbind(fx, fy) ~ x * y)

  # The warning is issued during the computation (ggplot_build),
  # so wrap that call in expect_warning.
  expect_warning(
    pb <- ggplot_build(p),
    regexp = "eval_points"
  )

  # Now extract computed data from the built plot.
  d <- pb$data[[1]]

  # Check that required columns exist and that endpoints are computed.
  expect_true(all(c("x", "y", "xend", "yend", "id") %in% names(d)))
})

# 2. Test with evaluation points supplied and pi_type "ellipse"
test_that("geom_vector_smooth computes ellipse parameters when eval_points provided", {
  # Create a grid of evaluation points.
  eval_pts <- expand.grid(
    x = seq(min(df$x), max(df$x), length.out = 4),
    y = seq(min(df$y), max(df$y), length.out = 4)
  )

  p <- ggplot(df, aes(x = x, y = y)) +
    geom_vector_smooth(aes(fx = fx, fy = fy),
                       n = c(5, 5),
                       pi_type = "ellipse",
                       conf_level = 0.95,
                       eval_points = eval_pts,
                       formula = cbind(fx, fy) ~ x * y)

  d <- get_smooth_data(p)

  # Expect that the computed grid contains ellipse parameters.
  expect_true(all(c("ellipse_width", "ellipse_height", "ellipse_angle") %in% names(d)))

  # Also check that endpoints are computed (xend = x + fx, etc.)
  expect_equal(d$xend, d$x + d$fx, tolerance = 1e-6)
  expect_equal(d$yend, d$y + d$fy, tolerance = 1e-6)
})

# Helper function to recursively search for a grob with given classes.
find_grob_recursive <- function(grob, classes) {
  if (any(sapply(classes, function(cl) inherits(grob, cl)))) {
    return(TRUE)
  }
  if (!is.null(grob$children) && is.list(grob$children)) {
    return(any(sapply(grob$children, find_grob_recursive, classes = classes)))
  }
  FALSE
}

test_that("GeomVectorSmooth draw_panel produces a grob", {
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_vector_smooth(aes(fx = fx, fy = fy),
                       n = c(5, 5),
                       pi_type = "wedge",
                       conf_level = 0.95,
                       eval_points = NULL,  # forces wedge (via warning)
                       formula = cbind(fx, fy) ~ x * y,
                       arrow = grid::arrow(angle = 20, length = unit(0.015, "npc"), type = "closed"),
                       se = TRUE, se.circle = TRUE)

  # Build the grob table.
  gt <- ggplot_gtable(ggplot_build(p))

  # Recursively search the entire grob tree.
  has_segments <- any(sapply(gt$grobs, find_grob_recursive, classes = c("segments", "polyline")))

  expect_true(has_segments)
})


# 4. Test that the stat (StatVectorSmooth) returns a data frame with predicted endpoints.
test_that("StatVectorSmooth computes prediction intervals and endpoints", {
  # Build a plot with a simple configuration.
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_vector_smooth(aes(fx = fx, fy = fy),
                       n = c(5, 5),
                       pi_type = "wedge",
                       conf_level = 0.95,
                       eval_points = expand.grid(
                         x = seq(min(df$x), max(df$x), length.out = 4),
                         y = seq(min(df$y), max(df$y), length.out = 4)
                       ),
                       formula = cbind(fx, fy) ~ x * y)

  d <- get_smooth_data(p)

  # Check that for each predicted grid point, xend and yend are computed.
  expect_true("xend" %in% names(d))
  expect_true("yend" %in% names(d))

  # Additionally, if pi_type is wedge, we expect prediction interval columns.
  expect_true(all(c("min_angle", "max_angle") %in% names(d)))
})

# 1. Basic test: when no eval_points are provided and pi_type is "ellipse",
# a warning should be issued and the smooth stat switches to "wedge".
test_that("geom_vector_smooth issues warning and switches pi_type when eval_points is NULL", {
  p <- ggplot(df, aes(x = x, y = y)) +
    stat_vector_smooth(aes(fx = fx, fy = fy),
                       n = c(5, 5),
                       pi_type = "ellipse",
                       conf_level = 0.95,
                       formula = cbind(fx, fy) ~ x * y)

  # The warning is issued during the computation (ggplot_build),
  # so wrap that call in expect_warning.
  expect_warning(
    pb <- ggplot_build(p),
    regexp = "eval_points"
  )

  # Now extract computed data from the built plot.
  d <- pb$data[[1]]

  # Check that required columns exist and that endpoints are computed.
  expect_true(all(c("x", "y", "xend", "yend", "id") %in% names(d)))
})
