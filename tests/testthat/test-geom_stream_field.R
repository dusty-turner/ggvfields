library(testthat)
library(ggvfields)

# A helper function to recursively search through a grob tree for a given class.
find_grob_recursive <- function(grob, classes) {
  if (any(sapply(classes, function(cl) inherits(grob, cl)))) {
    return(TRUE)
  }
  if (!is.null(grob$children) && is.list(grob$children)) {
    return(any(vapply(grob$children, find_grob_recursive, logical(1), classes = classes)))
  }
  FALSE
}

# Define a simple circular vector field function: rotates the point.
f_circular <- function(u) {
  # For a point (x,y), return (-y, x)
  c(-u[2], u[1])
}

test_that("geom_stream_field computes streamlines with required aesthetics", {
  # Build a ggplot using geom_stream_field with our circular vector field.
  p <- ggplot() +
    geom_stream_field(fun = f_circular, xlim = c(-1, 1), ylim = c(-1, 1))

  # Build the plot to inspect computed data.
  pb <- ggplot_build(p)
  expect_true(length(pb$data) > 0)

  # For our custom stat (StatStreamField), the computed data should include x, y, and t.
  stream_field_data <- pb$data[[1]]
  expect_true(all(c("x", "y", "t") %in% names(stream_field_data)))

  # Optionally, you could check that a grouping variable ("id") is present.
  expect_true("id" %in% names(stream_field_data))
})

test_that("geom_stream_field grob tree contains expected stream components", {
  # Build a plot with additional parameters (arrow, tail_point, and eval_point)
  p <- ggplot() +
    geom_stream_field(fun = f_circular,
                      xlim = c(-1, 1), ylim = c(-1, 1),
                      tail_point = TRUE, eval_point = TRUE,
                      arrow = grid::arrow(angle = 30, length = unit(0.02, "npc")))

  # Convert the plot to a grob table.
  gt <- ggplot_gtable(ggplot_build(p))

  # Recursively search the grob tree for expected grob classes.
  # Our GeomStream (used by geom_stream_field) should produce either segments or polyline grobs.
  found_stream_grob <- any(vapply(gt$grobs, find_grob_recursive, logical(1), classes = c("segments", "polyline")))
  expect_true(found_stream_grob)
})

test_that("geom_stream_field layer is built correctly with proper defaults", {
  # Build a layer with geom_stream_field using a non-default mapping.
  p <- ggplot() +
    geom_stream_field(fun = f_circular, xlim = c(-1,1), ylim = c(-1,1),
                      mapping = aes(color = after_stat(avg_spd)))

  # Build the plot and inspect the layer.
  pb <- ggplot_build(p)
  expect_true(length(pb$data) > 0)

  stream_field_data <- pb$data[[1]]
  # Check that the default mapping computed by the stat is applied
  expect_true("colour" %in% names(stream_field_data))
})

test_that("stat_stream_field builds a proper ggplot2 layer and computes data", {
  p <- ggplot() +
    stat_stream_field(fun = f_circular, xlim = c(-1, 1), ylim = c(-1, 1), n = 5)

  pb <- ggplot_build(p)
  # Extract computed data from the layer
  data <- pb$data[[1]]

  # Check that the computed data includes the required columns.
  expect_true(all(c("x", "y", "t") %in% names(data)))
  expect_true("id" %in% names(data))
})

test_that("stat_stream_field2 builds a proper ggplot2 layer and computes data", {
  p <- ggplot() +
    stat_stream_field2(fun = f_circular, xlim = c(-1, 1), ylim = c(-1, 1), n = 5)

  pb <- ggplot_build(p)
  data <- pb$data[[1]]

  # Check that the computed data includes the required columns.
  expect_true(all(c("x", "y", "t") %in% names(data)))
  expect_true("id" %in% names(data))
})

test_that("geom_stream_field2 builds a proper ggplot2 layer and computes data", {
  p <- ggplot() +
    geom_stream_field2(fun = f_circular, xlim = c(-1, 1), ylim = c(-1, 1), n = 5)

  pb <- ggplot_build(p)
  data <- pb$data[[1]]

  # Check that the computed data includes the required columns.
  expect_true(all(c("x", "y", "t") %in% names(data)))
  expect_true("id" %in% names(data))
})

f <- efield_maker()

test_that("geom_stream_field2 builds a proper ggplot2 layer and computes data", {
  p <- ggplot() +
    geom_stream_field(fun = f, xlim = c(-1, 1), ylim = c(-1, 1), n = 5, type = "vector", center = FALSE, grid = "hex")

  pb <- ggplot_build(p)
  data <- pb$data[[1]]

  # Check that the computed data includes the required columns.
  expect_true(all(c("x", "y", "t") %in% names(data)))
  expect_true("id" %in% names(data))
})
