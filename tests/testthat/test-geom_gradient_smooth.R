library(testthat)
library(ggvfields)

# Create sample data for gradient field testing
set.seed(123)
sample_data <- data.frame(
  x = runif(100, -10, 10),
  y = runif(100, -10, 10)
)
# Define a simple scalar field (z) as a function of x and y
sample_data$z <- sample_data$x^2 + sample_data$y^2

test_that("ggplot with geom_gradient_smooth processes data correctly", {
  p <- ggplot(sample_data, aes(x = x, y = y, z = z)) +
    geom_gradient_smooth()

  # Build the plot to extract computed (stat) data.
  pb <- ggplot_build(p)
  expect_true(length(pb$data) > 0)

  gradient_data <- pb$data[[1]]

  # Check that the computed data contains the expected x and y aesthetics.
  expect_true(all(c("x", "y") %in% names(gradient_data)))

  # Optionally, since 'z' is used internally and then dropped, our modified stat should
  # include a dummy z (if implemented that way) to avoid warnings.
  # Here we test that a z column exists (even if it is NA).
  expect_true("z" %in% names(gradient_data))
})

test_that("geom_gradient_smooth layer creates a proper LayerInstance", {
  layer_obj <- geom_gradient_smooth(mapping = aes(x = x, y = y, z = z))
  expect_s3_class(layer_obj, "LayerInstance")

  # Check that the layer uses the default StatStreamField (or modified one if applicable)
  expect_equal(layer_obj$stat, StatStreamField)

  # And that the layer uses GeomStream as its geom.
  expect_equal(layer_obj$geom, GeomStream)
})

test_that("ggplot with stat_gradient_smooth processes data correctly", {
  p <- ggplot(sample_data, aes(x = x, y = y, z = z)) +
    stat_gradient_smooth()

  # Build the plot to extract computed (stat) data.
  pb <- ggplot_build(p)
  expect_true(length(pb$data) > 0)

  gradient_data <- pb$data[[1]]

  # Check that the computed data contains the expected x and y aesthetics.
  expect_true(all(c("x", "y") %in% names(gradient_data)))

  # Optionally, since 'z' is used internally and then dropped, our modified stat should
  # include a dummy z (if implemented that way) to avoid warnings.
  # Here we test that a z column exists (even if it is NA).
  expect_true("z" %in% names(gradient_data))
})

test_that("stat_gradient_smooth layer creates a proper LayerInstance", {
  layer_obj <- stat_gradient_smooth(mapping = aes(x = x, y = y, z = z))
  expect_s3_class(layer_obj, "LayerInstance")

  # Check that the layer uses the default StatStreamField (or modified one if applicable)
  expect_equal(layer_obj$stat, StatStreamField)

  # And that the layer uses GeomStream as its geom.
  expect_equal(layer_obj$geom, GeomStream)
})

