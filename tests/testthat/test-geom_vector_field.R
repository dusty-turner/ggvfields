library(testthat)
library(ggvfields)

# Define a simple vector field function: rotates (x, y) to (-y, x)
f <- function(u) {
  c(-u[2], u[1])
}

# Create test data: a grid of seed points
df <- data.frame(
  x = seq(0, 1, length.out = 5),
  y = seq(0, 1, length.out = 5)
)

# Helper function to build and extract computed data.
get_data <- function(p) {
  pb <- ggplot_build(p)
  pb$data[[1]]
}

# Test geom_vector_field: Default mapping should set color = after_stat(norm)
test_that("geom_vector_field computes endpoints and includes required aesthetics", {
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_vector_field(fun = f, xlim = c(0, 1), ylim = c(0, 1))

  d <- get_data(p)

  # Check that required columns exist.
  expect_true(all(c("x", "y", "fx", "fy", "id", "colour") %in% names(d)))

  # Basic check that endpoints are numeric.
  expect_type(d$fx, "double")
  expect_type(d$fy, "double")
})

# Test stat_vector_field: Should produce similar output as geom_vector_field.
test_that("stat_vector_field computes endpoints and includes required aesthetics", {
  p <- ggplot(df, aes(x = x, y = y)) +
    stat_vector_field(fun = f, xlim = c(0, 1), ylim = c(0, 1))

  d <- get_data(p)
  expect_true(all(c("x", "y", "fx", "fy", "id", "colour") %in% names(d)))
})

# Test geom_vector_field2: Default mapping should include length = after_stat(norm)
test_that("geom_vector_field2 includes length aesthetic and computes endpoints", {
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_vector_field2(fun = f, xlim = c(0, 1), ylim = c(0, 1))

  d <- get_data(p)
  # Check that 'length' is computed and that endpoints exist.
  expect_true(all(c("x", "y", "fx", "fy", "id", "colour", "length") %in% names(d)))
})

# Test stat_vector_field2: Should produce similar output as geom_vector_field2.
test_that("stat_vector_field2 includes length aesthetic and computes endpoints", {
  p <- ggplot(df, aes(x = x, y = y)) +
    stat_vector_field2(fun = f, xlim = c(0, 1), ylim = c(0, 1))

  d <- get_data(p)
  expect_true(all(c("x", "y", "fx", "fy", "id", "length") %in% names(d)))
})
