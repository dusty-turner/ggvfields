library(testthat)
library(ggvfields)  # Assumes your package is loaded

# Create test data using explicit endpoints
vectors1 <- data.frame(
  x    = c(0, 1, 2),
  y    = c(0, 1, 2),
  xend = c(3, 1, 5),
  yend = c(0, 5, 6)
)

# Create test data using polar coordinates (angle in radians, distance)
vectors2 <- data.frame(
  x        = c(0, 1, 2),
  y        = c(0, 1, 2),
  angle    = c(0, pi/2, pi/4),
  distance = c(3, 4, 5)
)

# ---- Tests for explicit endpoints input ----

test_that("geom_vector computes endpoints correctly (explicit endpoints)", {
  p <- ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_vector(center = FALSE, normalize = FALSE)

  pb <- ggplot_build(p)
  d <- pb$data[[1]]

  # Expect two rows per vector (one for t = 0 and one for t = 1)
  expect_equal(nrow(d), nrow(vectors1) * 2)

  # Check that required columns are present
  expect_true(all(c("x", "y", "t", "group") %in% names(d)))

  # For each vector, the first row (tail) should have t = 0 and the second (head) t = 1.
  groups <- unique(d$group)
  for (g in groups) {
    sub <- d[d$group == g, ]
    expect_equal(sub$t[1], 0)
    expect_equal(sub$t[2], 1)
  }
})

test_that("stat_vector computes endpoints correctly (explicit endpoints)", {
  p <- ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
    stat_vector(center = FALSE, normalize = FALSE)

  pb <- ggplot_build(p)
  d <- pb$data[[1]]

  # Expect two rows per vector
  expect_equal(nrow(d), nrow(vectors1) * 2)

  # For each vector, verify t values
  groups <- unique(d$group)
  for (g in groups) {
    sub <- d[d$group == g, ]
    expect_equal(sub$t[1], 0)
    expect_equal(sub$t[2], 1)
  }
})

# ---- Tests for polar coordinates input ----

test_that("geom_vector computes endpoints correctly (angle/distance input)", {
  p <- ggplot(vectors2) +
    geom_vector(aes(x = x, y = y, angle = angle, distance = distance),
                center = FALSE, normalize = FALSE)

  pb <- ggplot_build(p)
  d <- pb$data[[1]]

  # Expect two rows per vector
  expect_equal(nrow(d), nrow(vectors2) * 2)

  # For the first vector:
  # starting point is (0,0) and angle = 0, distance = 3 so endpoint should be (3,0)
  first_vec <- d[d$group == 1, ]
  expect_equal(first_vec$t[1], 0)
  expect_equal(first_vec$t[2], 1)
  # The second row (head) should have the computed endpoint.
  expect_equal(first_vec$x[2], 0 + 3 * cos(0), tolerance = 1e-6)
  expect_equal(first_vec$y[2], 0 + 3 * sin(0), tolerance = 1e-6)
})

test_that("geom_vector computes endpoints correctly (explicit endpoints)", {
  p <- ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_vector2()

  pb <- ggplot_build(p)
  d <- pb$data[[1]]

  # Expect two rows per vector (one for t = 0 and one for t = 1)
  expect_equal(nrow(d), nrow(vectors1) * 2)

  # Check that required columns are present
  expect_true(all(c("x", "y", "t", "group") %in% names(d)))

  # For each vector, the first row (tail) should have t = 0 and the second (head) t = 1.
  groups <- unique(d$group)
  for (g in groups) {
    sub <- d[d$group == g, ]
    expect_equal(sub$t[1], 0)
    expect_equal(sub$t[2], 1)
  }
})

test_that("geom_vector computes endpoints correctly (explicit endpoints)", {
  p <- ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
    stat_vector(center = FALSE, normalize = FALSE)

  pb <- ggplot_build(p)
  d <- pb$data[[1]]

  # Expect two rows per vector (one for t = 0 and one for t = 1)
  expect_equal(nrow(d), nrow(vectors1) * 2)

  # Check that required columns are present
  expect_true(all(c("x", "y", "t", "group") %in% names(d)))

  # For each vector, the first row (tail) should have t = 0 and the second (head) t = 1.
  groups <- unique(d$group)
  for (g in groups) {
    sub <- d[d$group == g, ]
    expect_equal(sub$t[1], 0)
    expect_equal(sub$t[2], 1)
  }
})

test_that("geom_vector computes endpoints correctly (explicit endpoints)", {
  p <- ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
    stat_vector2()

  pb <- ggplot_build(p)
  d <- pb$data[[1]]

  # Expect two rows per vector (one for t = 0 and one for t = 1)
  expect_equal(nrow(d), nrow(vectors1) * 2)

  # Check that required columns are present
  expect_true(all(c("x", "y", "t", "group") %in% names(d)))

  # For each vector, the first row (tail) should have t = 0 and the second (head) t = 1.
  groups <- unique(d$group)
  for (g in groups) {
    sub <- d[d$group == g, ]
    expect_equal(sub$t[1], 0)
    expect_equal(sub$t[2], 1)
  }
})

