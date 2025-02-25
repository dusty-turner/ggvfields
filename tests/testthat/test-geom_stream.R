library(testthat)
library(ggvfields)

# Create sample stream data for testing
stream_1 <- data.frame(
  x = c(0, 3),
  y = c(0, 0),
  t = 0:1
)
stream_2 <- data.frame(
  x = c(1, 1),
  y = c(1, 5),
  t = 0:1
)
stream_3 <- data.frame(
  x = c(2, 5),
  y = c(2, 6),
  t = 0:1
)
streams <- rbind(
  cbind(stream_1, id = 1),
  cbind(stream_2, id = 2),
  cbind(stream_3, id = 3)
)

# Build a ggplot with geom_stream and check that the computed data is as expected.
test_that("ggplot with geom_stream processes data correctly", {
  p <- ggplot(streams, aes(x = x, y = y, t = t, group = id)) +
    geom_stream()

  # Build the plot to inspect computed data.
  pb <- ggplot_build(p)
  expect_true(length(pb$data) > 0)

  stream_data <- pb$data[[1]]

  # Verify that the computed data contains the required 't' aesthetic.
  expect_true("t" %in% names(stream_data))

  # For each group, the t values should be sorted.
  grouped_t <- split(stream_data$t, stream_data$group)
  expect_true(all(vapply(grouped_t, function(vec) identical(vec, sort(vec)), logical(1))))

  # Check that key aesthetics (like x and y positions) are present.
  expect_true(all(c("x", "y") %in% names(stream_data)))
})


# Also test the layer constructors using data-based reasoning
test_that("geom_stream and stat_stream create proper layers", {
  # Using data-based mapping with id converted to group automatically.
  mapping <- aes(x = x, y = y, t = t, id = I(1))
  # Expect a warning about the unknown 'id' aesthetic; capture it.
  layer_obj1 <- geom_stream(mapping = mapping)
  expect_s3_class(layer_obj1, "LayerInstance")
  expect_equal(layer_obj1$stat, StatStream)
  expect_equal(layer_obj1$geom, GeomStream)
  expect_true("group" %in% names(layer_obj1$mapping))

  # Similarly for stat_stream.
  layer_obj2 <- stat_stream(mapping = mapping)

  expect_s3_class(layer_obj2, "LayerInstance")
  expect_equal(layer_obj2$stat, StatStream)
  expect_equal(layer_obj2$geom, GeomStream)
  expect_true("group" %in% names(layer_obj2$mapping))
})

# Test error condition when required aesthetic 't' is missing.
test_that("StatStream errors when 't' aesthetic is missing", {
  data_no_t <- data.frame(x = 1:5, y = 1:5)
  expect_error(
    StatStream$compute_group(data_no_t, scales::identity_trans()),
    "StatStream requires a 't'"
  )
})

# Test compute_group with valid data to ensure norm is computed.
test_that("StatStream compute_group adds 'norm' correctly", {
  data_valid <- data.frame(x = 1:5, y = 6:10, t = seq(0, 1, length.out = 5))
  computed <- StatStream$compute_group(data_valid, scales::identity_trans())
  expect_true("norm" %in% names(computed))

  # Calculate expected norm from the range of x and y.
  expected_norm <- sqrt((diff(range(data_valid$x)))^2 + (diff(range(data_valid$y)))^2)
  expect_equal(computed$norm[1], expected_norm)
})

# Test that GeomStream's setup_data properly removes infinite t values.
test_that("GeomStream setup_data removes infinite t values", {
  data_inf <- data.frame(
    x = 1:4,
    y = 1:4,
    t = c(0, 1, Inf, 2),
    group = rep(1, 4)
  )
  clean_data <- GeomStream$setup_data(data_inf, NULL)
  expect_false(any(is.infinite(clean_data$t)))
})

# Test the scale_length_continuous function using a ggplot example.
test_that("scale_length_continuous works as expected", {
  # For default max_range (<= 0.5), it returns a continuous scale.
  scale_obj <- scale_length_continuous()
  expect_true(inherits(scale_obj, "ScaleContinuous"))

  # For max_range > 0.5, it returns a list (scale + theme modification).
  scale_list <- scale_length_continuous(max_range = 0.6)
  expect_true(is.list(scale_list))
  expect_length(scale_list, 2)
})

