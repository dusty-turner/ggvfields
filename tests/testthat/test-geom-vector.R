# # Load required libraries
# # library(testthat)
# # library(ggplot2) # Assuming geom_vector and geom_vector2 are extensions from a package
#
# # Create reproducible data for testing
# wind_data <-
#   structure(list(
#     lon = c(-0.56,-0.23, 1.56, 0.07, 0.13, 1.72, 0.46,-1.27,-0.69,-0.45),
#     lat = c(1.22, 0.36, 0.4, 0.11,-0.56, 1.79, 0.5,-1.97, 0.7,-0.47),
#     wind_dir = c(-2.24,-0.54,-0.54,-0.82,-2.18,-2.27,-1.68,-0.21,-1.47, 2.25),
#     wind_spd = c(0.67, 2.67, 2.63, 2.5, 2.21, 1.94, 0.91, 1.45, 0.02, 0.73),
#     dx = c(-0.73,-3.25, 0.01,-1.03, 0.13,-0.85, 0.41, 0.78, 0.85, 0.03),
#     dy = c(-0.01,-4.11,-4.81,-0.69,-1.74, 2.62,-0.42,-0.04,-0.24, 0.03)
#   ),
#   row.names = c(NA, -10L), class = "data.frame")
#
# test_that("Basic vector plot using dx and dy works", {
#   p <- ggplot(wind_data) +
#     geom_vector(aes(x = lon, y = lat, dx = dx, dy = dy))
#
#   # Check if p is a ggplot object
#   expect_s3_class(p, "ggplot")
#
#   # Check the mapping at the layer level
#   layer_mapping <- p$layers[[1]]$mapping
#   expect_true("x" %in% names(layer_mapping))
#   expect_true("y" %in% names(layer_mapping))
#   expect_true("dx" %in% names(layer_mapping))
#   expect_true("dy" %in% names(layer_mapping))
# })
#
# test_that("Basic vector plot turning off center and normalize", {
#   p <- ggplot(wind_data) +
#     geom_vector(aes(x = lon, y = lat, dx = dx, dy = dy), center = FALSE, normalize = FALSE)
#
#   # Check if p is a ggplot object
#   expect_s3_class(p, "ggplot")
#
#   # Check the mapping at the layer level
#   layer_mapping <- p$layers[[1]]$mapping
#   expect_true("x" %in% names(layer_mapping))
#   expect_true("y" %in% names(layer_mapping))
#   expect_true("dx" %in% names(layer_mapping))
#   expect_true("dy" %in% names(layer_mapping))
# })
#
# test_that("Stat: Basic vector plot using dx and dy works", {
#   p <- ggplot(wind_data) +
#     stat_vector(aes(x = lon, y = lat, dx = dx, dy = dy))
#
#   # Check if p is a ggplot object
#   expect_s3_class(p, "ggplot")
#
#   # Check the mapping at the layer level
#   layer_mapping <- p$layers[[1]]$mapping
#   expect_true("x" %in% names(layer_mapping))
#   expect_true("y" %in% names(layer_mapping))
#   expect_true("dx" %in% names(layer_mapping))
#   expect_true("dy" %in% names(layer_mapping))
# })
#
# test_that("Verify turning off length works", {
#   p <- ggplot(wind_data) +
#     geom_vector(aes(x = lon, y = lat, dx = dx, dy = dy, length = after_stat(NULL)))
#
#   # Check if p is a ggplot object
#   expect_s3_class(p, "ggplot")
#
#   # Check the mapping at the layer level
#   layer_mapping <- p$layers[[1]]$mapping
#   expect_true("x" %in% names(layer_mapping))
#   expect_true("y" %in% names(layer_mapping))
#   expect_true("dx" %in% names(layer_mapping))
#   expect_true("dy" %in% names(layer_mapping))
# })
#
#
# test_that("Vector plot using angle and distance works", {
#   p <- ggplot(wind_data) +
#     geom_vector(aes(x = lon, y = lat, angle = wind_dir, distance = wind_spd))
#
#   # Check if p is a ggplot object
#   expect_s3_class(p, "ggplot")
#
#   # Check the mapping at the layer level
#   layer_mapping <- p$layers[[1]]$mapping
#   expect_true("x" %in% names(layer_mapping))
#   expect_true("y" %in% names(layer_mapping))
#   expect_true("angle" %in% names(layer_mapping))
#   expect_true("distance" %in% names(layer_mapping))
# })
#
# test_that("Vector length scaling using geom_vector2 works", {
#   p <- ggplot(wind_data) +
#     geom_vector2(aes(x = lon, y = lat, dx = dx, dy = dy))
#
#   # Check if p is a ggplot object
#   expect_s3_class(p, "ggplot")
#
#   # Check the mapping at the layer level
#   layer_mapping <- p$layers[[1]]$mapping
#   expect_true("x" %in% names(layer_mapping))
#   expect_true("y" %in% names(layer_mapping))
#   expect_true("dx" %in% names(layer_mapping))
#   expect_true("dy" %in% names(layer_mapping))
# })
#
# test_that("Manual vector length mapping using after_stat(norm) works", {
#   p <- ggplot(wind_data) +
#     geom_vector(aes(x = lon, y = lat, dx = dx, dy = dy, length = after_stat(norm)))
#
#   # Check if p is a ggplot object
#   expect_s3_class(p, "ggplot")
#
#   # Check the mapping at the layer level
#   layer_mapping <- p$layers[[1]]$mapping
#   expect_true("x" %in% names(layer_mapping))
#   expect_true("y" %in% names(layer_mapping))
#   expect_true("dx" %in% names(layer_mapping))
#   expect_true("dy" %in% names(layer_mapping))
#   expect_true("length" %in% names(layer_mapping))
# })
#
