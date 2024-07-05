
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **ggvfields**

## Overview

**ggvfields** provides tools for visualizing vector fields, complex
numbers, and soon dual numbers and more.

``` r
library("ggvfields")
#> Loading required package: ggplot2
```

## Usage

### `geom_vector_field()`

The `geom_vector_field()` function generates a vector field plot layer
using a user-defined function to compute the vector components. This
function abstracts away the mathematical computations required to
generate the vector field, so the user does not need to manually
calculate and input the vector components into `geom_segment()`. It
simplifies the process, making it easier to create vector field
visualizations without dealing with the underlying math.

``` r
# custom function
f <- function(v) {
    x <- v[1]
    y <- v[2]
    u <- unname(-y)
    v <- unname(x)
    c(u, v)
}

# create a ggplot with the vector field layer
ggplot() +
  geom_vector_field(fun = f, xlim = c(-10, 10), ylim = c(-10, 10), n = 20, center = TRUE, normalize = TRUE) +
  coord_fixed()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

This function also offers several aesthetic mappings which allow you to
map several characteristics of the vector field to different aesthetic
mappings.

#### Vector Magnitude

To visualize the magnitude of the vectors:

``` r
ggplot() +
  geom_vector_field(aes(color = after_stat(magnitude)),
                    fun = f, xlim = c(-10, 10), ylim = c(-10, 10), n = 21, center = TRUE, normalize = TRUE
                    ) +
  coord_fixed()
#> Warning: Removed 1 rows containing missing values (`geom_vector_field()`).
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

#### Divergence

To visualize the divergence of the vector field:

``` r
ggplot() +
  geom_vector_field(aes(color = after_stat(divergence)), 
                    fun = f, xlim = c(-10, 10), ylim = c(-10, 10), n = 21, center = TRUE, normalize = TRUE
                    ) +
  coord_fixed()
#> Warning: Removed 1 rows containing missing values (`geom_vector_field()`).
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

#### Magnitude of the Curl

To visualize the magnitude of the curl of the vector field:

``` r
ggplot() +
  geom_vector_field(aes(color = after_stat(curl)), 
                    fun = f, xlim = c(-10, 10), ylim = c(-10, 10), n = 21, center = TRUE, normalize = TRUE 
                    ) +
  coord_fixed()
#> Warning: Removed 1 rows containing missing values (`geom_vector_field()`).
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

#### Laplace Operator

To visualize the Laplace operator of the vector field:

``` r
ggplot() +
  geom_vector_field(aes(color = after_stat(laplacian)), 
                    fun = f, xlim = c(-10, 10), ylim = c(-10, 10), n = 21, center = TRUE, normalize = TRUE 
                    ) +
  coord_fixed()
#> Warning: Removed 1 rows containing missing values (`geom_vector_field()`).
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

#### Directional Derivative

To visualize the directional derivative from any point to another point
`c(u, v)` within the vector field:

``` r
x1 <- 5
y1 <- 6

ggplot() +
  geom_vector_field(aes(color = after_stat(directional_derivative)),
                    fun = f, xlim = c(-10, 10), ylim = c(-10, 10), n = 20, center = TRUE, normalize = TRUE, 
                    u = x1, v = y1) +
  geom_point(aes(x = x1, y = y1)) +
  scale_color_viridis_c() +
  coord_fixed()
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

### `geom_complex_function()`

The `geom_complex_function()` function generates a vector field plot
layer using a user-defined function to compute the vector components.
This function abstracts away the mathematical computations required to
generate the vector field, so the user does not need to manually
calculate and input the vector components into `geom_segment()`. It
simplifies the process, making it easier to create vector field
visualizations without dealing with the underlying math.

``` r
# custom function
f <- function(z) (z^2 + 1) / (z^2 - 1)

# create a ggplot with the complex vector field layer
ggplot() +
  geom_complex_function(fun = f, relim = c(-2, 2), imlim = c(-2, 2), n = 100) +
  coord_fixed()
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

## License

This package is licensed under the MIT License.

## Contact

For any questions or issues, please [open an
issue](https://github.com/dusty-turner/ggvfields/issues/new) on GitHub
or contact the maintainer.

## Installation

You can install the development version from GitHub:

``` r
devtools::install_github("dusty-turner/ggvfields")
```
