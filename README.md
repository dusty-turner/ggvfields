
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggvfields

## Overview

ggvfields provides tools for visualizing vector fields, complex numbers,
and soon dual numbers and more.

## Installation

You can install the development version from GitHub:

``` r
devtools::install_github("dusty-turner/ggvfields")
```

``` r
library(ggvfields)
```

## Usage

### `geom_vector_field`

The `geom_vector_field` function generates a vector field plot layer
using a user-defined function to compute the vector components. This
function abstracts away the mathematical computations required to
generate the vector field, so the user does not need to manually
calculate and input the vector components into `geom_segment`. It
simplifies the process, making it easier to create vector field
visualizations without dealing with the underlying math.

``` r
library(ggplot2)

# custom function
f <- function(x, y) {
  u <- -y
  v <- x
  list(u, v)
}

# Create a ggplot with the vector field layer
ggplot() +
  geom_vector_field(fun = f, xlim = c(-10, 10), ylim = c(-10, 10), n = 20) +
  coord_fixed()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

### `geom_complex_vector_field`

The geom_vector_field function generates a vector field plot layer using
a user-defined function to compute the vector components. This function
abstracts away the mathematical computations required to generate the
vector field, so the user does not need to manually calculate and input
the vector components into geom_segment. It simplifies the process,
making it easier to create vector field visualizations without dealing
with the underlying math.

``` r
# custom function
f <- function(z) (z^2 + 1) / (z^2 - 1)

# Create a ggplot with the complex vector field layer
ggplot() +
  geom_complex_vector_field(fun = f, relim = c(-2, 2), imlim = c(-2, 2), n = 100) +
  labs(x = "Real", y = "Imaginary") +
  coord_fixed()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

## License

This package is licensed under the MIT License.

## Contact

For any questions or issues, please open an issue on GitHub or contact
the maintainer.
