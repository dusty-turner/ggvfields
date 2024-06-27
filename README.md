
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggvfields

## Overview

ggvfields provides tools for visualizing vector fields, complex numbers,
dual numbers, and more. The package is designed to be user-friendly and
extendable, making it easy to create detailed and informative plots for
a variety of mathematical constructs.

## Installation

You can install the development version from GitHub:

``` r
devtools::install_github("dusty-turner/ggvfields")
```

## Usage

### `geom_vector_field`

The `geom_vector_field` function also generates a vector field plot
layer using a user-defined function to compute the vector components.
This function can be used similarly to `stat_vector_field` but provides
more flexibility in terms of ggplot2 layering and customization.

``` r
# Create a ggplot with the vector field layer
ggplot() +
  geom_vector_field(fun = f, xlim = c(-10, 10), ylim = c(-10, 10), n = 20)
```

## License

This package is licensed under the MIT License.

## Contact

For any questions or issues, please open an issue on GitHub or contact
the maintainer.
