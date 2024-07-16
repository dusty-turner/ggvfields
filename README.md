
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **ggvfields**

## Overview

**ggvfields** provides tools for visualizing vector fields, complex
numbers, and soon dual numbers and more.

``` r
library("ggvfields")
#> Loading required package: ggplot2
options(ggplot2.continuous.colur="viridis")
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
  x <- v[1]; y <- v[2]
  c(-y, x) # = f(x,y)
}

# create a ggplot with the vector field layer
ggplot() +
  geom_vector_field(fun = f, xlim = c(-10, 10), ylim = c(-10, 10)) +
  coord_fixed()
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />

By default, the magnitude (or length/norm) of the vector lengths is
mapped to the color aesthetic.

The norm of a vector $\mathbf{w} = (u, v)$ is given by:

$|\mathbf{w}| = \sqrt{u^2 + v^2}$

This function also offers several other aesthetic mappings which allow
you to map several characteristics of the vector field to different
aesthetic mappings.

#### Divergence

Divergence is an operation on a vector field that tells us how the field
behaves toward or away from a point. Locally, the divergence of a vector
field $\mathbf{F}$ in $\mathbb{R}^2$ at a particular point is a measure
of the “outflowing-ness” of the vector field at that particular point.
The divergence of a vector field results in a scalar function.

If $\mathbf{F} = \langle \mathbf{F}_x(x,y), \mathbf{F}_y(x,y) \rangle$
is a vector field in $\mathbb{R}^2$, then the divergence of $\mathbf{F}$
is defined by:

$$
\text{div} \, \mathbf{F} = \frac{\partial \mathbf{F}_x}{\partial x} + \frac{\partial \mathbf{F}_y}{\partial y}
$$

To visualize the divergence of the vector field:

``` r
ggplot() +
  geom_vector_field(
    aes(color = after_stat(divergence)), 
    fun = f, xlim = c(-10, 10), ylim = c(-10, 10)
  ) +
  coord_fixed()
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

#### Curl

Curl is an operation on a vector field that measures the rotation or
swirling strength at a point. In two dimensions, the curl of a vector
field $\mathbf{F}$ in $\mathbb{R}^2$ is a scalar value that indicates
how much the vector field tends to rotate around that point.

If $\mathbf{F} = \langle \mathbf{F}_x(x,y), \mathbf{F}_y(x,y) \rangle$
is a vector field in $\mathbb{R}^2$, then the curl of $\mathbf{F}$ is
defined by:

$$
\text{curl} \, \mathbf{F} = \frac{\partial \mathbf{F}_y}{\partial x} - \frac{\partial \mathbf{F}_x}{\partial y}
$$

To visualize the the curl:

``` r
ggplot() +
  geom_vector_field(
    aes(color = after_stat(curl)), 
    fun = f, xlim = c(-10, 10), ylim = c(-10, 10)
  ) +
  coord_fixed()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

#### Laplace Operator

The Laplace operator (or Laplacian) measures the rate at which the
average value of a function around a point differs from the value at
that point. It is commonly used to understand how a scalar field spreads
out or compresses in space.

If $\mathbf{F} = \langle \mathbf{F}_x(x,y), \mathbf{F}_y(x,y) \rangle$
is a vector field in $\mathbb{R}^2$, then the Laplacian of $\mathbf{F}$
is defined by taking the sum of the second partial derivatives of each
component of the vector field.

The Laplacian of the vector field $\mathbf{F}$ is thus given by:

$$
\Delta \mathbf{F} = \frac{\partial^2 \mathbf{F}_x}{\partial x^2} + \frac{\partial^2 \mathbf{F}_x}{\partial y^2} + \frac{\partial^2 \mathbf{F}_y}{\partial x^2} + \frac{\partial^2 \mathbf{F}_y}{\partial y^2}
$$

This results in a scalar value that describes how the vector field
spreads out or compresses at different points in the field.

To visualize the Laplace operator of the vector field, lets look at a
different vector field:

``` r
g <- function(v) {
  x <- v[1]
  y <- v[2]
  c(-sin(y), cos(x))
}

ggplot() +
  geom_vector_field(
    aes(color = after_stat(laplacian)), n = 20,
    fun = g, xlim = c(-2*pi, 2*pi), ylim = c(-2*pi, 2*pi)
  ) +
  coord_fixed()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

#### Directional Derivative

The directional derivative of a vector field measures the rate of change
of the field in a specified direction. It provides insight into how the
field varies as you move in a particular direction.

If $\mathbf{F} = \langle \mathbf{F}_x(x,y), \mathbf{F}_y(x,y) \rangle$
is a vector field in $\mathbb{R}^2$, and
$\mathbf{v} = \langle v_x, v_y \rangle$ is a direction vector, then the
directional derivative of $\mathbf{F}$ in the direction of $\mathbf{v}$
is given by:

$$
D_{\mathbf{v}} \mathbf{F} = \frac{\partial \mathbf{F}_x}{\partial x} v_x + \frac{\partial \mathbf{F}_x}{\partial y} v_y + \frac{\partial \mathbf{F}_y}{\partial x} v_x + \frac{\partial \mathbf{F}_y}{\partial y} v_y
$$

For a vector field, this represents the rate of change of each component
of the field in the direction specified by $\mathbf{v}$.

To visualize the Laplace operator of the vector field:

``` r
vec <- c(5, 6)

ggplot() +
  geom_vector_field(
    aes(color = after_stat(directional_derivative)),
    fun = f, xlim = c(-10, 10), ylim = c(-10, 10),
    v = vec) +
  geom_point(aes(x = vec[1], y = vec[2])) +
  coord_fixed()
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

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

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

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

## Related projects

For creating vector fields, the
[**ggquiver**](http://pkg.mitchelloharawild.com/ggquiver/) package
provides quiver plots to visualize vector fields, while the
[**ggarchery**](https://github.com/mdhall272/ggarchery) package can
handling of segments with arrowheads.
