
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **ggvfields**

## Overview

**ggvfields** provides tools for visualizing vector fields, stream
plots, and soon complex numbers and more.

``` r
remotes::install_github("dusty-turner/ggvfields")
```

``` r
library("ggvfields")
#> Loading required package: ggplot2
options(ggplot2.continuous.colour="viridis")
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
f <- function(v) {
  x <- v[1]; y <- v[2]
  c(-y, x) # = f(x,y)
}

ggplot() +
  geom_vector_field(fun = f, xlim = c(-10, 10), ylim = c(-10, 10)) +
  coord_fixed() 
```

<img src="man/figures/README-unnamed-chunk-4-1.png" width="100%" />

This function allows the user to map several characteristics of the
vector field to different aesthetic mappings.

#### Norm

The norm of a vector $\mathbf{w} = (u, v)$ is given by:

$|\mathbf{w}| = \sqrt{u^2 + v^2}$

We can visualize the norm by mapping the value of the norm to the color
aesthetic.

``` r
ggplot() +
  geom_vector_field(
    aes(color = after_stat(norm)),
    fun = f, xlim = c(-10, 10), ylim = c(-10, 10)) +
  coord_fixed()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

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

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

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

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

#### Aesthetics In Development

Another aesthetic in development available for mapping is length. The
length aesthetic maps the value of the computed measure to the length of
the vector.

``` r
ggplot() +
  geom_vector_field(
    aes(length = after_stat(norm), color = after_stat(norm)),
    fun = f, xlim = c(-10, 10), ylim = c(-10, 10)) +
  coord_fixed() +
  theme(legend.box = "horizontal")
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

### `geom_vector()`

The `geom_vector()` function generates a plot layer that visualizes
vectors as line segments with optional arrowheads. It is designed to
help you visualize directional data, such as wind directions, gradients,
or flow fields. The vectors are defined by their start (`x`, `y`) and
vector components (`dx`, `dy`) coordinates, or alternatively by their
angular (`angle`) and distance (`distance`) components.

#### Cartesian

``` r
set.seed(1234)
n <- 10
wind_data_polar <- data.frame(
  lon = rnorm(n), 
  lat = rnorm(n), 
  wind_dir = runif(n, -pi, pi),
  wind_spd = rchisq(n, df = 2)
)

wind_data_cartesian <- within(wind_data_polar, {
  wind_lon_comp <- wind_spd * cos(wind_dir)
  wind_lat_comp <- wind_spd * sin(wind_dir)
  dx <- wind_lon_comp
  dy <- wind_lat_comp
})

wind_data_cartesian |> 
  ggplot() +
  geom_vector(aes(x = lon, y = lat, dx = dx, dy = dy), 
              add_points = TRUE, normalize = TRUE, center = TRUE) +
  labs(title = "Wind Vectors (Cartesian Input)",
       x = "Longitude", y = "Latitude")  +
  lims(x = c(-3,2), y = c(-2,3))
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

#### Polar

``` r
wind_data_polar |> 
  ggplot() +
  geom_vector(aes(x = lon, y = lat, angle = wind_dir, distance = wind_spd),
              add_points = TRUE, normalize = TRUE, center = TRUE) +
  labs(title = "Wind Vectors (Polar Input)",
       x = "Longitude", y = "Latitude") +
  lims(x = c(-3,2), y = c(-2,3))
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

#### Map Norm to Length Aesthetic

``` r
wind_data_cartesian |> 
  ggplot() +
  geom_vector(aes(x = lon, y = lat, dx = dx, dy = dy, length = after_stat(norm)), 
              add_points = TRUE, center = TRUE) +
  labs(title = "Wind Vectors (Cartesian Input)",
       x = "Longitude", y = "Latitude")  +
  lims(x = c(-3,2), y = c(-2,3)) +
  scale_length_continuous()
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

### `geom_streamplot()`

The `geom_streamplot()` function generates a stream plot layer of a
user-defined vector field function. The lines in the plot represent the
flow of data points through the vector field.

``` r
f <- function(v) {
  x <- v[1]
  y <- v[2]
  c(-1 - x^2 + y, 1 + x - y^2)
}

ggplot() +
  geom_streamplot(
    fun = f, xlim = c(-3, 3), ylim = c(-3, 3),
    ) +
  coord_fixed() +
  theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

The `chop` parameter (defaulted to TRUE) allows you to chop the
trajectories into segments. This can be useful for better visualization
of the streamlines when they are long and complex.

It may be useful to not break up the streamlines.

``` r
ggplot() +
  geom_streamplot(
    fun = f, xlim = c(-3, 3), ylim = c(-3, 3),
    chop = FALSE
    ) +
  coord_fixed() +
  theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

It may also be useful to break up the streamlines into more segments.
The `scale_stream` parameter (defaults to 1) adjusts the segmentation of
streamlines by specifying the proportion of the streamline length used
to divide it into smaller segments.

``` r
ggplot() +
  geom_streamplot(
    fun = f, xlim = c(-3, 3), ylim = c(-3, 3),
    chop = TRUE, scale_stream = .9,
    ) +
  coord_fixed() +
  theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" />

### Map Calculus Measures to Aesthetics

``` r
ggplot() +
  geom_streamplot(
    fun = f, xlim = c(-3, 3), ylim = c(-3, 3),
    aes(color = after_stat(log(divergence + 10)))
    ) +
  coord_fixed() +
  theme_minimal() 
#> Warning in log(divergence + 10): NaNs produced
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="100%" />

### Animate `geom_streamplot()`

``` r
# Create stream plot with rownum aesthetic
p <- ggplot() +
 geom_streamplot(
   aes(rownum = after_stat(rownum)), 
    fun = f, xlim = c(-3, 3), ylim = c(-3, 3),
 ) +
 coord_fixed() +
 theme_bw()
# Create an animation transition plot
anim <- animation_transition(plot = p) +   
 gganimate::transition_reveal(rownum) +
 gganimate::ease_aes('linear')

# Animate the plot
gganimate::animate(anim, nframes = 25, fps = 5, end_pause = 0, renderer = gganimate::gifski_renderer())
```

<img src="man/figures/README-animation-1.gif" width="100%" />

### `geom_flow()`

The `geom_flow()` function generates a flow plot layer for a
user-defined vector field function. The lines in the plot represent the
flow of data points through the vector field, visualizing the trajectory
of particles over time. Each flow line traces where a “marble” would
move through the vector field if dropped at a specific starting point,
making this an intuitive way to visualize dynamic systems.

By default, the color of each flow line corresponds to time (`t`),
meaning the color transitions along the path represent the progression
of time. As the flow line evolves, it shows how a particle would move
over time if following the vector field. You can change the coloring by
mapping aesthetics to other computed measures if needed, but time
remains the default.

Flows are computed using the `deSolve` package’s ODE solver, with the
`rk4` method (a fourth-order Runge-Kutta method) used for numerical
integration. This solver ensures accurate and efficient computation of
flow lines, abstracting away complex calculations for the user.

``` r
ggplot() +
  geom_flow(
    fun = f, xlim = c(-10, 10), ylim = c(-10, 10)
    ) +
  coord_fixed() +
  theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" />

In this example, flow lines evolve according to the vector field defined
by `f`. The color along each line will show how the particle moves over
time (`t`) within the vector field.

#### Adaptive Parameters

Several parameters in `geom_flow()` are adaptive, meaning they adjust
automatically based on the characteristics of the vector field and the
plot limits. These adaptive parameters help optimize the flow
visualization without requiring manual tuning:

- **`threshold_distance`**: This parameter controls the minimum distance
  between adjacent flow lines to prevent them from overlapping. If not
  specified, it is calculated automatically as half the Euclidean
  distance between adjacent grid points. This ensures the plot remains
  uncluttered, with flow lines spaced appropriately based on the grid
  dimensions (`n`) and the axis limits (`xlim`, `ylim`).

- **`T`**: This parameter represents the total time span for the ODE
  solver to trace the flow paths. If `T` is `NULL`, it is automatically
  computed by starting from the center of the plot and estimating how
  long it would take a particle to travel from the center to the
  farthest boundary of the vector field - assuming that the vector field
  is not cyclic nor does it reach a sync. This ensures that the
  trajectories capture the significant dynamics of the vector field
  without extending unnecessarily.

- **`iterations`**: This parameter defines the number of time steps for
  the ODE solver to use when tracing the flow lines. A higher number of
  iterations results in smoother and more detailed flows. If
  `iterations` is left as `NULL`, it is computed adaptively based on the
  value of `T`, ensuring that longer time spans result in more
  iterations for smoother paths.

These adaptive parameters allow `geom_flow()` to create a well-balanced
plot by dynamically adjusting the precision and spacing of flow lines,
based on the underlying vector field and plot limits.

#### Example with Custom Parameters

Below is an example where we customize the grid size, time span (`T`),
number of iterations, and the threshold distance between flow lines:

``` r
ggplot() +
  geom_flow(
    fun = f, n = c(21, 21), xlim = c(-10, 10), ylim = c(-10, 10),
    iterations = 1000, threshold_distance = 0.5, T = 5) +
  coord_fixed() +
  theme_minimal()
```

<img src="man/figures/README-unnamed-chunk-17-1.png" width="100%" />

<!-- The `mask_shape_type` parameter allows you to specify the mask shape used for streamline generation which influences how the streamlines are placed and how closely they can approach each other. The default mask shape is `"square"`, but you can also use `"diamond"`, `"inset_square"`, or `"circle"`.  During streamline generation, when a streamline enters the specified shape, no other streamlines will enter that region.  -->
<!-- - **Square Mask (default)**: Streamlines are restricted to a grid where each cell is a square. This generally results in evenly spaced streamlines. -->
<!-- - **Diamond Mask**: Streamlines are restricted to a square grid with diamonds inset within each square.  This can create a more dense pattern which can have better visualizations for some functions. - **Inset Square Mask**: Streamlines are restricted to a grid with smaller squares inset within larger squares. This can create a denser and more detailed pattern of streamlines. -->
<!-- - **Diamond Mask**: Streamlines are restricted to a square grid with diamonds inset within each square.  This can create a more dense pattern which can have better visualizations for some functions. - **Circle Mask**: Streamlines are restricted to a grid with inset circles inside the square grid. -->
<!-- ```{r} -->
<!-- ggplot() + -->
<!--   geom_streamplot(aes(group = after_stat(id)), -->
<!--                   fun = f, xlim = c(-3, 3), ylim = c(-3, 3), max_length = 10000, -->
<!--                   max_steps = 10000, ds = .05, min_dist = .25,  -->
<!--                   mask_shape_type = "diamond") + -->
<!--   coord_fixed() + -->
<!--   theme_minimal() -->
<!-- ``` -->

### `geom_complex_function()`

The `geom_complex_function()` function generates a vector field plot
layer using a user-defined function to compute the vector components.
This function abstracts away the mathematical computations required to
generate the vector field, so the user does not need to manually
calculate and input the vector components into `geom_segment()`. It
simplifies the process, making it easier to create vector field
visualizations without dealing with the underlying math.

``` r
f <- function(z) (z^2 + 1) / (z^2 - 1)

ggplot() +
  geom_complex_function(fun = f, relim = c(-2, 2), imlim = c(-2, 2), n = 100) +
  labs(x = "Real", y = "Imaginary") +
  coord_fixed() +
  theme(legend.box = "horizontal")
```

<img src="man/figures/README-unnamed-chunk-18-1.png" width="100%" />

We can enhance this plot with a little help from biscale.

Using biscale, we can apply a bivariate color scale to the plot, which
allows us to represent two variables—angle (direction) and magnitude
(intensity)—simultaneously. This makes it easier to visualize how these
properties change across the field.

``` r
library(biscale)
library(cowplot)

plot <-
  ggplot() +
  geom_complex_function(aes(fill = after_stat(bi_class)),
    fun = f, relim = c(-2, 2), imlim = c(-2, 2), n = 100
    ) +
    bi_scale_fill(pal = "DkBlue") +
    labs(
      title = "",
      x = "Real (Re)",
      y = "Imaginary (Im)"
    ) +
    bi_theme(base_size = 16) +
    theme(legend.position = "none") +
  coord_fixed() 
  
legend <- bi_legend(pal = "DkBlue",
                    xlab = "Angle",
                    ylab = "Magnitude",
                    size = 6)

ggdraw() +
  draw_plot(plot, 0, 0, .8, 1) +  
  draw_plot(legend, x = .55, y = .6, width = .3, height = 0.3)
```

<img src="man/figures/README-unnamed-chunk-19-1.png" width="100%" />

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
