# Create a Gradient Smoothed Field Layer

`geom_gradient_smooth()` creates a ggplot2 layer that visualizes the
gradient of a scalar field computed from raw data. A linear model is
fitted using the supplied `formula` (default:
`z ~ x + y + I(x^2) + I(y^2)`) on the raw data, and the numerical
gradient is computed using numDeriv::grad(). The computed gradient field
is then visualized using [`GeomStream()`](/reference/geom_stream.md).

## Usage

``` r
geom_gradient_smooth(
  mapping = NULL,
  data = NULL,
  stat = StatStreamField,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = TRUE,
  inherit.aes = TRUE,
  formula = z ~ x + y + I(x^2) + I(y^2),
  xlim = NULL,
  ylim = NULL,
  n = 11,
  max_it = 1000,
  T = NULL,
  L = NULL,
  center = TRUE,
  type = "vector",
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  grid = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = grid::arrow(angle = 30, length = grid::unit(0.02, "npc"), type = "closed")
)

stat_gradient_smooth(
  mapping = NULL,
  data = NULL,
  geom = GeomStream,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = TRUE,
  inherit.aes = TRUE,
  formula = z ~ x + y + I(x^2) + I(y^2),
  xlim = NULL,
  ylim = NULL,
  n = 11,
  max_it = 1000,
  T = NULL,
  L = NULL,
  center = TRUE,
  type = "vector",
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  grid = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = grid::arrow(angle = 30, length = grid::unit(0.02, "npc"), type = "closed")
)
```

## Arguments

- mapping:

  A set of aesthetic mappings created by
  [`ggplot2::aes()`](https://ggplot2.tidyverse.org/reference/aes.html).
  **Required:** Must include **`x`** and **`y`**; vector displacements
  are defined by **`fx`** and **`fy`**.

- data:

  A data frame containing the raw vector data.

- stat:

  The statistical transformation to use on the data. Defaults to
  `"vector_smooth"`.

- position:

  Position adjustment, either as a string or the result of a position
  adjustment function.

- ...:

  Additional arguments passed to the layer.

- na.rm:

  Logical. If `FALSE` (the default), missing values are removed with a
  warning.

- show.legend:

  Logical. Should this layer be included in the legends?

- inherit.aes:

  Logical. If `FALSE`, overrides the default aesthetics rather than
  combining with them.

- formula:

  A formula specifying the linear model for the scalar field. Defaults
  to `z ~ x + y + I(x^2) + I(y^2)`.

- xlim:

  Numeric vector of length 2 specifying the domain limits in the
  \\x\\-direction. Defaults to \\c(-1, 1)\\.

- ylim:

  Numeric vector of length 2 specifying the domain limits in the
  \\y\\-direction. Defaults to \\c(-1, 1)\\.

- n:

  An integer vector specifying the grid resolution for smoothing.

- max_it:

  Maximum number of iterations for field integration (when used in
  streamlines).

- T:

  If `normalize = FALSE`, this controls the time length for growing
  streams.

- L:

  If `normalize = TRUE`, this controls the fixed length of streams or
  vectors.

- center:

  Logical. If `TRUE`, the vector is recentered so that the original
  `(x, y)` becomes the midpoint (default is `TRUE` for
  [`geom_vector()`](/reference/geom_vector.md) and `FALSE` for
  [`geom_vector2()`](/reference/geom_vector.md)).

- type:

  Character. Either `"stream"` (default) or `"vector"`. `"stream"`
  computes a full streamline by integrating in both directions (if
  `center = TRUE`), while `"vector"` computes a single vector.

- normalize:

  Logical. If `TRUE`, the vector endpoints are scaled to unit length
  before being scaled by `L` (default: `TRUE`).

- tail_point:

  Logical. If `TRUE`, draws the tail point of vectors/streams (default:
  `FALSE`).

- eval_point:

  Logical. If `TRUE`, marks the evaluation points used to fit gradients.

- grid:

  A user-supplied data frame or pattern (e.g., "hex") for specifying
  custom evaluation points.

- lineend:

  Line end style (round, butt, square).

- linejoin:

  Line join style (round, mitre, bevel).

- linemitre:

  Line mitre limit (number greater than 1).

- arrow:

  An optional [`grid::arrow()`](https://rdrr.io/r/grid/arrow.html)
  specification to add arrowheads to the vectors (default:
  `grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")`).

- geom:

  The geometric object used to render the streamline (only used in
  [`stat_stream()`](/reference/geom_stream.md); defaults to
  [GeomStream](/reference/geom_stream.md)).

## Value

A ggplot2 layer that can be added to a ggplot object.

## Aesthetics

`geom_gradient_smooth()` supports the following aesthetics (required
aesthetics are in **bold**):

- **`x`**: The x-coordinate of the data point.

- **`y`**: The y-coordinate of the data point.

- **`z`**: The scalar value used for computing the gradient.

- `color`: The color used for the gradient vectors. Defaults depend on
  the selected `type`.

## Details

**Gradient Calculation:** A linear model is fitted using the provided
`formula` and the raw data. The scalar field defined by the model is
then differentiated numerically with
[`numDeriv::grad()`](https://rdrr.io/pkg/numDeriv/man/grad.html) to
yield gradient vectors.

**Visualization:** The resulting gradient field is visualized using
[`GeomStream()`](/reference/geom_stream.md). Since `z` is only used
internally, it is dropped from the final visual output.

## Examples

``` r
if (FALSE) { # \dontrun{
# Define several scalar field functions:

# Example 1: f(x, y) = x^2 - y^2
f <- function(u) {
  x <- u[1]
  y <- u[2]
  x^2 - y^2
}

# Example 2: g(x, y) = sin(x) * cos(y)
g <- function(u) {
  x <- u[1]
  y <- u[2]
  sin(x) * cos(y)
}

# Example 3: h(x, y) = log(|x| + 1) + sqrt(|y|)
h <- function(u) {
  x <- u[1]
  y <- u[2]
  log(abs(x) + 1) + sqrt(abs(y))
}

# Create a grid of evaluation points
grid_data <- expand.grid(
  x = seq(-5, 5, length.out = 30),
  y = seq(-5, 5, length.out = 30)
)

# Compute the scalar field for f and plot its gradient
grid_data$z <- apply(grid_data, 1, f)

ggplot(grid_data, aes(x = x, y = y, z = z)) +
  geom_gradient_smooth()

# Compute and plot for g:
grid_data$z <- apply(grid_data, 1, g)
ggplot(grid_data, aes(x = x, y = y, z = z)) +
  geom_gradient_smooth()

# Compute and plot for h:
grid_data$z <- apply(grid_data, 1, h)
ggplot(grid_data, aes(x = x, y = y, z = z)) +
  geom_gradient_smooth()
} # }
```
