# Create a Continuous Scale for Vector Length

`scale_length_continuous()` provides a continuous scale for controlling
the length aesthetic in a ggplot. This is particularly useful when
working with vector plots where vector lengths are mapped to a
continuous scale.

## Usage

``` r
scale_length_continuous(max_range = 0.5, ...)
```

## Arguments

- max_range:

  The maximum value to which the input is rescaled. Numeric scalar
  specifying the upper bound of the output range. Should be between 0
  and 1.

- ...:

  Other arguments passed to
  [`continuous_scale()`](https://ggplot2.tidyverse.org/reference/continuous_scale.html).

## Value

If `max_range` is less than or equal to 0.5 (the default), a continuous
scale object (typically of class `"ScaleContinuous"`) mapping the
`length` aesthetic is returned. If `max_range` is greater than 0.5, a
list is returned with two components:

- the continuous scale object, and

- a theme modification (a `theme` object) that adjusts the legend key
  width based on the value of `max_range`.
