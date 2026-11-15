# ggvfields 1.1.0

## Changes in behavior

* Streams are now computed with a Runge-Kutta 4 integrator by default
  (`method = "rk4"`), in `geom_stream_field()` and its relatives as well as
  `geom_gradient_field(type = "stream")`, `geom_stream_smooth()` and
  `geom_gradient_smooth()`. Streams follow the field more closely, which
  changes `avg_spd` slightly. Streams stop at sinks and at singularities such
  as point charges, and cross jumps in the field, such as one defined with
  `sign()` or `ifelse()`. The adaptive deSolve integrator remains available
  as `method = "lsoda"`, and `"euler"` is also available. An unknown `method`
  is an error.
* `geom_stream_field2()` and `stat_stream_field2()` now grow every stream to
  length `L` and then crop all of them back to the time of the fastest one,
  so a stream's length shows its speed relative to the fastest, as the
  vectors of `geom_vector_field2()` do. `type = "vector"` is unchanged.
* Mapped lengths are proportional to the mapped value: in
  `geom_vector2()`, `geom_vector_field2()` and `geom_gradient_field2(type =
  "vector")`, a vector with half the largest norm is drawn half as long. The
  longest vector is `max_range` centimeters (0.5 by default; set it with
  `scale_length_continuous()`), and the legend shows the same lengths.
* The ellipses and wedges of `geom_vector_smooth(method = "gam")`, the
  default, are now prediction intervals for a new observation at each
  location, as for `method = "lm"` and `method = "kriging"`: they include
  each component's residual variance as well as the variance of the fit.
  They are larger than before.
* The wedges of `geom_vector_smooth(pi_type = "wedge")` are computed from the
  exact distribution of the angle of the predicted vector tip, so they cover
  the stated `conf_level` however large the uncertainty, and are asymmetric
  when the uncertainty is. They are wider than before where the uncertainty is
  large.
* Supplying `formula` with the default `method = "gam"` in
  `geom_stream_smooth()`, `stat_stream_smooth()`, `geom_vector_smooth()` or
  `stat_vector_smooth()` now gives a warning, since the gam smoothers fit
  `fx` and `fy` with `s(x, y)`. Use `method = "lm"` to fit a formula.
* The default of `conf_level` in `geom_vector_smooth()` is now `0.95`. Results
  are the same.

## New features and improvements

* `geom_stream_smooth()`, `geom_vector_smooth()` and their stats work with
  small data sets: with fewer than 30 distinct locations the gam smoother
  uses half as many basis functions as locations. At least 5 locations are
  needed; with fewer, the error suggests `method = "lm"`.
* With a supplied `grid` data frame, the default length `L` of vectors and
  streams is 0.85 times the grid's spacing (the median distance between
  neighboring points), so dense grids do not overplot. Regular grids get the
  same length as before.
* `geom_vector_smooth()` and `stat_vector_smooth()` choose the interval style
  by default (`pi_type = NULL`): ellipses at `eval_points` and wedges on a
  grid, without a message. They also gain `xlim` and `ylim` to set the range
  of the grid, which defaults to the range of the data.
* `geom_vector_smooth(method = "kriging")` estimates the range of its
  variogram from the data and works in standardized coordinates, so the
  smoothed field does not depend on the units of `x` and `y`. The fitted
  coregionalization is kept positive definite.
* `stat_vector_smooth()` is exported.
* `StatStreamSmooth` and `StatGradientSmooth` are the stats behind
  `geom_stream_smooth()` and `geom_gradient_smooth()`.
* `stat_vector()` maps `color = after_stat(norm)` by default, like
  `geom_vector()`.
* `geom_gradient_field2(type = "vector")` and `stat_gradient_field2(type =
  "vector")` map `length = after_stat(norm)`, so vectors are scaled by the
  size of the gradient, like `geom_vector_field2()`. Streams are unchanged.
* Layers computed from a function (`stat_stream_field()`,
  `geom_gradient_field()`, `geom_gradient_field2()`, `geom_potential()` and
  their `stat_*()` versions) default to `inherit.aes = FALSE`, like
  `geom_vector_field()` and `geom_stream_field()`, so they can be added to a
  plot that has its own data, for example to draw a field behind observed
  vectors.
* `stat_stream_field()` passes `normalize` and `stat_stream_field2()` passes
  `tol` through to the computation, as `geom_stream_field()` and
  `geom_stream_field2()` do.
* `efield_maker()` passes `k` and `q_test` to the field it returns, and the
  field keeps the argument values `efield_maker()` was called with, so fields
  can be made in a loop.
* `linetype` applies to vector layers such as `geom_vector()` and
  `geom_vector_field()`.

## Stream speeds (`avg_spd`)

* `avg_spd`, the default color of streams, is the length of the drawn stream
  divided by the time it took, in every mode:
  - streams run to a time `T`, and streams with `normalize = FALSE` that are
    cropped to the time of the fastest stream, use that shared time, so
    color and length agree;
  - a stream run to a length `L` that stops short of it at a sink would take
    unbounded time to reach the sink, so its speed is measured up to 99% of
    the length it achieved. This makes it the same for every integrator.

## Bug fixes

* `geom_gradient_smooth()` keeps a color mapping supplied in `mapping`, and
  every field layer, including `geom_gradient_field2()`, gives the same error
  for an unknown `type`.

* `geom_stream_smooth()`, `geom_gradient_smooth()` and `geom_vector_smooth()`
  always use the stat that fits their model; a different `stat` is ignored
  with a warning.

* Centered streams (`center = TRUE`, the default) are drawn when a stream can
  move only one way from its seed, for example on the edge of the domain
  where the field is defined.
* Stream layers skip seed points where the field is undefined, for example a
  grid point on a charge with `efield_maker()`, and draw the remaining
  streams.
* `geom_stream()` gives the same message as `geom_path()` when every group
  has a single observation.
* `geom_stream_smooth()` and `stat_stream_smooth()` accept fixed aesthetics
  such as `color = "red"` or `linewidth = 2` without a warning, and respect
  `colour = "red"`.

## Performance

* `geom_stream_smooth()` and `geom_gradient_smooth()` fit their model once per
  group rather than at every evaluation of the field, which is much faster.
* Computing streams with `method = "lsoda"` is about twice as fast.

## Documentation

* The help pages document the defaults of `pi_type` in `geom_vector_smooth()`
  (ellipses at `eval_points`, wedges on a grid) and of `n` in
  `geom_potential()`, whose `fun` returns a vector.
* The smoothers are documented as using a gam by default, with `formula`
  applying to `method = "lm"`.
* Angles in `geom_stream_smooth()` are documented as radians, and the
  `geom_vector_smooth()` examples are clarified.
* Examples are run when the package is checked.
* The pkgdown site is at <https://dustysturner.com/ggvfields/>.

## Internal

* sp and gstat are suggested rather than imported, since only
  `method = "kriging"` in `geom_vector_smooth()` needs them; you are asked to
  install them the first time you use it. farver is no longer a dependency,
  and rlang is declared.
* The package uses only exported ggplot2 functions.
* Each `geom_*()` / `stat_*()` pair is built by one shared internal
  constructor, and a test checks that both members of a pair pass the same
  parameters and default mappings.
* Errors are raised with cli throughout.
* Added tests for the changes above, visual snapshot tests with vdiffr (now
  in Suggests; CRAN skips them), and an R CMD check workflow.

# ggvfields 1.0.1
* Fixed the `length` legend for `geom_vector_field2()` (and the related
  stream geoms): `GeomStream` now uses a custom `draw_key` that dispatches
  to `draw_key_length()` when the `length` aesthetic is mapped, so the
  legend relating segment lengths to vector norms is drawn again.
* Corrected the handling of the time variable `t` in `geom_stream()`.
* Fixed the draw order of grobs in the stream and vector geoms and
  suppressed spurious output from the kriging step.
* Fixed handling of the `linewidth` argument.
* Added an option to choose between `gam` and `lm` when smoothing.

# ggvfields 1.0.0

* Initial CRAN submission.
