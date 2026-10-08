# ggvfields (development version)

## Changes in behavior

* `method` in `geom_stream_field()`, `stat_stream_field()`,
  `geom_stream_field2()` and `stat_stream_field2()` now selects the integrator.
  It was documented (`"rk4"` by default, `"euler"` available) but ignored, and
  streams were always computed with the adaptive `lsoda` solver from deSolve.
  `"rk4"` and `"euler"` are now real fixed-step integrators: each step covers
  an equal share of the arc length `L`, or of the time `T` when streams are
  run to a time. The previous behavior is available as `method = "lsoda"`.
  Because `"rk4"` is the default, default stream plots are computed
  differently from 1.0.1; they look the same, are slightly more accurate, and
  each stream is made of more points. An unknown `method` is now an error.
* Supplying `formula` to `geom_stream_smooth()`, `stat_stream_smooth()`,
  `geom_vector_smooth()` or `stat_vector_smooth()` with the default
  `method = "gam"` now warns. The gam smoothers always fit `fx` and `fy` with
  `s(x, y)`, so the formula was silently ignored. Use `method = "lm"` to fit a
  formula.
* The default of `conf_level` in `geom_vector_smooth()` is `0.95` instead of
  `c(.95, NA)`. Only the first element was ever used, so results are the same.

## Bug fixes

* `efield_maker()` now uses its `k` and `q_test` arguments; they were ignored
  and the field was always computed with `k = 1` and `q_test = 1`.
* Stream layers no longer disappear when a seed point lands where the field is
  undefined, for example a grid point on a charge with `efield_maker()`. The
  undefined seeds are skipped and the remaining streams are drawn.
* `linetype` now works for vector layers such as `geom_vector()` and
  `geom_vector_field()`; vectors were always drawn solid.
* `geom_stream()` no longer errors when every group has a single observation.
  It gives the same message as `geom_path()` and draws nothing.
* `avg_spd`, the default color of streams, is now right for streams drawn with
  `normalize = FALSE`, `center = FALSE` and a time `T`. The arc length of the
  last point was miscalculated, which made `avg_spd` too low.
* `geom_stream_smooth()` and `stat_stream_smooth()` no longer warn about
  duplicated aesthetics when given a fixed aesthetic such as `color = "red"`
  or `linewidth = 2`, and `colour = "red"` is no longer overridden by the blue
  default.

## Performance

* `geom_stream_smooth()` and `geom_gradient_smooth()` fit their model once per
  group instead of at every evaluation of the field. A single layer could
  previously fit the model thousands of times. Results are unchanged.
* Computing streams with `method = "lsoda"`, which is also what
  `geom_gradient_field(type = "stream")` and the smoothers use, is about twice
  as fast. Results are unchanged.

## New

* `stat_vector_smooth()` is now exported. It was documented but could not be
  called.
* `StatStreamSmooth` and `StatGradientSmooth` are the stats behind
  `geom_stream_smooth()` and `geom_gradient_smooth()`.

## Documentation

* The help pages now state the real defaults: `pi_type = "ellipse"` in
  `geom_vector_smooth()` (wedges are used when `eval_points` is `NULL`), and
  `n = 51` in `geom_potential()`, whose `fun` returns a vector.
* The smoothers are documented as using a gam by default, with `formula`
  applying to `method = "lm"`.
* Angles in `geom_stream_smooth()` are documented as radians, a mislabelled
  example in `geom_vector_smooth()` is fixed, and the references to the
  nonexistent `geom_vector_smooth2()` are removed.
* Examples are now run when the package is checked. This turned up a
  `geom_stream_smooth()` example that failed because it had too few points
  for the default smoother, which needs at least 30.

## Internal

* sp and gstat are now suggested rather than imported, since only
  `method = "kriging"` in `geom_vector_smooth()` needs them; you are asked to
  install them the first time you use it. farver is no longer a dependency,
  and rlang, which was already used, is declared.
* The package no longer calls unexported ggplot2 functions.
* Each `geom_*()` / `stat_*()` pair is built by one shared internal
  constructor, so a pair can no longer drift apart, and a test checks that
  both members of a pair pass the same parameters.
* Errors are raised with cli throughout.
* Added regression tests for every fix above and an R CMD check workflow.

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
