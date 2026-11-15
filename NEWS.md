# ggvfields 1.1.0

## Changes in behavior

* The ellipses and wedges of `geom_vector_smooth(method = "gam")`, the
  default, are now prediction intervals, like those of `method = "lm"` and
  `method = "kriging"`. They used only the variance of the fitted mean
  (`se.fit`), which made them confidence intervals for the mean; they now add
  each component's residual variance, so they cover a new observation at that
  location. They are noticeably larger.

* `method` in `geom_stream_field()`, `stat_stream_field()`,
  `geom_stream_field2()` and `stat_stream_field2()` now selects the integrator.
  It was documented (`"rk4"` by default, `"euler"` available) but ignored, and
  streams were always computed with the adaptive `lsoda` solver from deSolve.
  `"rk4"` and `"euler"` are now real integrators: they step in equal shares of
  the arc length `L`, or of the time `T` when streams are run to a time, and
  shorten a step where the field turns or changes speed sharply, so streams
  that run into a sink or a point charge stop there, while streams cross a
  jump in the field (for example one defined with `sign()` or `ifelse()`). The 1.0.1 behavior is available as `method = "lsoda"`. An
  unknown `method` is now an error.
* Streams are now computed with `"rk4"` by default, in `geom_stream_field()`
  and its relatives as well as `geom_gradient_field(type = "stream")`,
  `geom_stream_smooth()` and `geom_gradient_smooth()`. Plots look the same,
  but streams follow the field more closely, which changes `avg_spd` slightly.
  The visible difference is at singularities: a stream that runs into a point
  charge, for example with `efield_maker()`, now stops at the charge instead
  of passing through it and out the other side.
* Supplying `formula` to `geom_stream_smooth()`, `stat_stream_smooth()`,
  `geom_vector_smooth()` or `stat_vector_smooth()` with the default
  `method = "gam"` now warns. The gam smoothers always fit `fx` and `fy` with
  `s(x, y)`, so the formula was silently ignored. Use `method = "lm"` to fit a
  formula.
* The default of `conf_level` in `geom_vector_smooth()` is `0.95` instead of
  `c(.95, NA)`. Only the first element was ever used, so results are the same.

## Bug fixes

* Centered streams (`center = TRUE`, the default) no longer fail the whole
  layer when a stream can move only one way from its seed, for example on the
  edge of the domain where the field is defined. Such a seed now gets the half
  of the stream that exists.

* `stat_vector()` colors vectors by their norm by default, as `geom_vector()`
  does. The default `color = after_stat(norm)` mapping was dropped from
  `stat_vector()` in a refactor before 1.0.0, so its vectors were black.

* `stat_stream_field()`, `geom_gradient_field()`, `geom_gradient_field2()`,
  `geom_potential()` and their `stat_*()` versions now default to
  `inherit.aes = FALSE`, like `geom_vector_field()` and `geom_stream_field()`.
  These layers are computed from `fun` and have no data columns, so
  inheriting the plot's aesthetics made them fail with "object not found" when
  added to a plot with its own data, for example to draw a field behind
  observed vectors.

* `avg_spd`, the default color of streams, is now right for
  `geom_stream_field(normalize = FALSE)` without `T`. Those streams are grown
  to length `L` and then cut back to the time of the fastest one, but kept
  the `avg_spd` of the uncut stream. It is now the length of the drawn stream
  divided by the time every stream was given, as for streams run to a time
  `T`, so color and length agree. The streams themselves are unchanged.
* `avg_spd` of streams that end at a sink no longer depends on the integrator
  or on where it decides the stream has stopped moving. A stream run to a
  length `L` that stops short of it at a sink would take unbounded time to
  reach the sink, which made its `avg_spd` close to 0 and differ between
  methods by a factor of 2; its speed is now measured up to 99% of the length
  it achieved. A stream run to a time `T` that stops early at a sink has
  `avg_spd` equal to its length over `T`. Streams that don't reach a sink are
  unchanged.

* `stat_stream_field()` now uses its `normalize` argument and
  `stat_stream_field2()` its `tol` argument, as `geom_stream_field()` and
  `geom_stream_field2()` do. Both were ignored: `normalize` was always `TRUE`
  and `tol` always `sqrt(.Machine$double.eps)`, the defaults, so plots that
  don't set them are unchanged.

* `geom_gradient_field2(type = "vector")` and `stat_gradient_field2(type =
  "vector")` now scale vectors by the size of the gradient, mapping
  `length = after_stat(norm)` as documented and as `geom_vector_field2()`
  does. The mapping had been overwritten, so every vector had the same length.
  The default streams are unchanged.

* `geom_stream_smooth()`, `geom_vector_smooth()` and their stats now work
  with fewer than 30 data points. The default `method = "gam"` smoother
  used mgcv's default basis of 30 functions, which needs at least 30
  distinct locations; with fewer, the layer warned that the computation
  failed and drew nothing. Small data sets now use half as many basis
  functions as locations. Fewer than 5 locations is an error that suggests
  `method = "lm"`. Fits with 30 or more locations are unchanged.

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
* Streams that run into a sink anywhere other than the origin now get the
  right `avg_spd`. The solver only recognized a sink at the origin; elsewhere
  it kept the clock running long after the stream had stopped moving, so
  `avg_spd` came out near zero. The drawn streams are unchanged.
* `geom_stream_smooth()` and `stat_stream_smooth()` no longer warn about
  duplicated aesthetics when given a fixed aesthetic such as `color = "red"`
  or `linewidth = 2`, and `colour = "red"` is no longer overridden by the blue
  default.

## Performance

* `geom_stream_smooth()` and `geom_gradient_smooth()` fit their model once per
  group instead of at every evaluation of the field. A single layer could
  previously fit the model thousands of times. Results are unchanged.
* Computing streams with `method = "lsoda"` is about twice as fast. Results
  are unchanged.

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
  `geom_stream_smooth()` example that failed because it had fewer than 30
  points, which the default gam smoother could not fit before this release
  (see Bug fixes).

## Internal

* sp and gstat are now suggested rather than imported, since only
  `method = "kriging"` in `geom_vector_smooth()` needs them; you are asked to
  install them the first time you use it. farver is no longer a dependency,
  and rlang, which was already used, is declared.
* The package no longer calls unexported ggplot2 functions.
* Each `geom_*()` / `stat_*()` pair is built by one shared internal
  constructor, so a pair can no longer drift apart, and a test checks that
  both members of a pair pass the same parameters and default mappings.
* Errors are raised with cli throughout.
* Added regression tests for every fix above and an R CMD check workflow.
* Added visual snapshot tests with vdiffr (now in Suggests), one or two per
  layer family; CRAN skips them.

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
