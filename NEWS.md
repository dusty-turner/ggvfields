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
