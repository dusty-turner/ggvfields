## Resubmission

This is a resubmission. In this version I have:

* Removed the stray 'Rplots.pdf' files that were included by mistake, and
  excluded them from the build.
* Skipped the visual snapshot tests on CRAN. The ERROR on r-devel-windows
  came from drawing one of these plots: "cannot open file
  '.../ggplot2/R/ggplot2.rdb': No such file or directory". It did not occur
  on win-builder (R-devel) or on the other platforms, but the tests no longer
  draw these plots on CRAN.

## Submission

This is a minor release (1.1.0). Streams are computed with a Runge-Kutta 4
integrator by default, the smoothing functions show prediction intervals, and
several functions gain arguments; see NEWS.md for the full list.

## Test environments

* local: Windows 11, R 4.5.2
* GitHub Actions: macOS (R release), Windows (R release), Ubuntu (R devel,
  release and oldrel-1)
* win-builder: Windows, R devel

## R CMD check results

0 errors | 0 warnings | 0 notes

## Reverse dependencies

There are currently no reverse dependencies.
