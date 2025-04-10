% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_stream_field.R
\docType{data}
\name{geom_stream_field}
\alias{geom_stream_field}
\alias{stat_stream_field}
\alias{geom_stream_field2}
\alias{stat_stream_field2}
\alias{StatStreamField}
\title{Create a Stream Field Layer in ggplot2}
\usage{
geom_stream_field(
  mapping = NULL,
  data = NULL,
  stat = StatStreamField,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 11,
  args = list(),
  max_it = 1000L,
  tol = sqrt(.Machine$double.eps),
  T = NULL,
  L = NULL,
  center = TRUE,
  type = "stream",
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  grid = NULL,
  method = "rk4",
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = grid::arrow(angle = 30, length = unit(0.02, "npc"), type = "closed")
)

stat_stream_field(
  mapping = NULL,
  data = NULL,
  geom = GeomStream,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 11,
  args = list(),
  max_it = 1000,
  tol = sqrt(.Machine$double.eps),
  T = NULL,
  L = NULL,
  center = TRUE,
  type = "stream",
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  grid = NULL,
  method = "rk4",
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  arrow = grid::arrow(angle = 30, length = unit(0.02, "npc"), type = "closed")
)

geom_stream_field2(
  mapping = NULL,
  data = NULL,
  stat = StatStreamField,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 11,
  args = list(),
  max_it = 1000,
  tol = sqrt(.Machine$double.eps),
  L = NULL,
  center = FALSE,
  type = "stream",
  tail_point = TRUE,
  eval_point = FALSE,
  grid = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  method = "rk4"
)

stat_stream_field2(
  mapping = NULL,
  data = NULL,
  geom = GeomStream,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = FALSE,
  fun,
  xlim = NULL,
  ylim = NULL,
  n = 11,
  args = list(),
  max_it = 1000,
  tol = sqrt(.Machine$double.eps),
  L = NULL,
  center = FALSE,
  type = "stream",
  tail_point = TRUE,
  eval_point = FALSE,
  grid = NULL,
  lineend = "butt",
  linejoin = "round",
  linemitre = 10,
  method = "rk4"
)
}
\arguments{
\item{mapping}{A set of aesthetic mappings created by \code{ggplot2::aes()}.
(Optional)}

\item{data}{A data frame or other object, as in \code{ggplot2::layer()}.
(Optional)}

\item{stat}{The statistical transformation to use on the data (default:
\link{StatStreamField}).}

\item{position}{Position adjustment, either as a string or the result of a
position adjustment function.}

\item{...}{Other arguments passed to \code{ggplot2::layer()} and the underlying
geometry/stat.}

\item{na.rm}{Logical. If \code{FALSE} (the default), missing values are removed
with a warning. If \code{TRUE}, missing values are silently removed.}

\item{show.legend}{Logical. Should this layer be included in the legends?}

\item{inherit.aes}{Logical. If \code{FALSE}, overrides the default aesthetics
rather than combining with them.}

\item{fun}{A function of two variables, \code{fun(x, y)}, returning a two-element
vector \eqn{(dx, dy)} that defines the local flow direction at any point.}

\item{xlim}{Numeric vector of length 2 specifying the domain limits in the
\eqn{x}-direction. Defaults to \eqn{c(-1, 1)}.}

\item{ylim}{Numeric vector of length 2 specifying the domain limits in the
\eqn{y}-direction. Defaults to \eqn{c(-1, 1)}.}

\item{n}{Integer or two-element numeric vector specifying the grid resolution
(number of seed points) along each axis. Defaults to \code{11}, producing an
\eqn{11 \times 11} grid.}

\item{args}{A list of additional arguments passed to \code{fun}.}

\item{max_it}{\code{integer(1)}; Maximum number of integration steps per
streamline (default: \code{1000L}).}

\item{tol}{\code{numeric(1)}; a tolerance used to determine if a sink has been
hit, among other things (default: \code{sqrt(.Machine$double.eps)}).}

\item{T}{Numeric. When \code{normalize = FALSE}, each streamline is integrated for
a fixed time \code{T} before being cropped to match the duration of the fastest
streamline reaching the arc length \code{L}. When \code{normalize = TRUE},
integration instead stops when the cumulative arc length reaches \code{L}, and
the parameter \code{T} is ignored.}

\item{L}{Numeric. Maximum arc length for each streamline. When \code{normalize = TRUE}, integration stops once the cumulative arc length reaches \code{L}. When
\code{normalize = FALSE}, streamlines are initially computed for a fixed time
\code{T} and then cropped so that all are truncated to the duration it takes the
fastest streamline to reach the arc length \code{L}. Defaults to \code{NULL} (a
suitable default is computed from the grid spacing).}

\item{center}{Logical. If \code{TRUE} (default), centers the seed points (or
resulting streamlines) so that the original (x, y) becomes the midpoint.}

\item{type}{Character. Either \code{"stream"} (default) or \code{"vector"}. \code{"stream"}
computes a full streamline by integrating in both directions (if \code{center = TRUE}), while \code{"vector"} computes a single vector.}

\item{normalize}{Logical.
When \code{normalize = TRUE} (the default), each streamline is integrated until its
cumulative arc length reaches the specified value \code{L}, ensuring that all streams
have a uniform, normalized length based on grid spacing.
When \code{normalize = FALSE}, the integration runs for a fixed time (\code{T}), and afterward,
all streamlines are cropped to the duration it takes for the fastest one to reach
the length \code{L}, allowing for variations in arc lengths that reflect differences in
flow speeds.}

\item{tail_point}{Logical. If \code{TRUE}, draws a point at the tail (starting
point) of each streamline. Defaults to \code{FALSE}.}

\item{eval_point}{Logical. If \code{TRUE}, draws a point at the evaluation point
where the field was computed. Defaults to \code{FALSE}.}

\item{grid}{A data frame containing precomputed grid points for seed
placement. If \code{NULL} (default), a regular Cartesian grid is generated based
on \code{xlim}, \code{ylim}, and \code{n}.}

\item{method}{Character. Integration method (e.g. \code{"rk4"} for Runge-Kutta 4,
\code{"euler"} for Euler's method). Defaults to \code{"rk4"}.}

\item{lineend}{Line end style (round, butt, square).}

\item{linejoin}{Line join style (round, mitre, bevel).}

\item{linemitre}{Line mitre limit (number greater than 1).}

\item{arrow}{A \code{grid::arrow()} specification for adding arrowheads to the
streamlines. Defaults to a closed arrow with a 30° angle and length \code{0.02}
npc.}

\item{geom}{The geometric object used to render the streamlines (defaults to
\link{GeomStream}).}
}
\value{
A ggplot2 layer that computes and renders streamlines over the
specified domain.
}
\description{
\code{\link[=geom_stream_field]{geom_stream_field()}} creates a ggplot2 layer that integrates a user-defined
vector field function \eqn{f(x, y) \to (dx, dy)} over a grid of seed points
within a specified domain. The function numerically integrates the field
starting from these seeds, producing streamlines that visualize the flow.
This is useful for visualizing vector fields, flow patterns, or trajectories,
such as in fluid dynamics or gradient fields.
}
\section{Aesthetics}{
 \code{geom_stream_field()} (and its stat variant) inherit
aesthetics from \link{GeomStream} and understand the following:
\itemize{
\item \strong{\code{x}}: x-coordinate of the seed point.
\item \strong{\code{y}}: y-coordinate of the seed point.
\item \code{color}: Color, typically used to represent computed statistics (e.g. average speed).
\item \code{linetype}: Type of line used to draw the streamlines.
\item \code{linewidth}: Thickness of the streamlines.
\item \code{alpha}: Transparency of the streamlines.
}
}

\section{Details}{
 The streamlines are generated by numerically integrating
the vector field defined by \code{fun(x, y)}. When \code{normalize = TRUE},
integration stops once the cumulative arc length reaches \code{L}; otherwise,
integration runs until time \code{T} is reached. If both \code{T} and \code{L} are
provided in incompatible combinations, one parameter is ignored. The
computed paths are rendered by \link{GeomStream}.
}

\section{Computed Variables}{


The following variables are computed internally by \link{StatStreamField} during the
integration of the vector field:

\describe{
\item{avg_spd}{For vector fields, this is computed as the total arc length divided
by the integration time, providing an estimate of the average speed. It is used to
scale the vector lengths when mapping \code{length = after_stat(norm)}.}

\item{t}{The integration time at each computed point along a streamline.}

\item{d}{The distance between consecutive points along the computed path.}

\item{l}{The cumulative arc length along the streamline, calculated as the cumulative
sum of \code{d}.}
}
}

\examples{

f <- function(u) c(-u[2], u[1])

# the basic usage involves providing a fun, xlim, and ylim
ggplot() + geom_stream_field(fun = f, xlim = c(-1,1), ylim = c(-1,1))
\dontrun{
# if unspecified, xlim and ylim default to c(-1,1). we use this in what
# follows to focus on other parts of the code
ggplot() + geom_stream_field(fun = f)
ggplot() + geom_stream_field(fun = f, center = FALSE)

ggplot() + geom_stream_field(fun = f, normalize = FALSE)
ggplot() + geom_stream_field(fun = f, normalize = FALSE, center = FALSE)

# run systems until specified lengths
ggplot() + geom_stream_field(fun = f, normalize = TRUE, L = .8)
ggplot() + geom_vector_field(fun = f, normalize = TRUE, L = .3)
ggplot() + geom_vector_field(fun = f, normalize = FALSE, L = 2)

# run systems for specified times
ggplot() + geom_stream_field(fun = f, normalize = FALSE, T = .1)

# tail and eval points
ggplot() + geom_stream_field(fun = f, tail_point = TRUE)
ggplot() + geom_stream_field(fun = f, eval_point = TRUE)

# changing the grid of evaluation
ggplot() + geom_stream_field(fun = f)
ggplot() + geom_stream_field(fun = f, grid = "hex")
ggplot() + geom_stream_field(fun = f, grid = "hex", n = 5)
ggplot() + geom_stream_field(fun = f, n = 5)
ggplot() + geom_stream_field(fun = f, xlim = c(-5, 5)) + coord_equal()
ggplot() + geom_stream_field(fun = f, xlim = c(-5, 5), n = c(21, 11)) + coord_equal()
ggplot() + geom_stream_field(fun = f)
ggplot() + geom_stream_field(fun = f, grid = grid_hex(c(-1,1), c(-1,1), .2))

# using other ggplot2 tools
f <- efield_maker()

ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2))

ggplot() +
  geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2)) +
  scale_color_viridis_c(trans = "log10")

ggplot() +
  geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2)) +
  scale_color_viridis_c(trans = "log10") +
  coord_equal()


# other vector fields
f <- function(u) u
ggplot() + geom_stream_field(fun = f, xlim = c(-1,1), ylim = c(-1,1))

f <- function(u) c(2,1)
ggplot() + geom_stream_field(fun = f, xlim = c(-1,1), ylim = c(-1,1))



# neat examples
f <- function(u) {
  x <- u[1]; y <- u[2]
  c(y, y*(-x^2 - 2*y^2 + 1) - x)
}
ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2))
ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2), type = "vector")

f <- function(u) {
  x <- u[1]; y <- u[2]
  c(y, x - x^3)
}
ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2))
ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2),
  grid = grid_hex(c(-2,2), c(-2,2), .35))

f <- function(u) {
  x <- u[1]; y <- u[2]
  c(x^2 - y^2, x^2 + y^2 - 2)
}
ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2))
ggplot() + geom_stream_field(fun = f, xlim = c(-2,2), ylim = c(-2,2),
  grid = grid_hex(c(-2,2), c(-2,2), .35))

ggplot() +
  geom_stream_field(fun = f, aes(alpha = after_stat(t)), xlim = c(-2,2), ylim = c(-2,2)) +
  scale_alpha(range  = c(0,1))

ggplot() +
  geom_stream_field(
    fun = f, xlim = c(-1,1), ylim = c(-1,1),
    linewidth = .75, arrow = arrow(length = unit(0.015, "npc"))
  )
}

}
\keyword{datasets}
