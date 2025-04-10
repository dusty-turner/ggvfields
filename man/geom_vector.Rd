% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_vector.R
\docType{data}
\name{geom_vector}
\alias{geom_vector}
\alias{stat_vector}
\alias{geom_vector2}
\alias{stat_vector2}
\alias{StatVector}
\title{Vector Layers for ggplot2}
\usage{
geom_vector(
  mapping = NULL,
  data = NULL,
  stat = StatVector,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  center = TRUE,
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  L = NULL,
  arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")
)

stat_vector(
  mapping = NULL,
  data = NULL,
  geom = GeomStream,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  center = TRUE,
  normalize = TRUE,
  tail_point = FALSE,
  eval_point = FALSE,
  L = NULL,
  arrow = grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")
)

geom_vector2(
  mapping = NULL,
  data = NULL,
  stat = StatVector,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  center = FALSE,
  tail_point = TRUE,
  eval_point = FALSE,
  L = NULL,
  arrow = NULL
)

stat_vector2(
  mapping = NULL,
  data = NULL,
  geom = GeomStream,
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  center = FALSE,
  tail_point = TRUE,
  eval_point = FALSE,
  L = NULL,
  arrow = NULL
)
}
\arguments{
\item{mapping}{A set of aesthetic mappings created by \code{ggplot2::aes()}.
\strong{Required:} Must include \strong{\code{x}} and \strong{\code{y}}; in addition, either \strong{\code{xend}}
and \strong{\code{yend}} or one of the alternative specifications (\strong{\code{fx}}/ \strong{\code{fy}}
or \strong{\code{angle}}/ \strong{\code{angle_deg}} and \strong{\code{distance}}) must be provided.}

\item{data}{A data frame containing the vector data in wide format.}

\item{stat}{The statistical transformation to use on the data (default:
\link{StatVector}).}

\item{position}{Position adjustment, either as a string or the result of a
position adjustment function.}

\item{...}{Other arguments passed on to \code{grid::layer()}.}

\item{na.rm}{Logical. If \code{FALSE} (the default), missing values are removed
with a warning.}

\item{show.legend}{Logical. Should this layer be included in the legends?}

\item{inherit.aes}{Logical. If \code{FALSE}, overrides the default aesthetics
rather than combining with them.}

\item{center}{Logical. If \code{TRUE}, the vector is recentered so that the
original \verb{(x, y)} becomes the midpoint (default is \code{TRUE} for
\code{geom_vector()} and \code{FALSE} for \code{geom_vector2()}).}

\item{normalize}{Logical. If \code{TRUE}, the vector endpoints are scaled to unit
length before being scaled by \code{L} (default: \code{TRUE}).}

\item{tail_point}{Logical. If \code{TRUE}, a point is drawn at the tail (the
starting point) of each vector (default is \code{FALSE} for \code{geom_vector()} and
\code{TRUE} for \code{geom_vector2()}).}

\item{eval_point}{Logical. If \code{TRUE}, a point is drawn at the evaluation
point corresponding to the original (untransformed) seed point before any
centering or normalization (default: \code{FALSE}).}

\item{L}{Numeric scalar. The desired length for the vectors in data units. If
\code{NULL} (the default), a value is computed automatically based on the plot’s
x and y limits.}

\item{arrow}{An optional \code{grid::arrow()} specification to add arrowheads to
the vectors (default: \code{grid::arrow(angle = 25, length = unit(0.025, "npc"), type = "closed")}).}

\item{geom}{The geometric object used to render the streamline (only used in
\code{stat_stream()}; defaults to \link{GeomStream}).}
}
\value{
A ggplot2 layer that can be added to a plot.
}
\description{
Create layers for drawing vectors on ggplot2 plots. These functions accept
wide-format data with the required aesthetics \strong{\code{x}} and \strong{\code{y}} plus either
\strong{\code{xend}} and \strong{\code{yend}} or one of the alternative specifications: \strong{\code{fx}}
and \strong{\code{fy}}, or \strong{\code{angle}}/ \strong{\code{angle_deg}} and \strong{\code{distance}}.
}
\details{
When specifying the vector direction using polar coordinates, you can provide
either:
\itemize{
\item \strong{\code{angle}}: the vector direction in \strong{radians}.
\item \strong{\code{angle_deg}}: the vector direction in \strong{degrees} (which is
automatically converted to radians).
}

The endpoints are computed by translating the starting point using these
polar coordinates along with the supplied \strong{\code{distance}}.

The data is converted to long format (two rows per vector) via \link{StatVector}
and rendered with \link{GeomStream}. Optionally, arrowheads can be added to
indicate direction.

There are two variants:
\itemize{
\item \code{geom_vector()}: Uses the user-supplied aesthetic mapping.
\item \code{geom_vector2()}: Uses the same underlying stat (\link{StatVector}) but adds a
default mapping for \code{length = after_stat(norm)}, making the computed vector
norm available as an aesthetic.
}
}
\section{Aesthetics}{


\code{geom_vector()} and \code{geom_vector2()} understand the following aesthetics
(required aesthetics are in bold):
\itemize{
\item \strong{x}
\item \strong{y}
\item xend
\item yend
\item fx (alternative specification)
\item fy (alternative specification)
\item angle (vector direction in radians; alternative specification)
\item angle_deg (vector direction in degrees; alternative specification, converted to radians)
\item distance (with angle/angle_deg, used to compute endpoints)
\item alpha
\item color
\item fill
\item group
\item linetype
\item size
}
}

\section{Computed Variables}{


These are calculated by the 'stat' part of layers and can be accessed with
delayed evaluation.

\describe{
\item{norm}{Calculated as the Euclidean distance between the starting point
(\code{x}, \code{y}) and the computed endpoint (\code{xend}, \code{yend}). This
value is used to normalize the vector length when the \code{normalize} parameter
is set to \code{TRUE}.}
}
}

\examples{
set.seed(1234)
n <- 10

# Generate wind data in polar coordinates
data <- data.frame(
  x = rnorm(n),
  y = rnorm(n),
  dir = runif(n, -pi, pi), # angle in radians
  spd = rchisq(n, df = 2)  # speed
) |>
  transform(fx = spd * cos(dir), fy = spd * sin(dir))

# Using fx/fy to compute endpoints
ggplot(data, aes(x, y)) +
  geom_vector(aes(fx = fx, fy = fy))

# Using angle (in radians) and distance to compute endpoints
ggplot(data, aes(x, y)) +
  geom_vector(aes(angle = dir, distance = spd))

# Using angle_deg (in degrees) and distance to compute endpoints
vectors3 <- data.frame(
  x = c(0, 1, 2),
  y = c(0, 1, 2),
  angle_deg = c(0, 90, 45),
  angle = c(0, pi/2, pi/4),
  distance = c(3, 4, 5)
)
ggplot(vectors3, aes(x, y)) +
  geom_vector(aes(angle_deg = angle_deg, distance = distance))

# Basic usage with explicit start and end points:
vectors1 <- data.frame(
  x    = c(0, 1, 2),
  y    = c(0, 1, 2),
  xend = c(3, 1, 5),
  yend = c(0, 5, 6)
)
ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_vector()

# Using center = TRUE to recenter vectors:
ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_vector(center = TRUE)

# Using normalize = TRUE to adjust vectors to unit length:
ggplot(vectors3, aes(x = x, y = y, angle = angle, distance = distance)) +
  geom_vector(normalize = TRUE)

# Using geom_vector2, which adds a default mapping for `length`
ggplot(vectors1, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_vector2()

}
\keyword{datasets}
