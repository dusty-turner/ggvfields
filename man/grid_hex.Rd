% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/grid_hex.R
\name{grid_hex}
\alias{grid_hex}
\title{Generate a Hexagonal Lattice}
\usage{
grid_hex(xlim, ylim, d)
}
\arguments{
\item{xlim}{A numeric vector of length 2 specifying the x-axis limits.}

\item{ylim}{A numeric vector of length 2 specifying the y-axis limits.}

\item{d}{A numeric value specifying the hexagon diameter.}
}
\value{
A data frame with two columns, \code{x} and \code{y}, containing the
coordinates of the hexagonal grid points.
}
\description{
This function generates a hexagonal lattice of points within the given x and
y limits, using a specified hexagon diameter. The diameter is 2 times the
distance between adjacent x (and y) values, see examples.
}
\examples{

xlim <- c(-1, 1)
ylim <- c(-1, 0)

grid <- grid_hex(xlim, ylim, .25)

head( grid )
str( grid )
plot( grid, asp = 1 )

diff(sort(unique(grid$x)))


}
