\name{ellipsoid}
\alias{ellipsoid}
\title{ Ellipsoid }
\description{
A function to generate an ellipsoid.
}
\usage{
ellipsoid(n = 10000, a = 1, b = 1, c = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ number of points }
  \item{a}{ radius in x direction }
  \item{b}{ radius in y direction }
  \item{c}{ radius in z direction }
}
\value{
  \item{points }{location of points}
  \item{edges }{edges of the object}
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/} }
\author{ Barret Schloerke }
\examples{
## Generates an ellipsoid
ellipsoid(n = 10000, a = 1, b = 1, c = 3)

## Through rggobi
ggobi(ellipsoid(n = 10000, a = 1, b = 1, c = 3))
}
\keyword{ dynamic }