\name{torus.flat}
\alias{torus.flat}
\title{ Flat Torus }
\description{
A function to generate a flat torus in any dimension.
}
\usage{
torus.flat(p = 4, n = 10000)
}
\arguments{
  \item{p}{ dimension of object.  (number of circles x2) }
  \item{n}{ number of points }
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/torus/} }
\author{ Barret Schloerke }
\examples{
## Generates a Flat Torus.
torus <- torus.flat(p = 4, n = 10000)

## Through rggobi
ggobi(torus.flat(p = 6, n = 10000))
}
\keyword{ dynamic }