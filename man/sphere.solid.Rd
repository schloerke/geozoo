\name{sphere.solid}
\alias{sphere.solid}
\title{ Solid sphere with Random Points }
\description{
A function to generate a solid sphere with random points.
}
\usage{
sphere.solid(p = 3)
}
\arguments{
  \item{p}{ dimension of object }
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/sphere/} }
\author{ Barret Schloerke }
\examples{
## Generates a solid sphere with random points.
sphere.solid(p = 3)

## Through rggobi
ggobi(sphere.solid(p = 5))
}
\keyword{ dynamic }