\name{sphere.solid.eq}
\alias{sphere.solid.eq}
\title{ Solid Sphere with Equidistant Points}
\description{
A function to generate a solid sphere with equidistant points.
}
\usage{
sphere.solid.eq(p = 3, n = 8)
}
\arguments{
  \item{p}{ dimension of object }
  \item{n}{ maximum number of points in the diameter }
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/sphere/} }
\author{ Barret Schloerke }
\examples{
## Generates a solid sphere with equidistant points
sphere.solid.eq(p = 3, n = 8)

## Through rggobi
ggobi(sphere.solid.eq(p = 4,n = 20))
}
\keyword{ dynamic }