\name{sphere}
\alias{sphere}
\title{ Sphere }
\description{
A function to generate a sphere with points on the surface.
}
\usage{
sphere(p = 3)
}
\arguments{
  \item{p}{ dimension of object }
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/sphere/} }
\author{ Barret Schloerke }
\examples{
## Generates a sphere with points on the surface.
sphere(p = 3)

## Through rggobi
ggobi(sphere(p = 4))
}
\keyword{ dynamic }