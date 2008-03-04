\name{sphere.hollow}
\alias{sphere.hollow}
\title{ Sphere }
\description{
A function to generate a sphere with points on the surface.
}
\usage{
sphere.hollow(p, n = p * 500)
}
\arguments{
  \item{p}{ dimension of object }
  \item{n}{ number of points }
}
\references{ \url{http://www.ggobi.org/packages/geozoo} }
\author{ Barret Schloerke }
\examples{
## Generates a sphere with points on the surface.
sphere.hollow(p = 3)

## Through rggobi
ggobi(sphere.hollow(p = 4))
}
\keyword{ dynamic }