\name{cube.solid.random}
\alias{cube.solid.random}
\title{ Solid Cube}
\description{
A function to generate a solid cube with random points.
}
\usage{
cube.solid.random(p, n = 850 * 2^p)
}
\arguments{
  \item{p}{ dimension of object }
  \item{n}{ number of points in object }
}
\value{
  \item{points }{location of points}
  \item{edges }{edges of the object}
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/cube/} }
\author{ Barret Schloerke }
\examples{
## Generates a solid cube with random points
cube.solid.random(p = 3,)

## Through rggobi
ggobi(cube.solid.random(p = 3,))
}
\keyword{ dynamic }