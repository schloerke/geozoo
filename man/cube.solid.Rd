\name{cube.solid}
\alias{cube.solid}
\title{ Solid Cube}
\description{
A function to generate a solid cube with random points.
}
\usage{
cube.solid(p = 3)
}
\arguments{
  \item{p}{ dimension of object }
}
\value{
  \item{points }{location of points}
  \item{edges }{edges of the object}
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/cube/} }
\author{ Barret Schloerke }
\examples{
## Generates a solid cube with random points
cube.solid.eq(p = 3)

## Through rggobi
ggobi(cube.solid(p = 3))
}
\keyword{ dynamic }