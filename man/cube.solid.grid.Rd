\name{cube.solid.grid}
\alias{cube.solid.grid}
\title{ Equidistant Solid Cube}
\description{
A function to generate a solid cube with equidistant points.
}
\usage{
cube.solid.grid(p = 3, n = 8)
}
\arguments{
  \item{p}{ dimension of object }
  \item{n}{ length of number of points in each dimension }
}
\value{
  \item{points }{location of points}
  \item{edges }{edges of the object}
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/cube/} }
\author{ Barret Schloerke }
\examples{
## Generates a solid cube with equidistant points
cube.solid.grid(p = 3, n = 8)

## Through rggobi
ggobi(cube.solid.grid(p = 3, n = 8))
}
\keyword{ dynamic }