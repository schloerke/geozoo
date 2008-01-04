\name{cube}
\alias{cube}
\title{ Cube }
\description{
A function generate a cube with vertices and a wire frame.
}
\usage{
cube(p = 3)
}
\arguments{
  \item{p}{ dimension of object}
}
\value{
  \item{points }{location of points}
  \item{edges }{edges of the object}
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/cube/} }
\author{ Barret Schloerke }
\examples{
## Generates a cube
cube(p = 3)

## Through rggobi
ggobi(cube(p = 3))
}
\keyword{ dynamic }