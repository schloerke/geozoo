\name{cube.dotline}
\alias{cube.dotline}
\title{ Cube with points along the wire frame}
\description{
A functions to generate a cube with points along its wire frame.
}
\usage{
cube.dotline(p = 3)
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
## Generates a cube with points along its wire frame
cube.dotline(p = 3)

\dontrun{
## Through rggobi
ggobi(cube.dotline(p = 3))
}}
\keyword{ dynamic }