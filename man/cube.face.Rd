\name{cube.face}
\alias{cube.face}
\title{ Cube with points on the 'face'.}
\description{
A function to generate a cube with points on its face.
}
\usage{
cube.face(p = 3)
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
## Generates a cube with points on its face
cube.face(p = 3)

\dontrun{
## Through rggobi
ggobi(cube.face(p = 3))
}}
\keyword{ dynamic }