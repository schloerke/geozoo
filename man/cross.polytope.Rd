\name{cross.polytope}
\alias{cross.polytope}
\title{ Cross Polytope }
\description{
A function generate a cross polytope, cube dual, with vertices and a wire frame.
}
\usage{
cross.polytope(p = 3)
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
## Generates a 3-D Cross Polytope
cross.polytope(p = 3)

## Through rggobi in 4-D
ggobi(cross.polytope(p = 4))
}
\keyword{ dynamic }