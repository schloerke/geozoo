\name{cube.solid.grid}
\alias{cube.solid.grid}
\title{Equidistant Solid Cube}
\usage{
  cube.solid.grid(p = 3, n = 8)
}
\arguments{
  \item{p}{dimension of object}

  \item{n}{length of number of points in each dimension}
}
\value{
  \item{points }{location of points} \item{edges }{edges of
  the object}
}
\description{
  A function to generate a solid cube with equidistant
  points
}
\examples{
## Generates a solid cube with equidistant points
cube.solid.grid(p = 3, n = 8)
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/cube/}
}
\keyword{dynamic}

