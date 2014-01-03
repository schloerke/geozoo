\name{cube.solid.random}
\alias{cube.solid.random}
\title{Solid Cube}
\usage{
  cube.solid.random(p, n = 850 * 2^p)
}
\arguments{
  \item{p}{dimension of object}

  \item{n}{number of points}
}
\value{
  \item{points }{location of points} \item{edges }{edges of
  the object}
}
\description{
  A function to generate a solid cube with random points
}
\examples{
## Generates a solid cube with random points
cube.solid.random(p = 3, n = 1000)
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/cube/}
}
\keyword{dynamic}

