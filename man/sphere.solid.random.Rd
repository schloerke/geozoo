\name{sphere.solid.random}
\alias{sphere.solid.random}
\title{Solid sphere with Random Points}
\usage{
  sphere.solid.random(p, n = p * 500)
}
\arguments{
  \item{p}{dimension of object}

  \item{n}{number of points}
}
\value{
  \item{points }{location of points} \item{edges }{edges of
  the object (null)}
}
\description{
  A function to generate a solid sphere with random points
}
\examples{
## Generates a solid sphere with random points.
sphere.solid.random(p = 3, n = 1000)
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/sphere/}
}
\keyword{dynamic}

