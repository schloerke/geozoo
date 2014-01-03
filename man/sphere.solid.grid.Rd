\name{sphere.solid.grid}
\alias{sphere.solid.grid}
\title{Solid Sphere with Equidistant Points}
\usage{
  sphere.solid.grid(p = 3, n = 8)
}
\arguments{
  \item{p}{dimension of object}

  \item{n}{maximum number of points in the diameter}
}
\value{
  \item{points }{location of points} \item{edges }{edges of
  the object (null)}
}
\description{
  A function to generate a solid sphere with equidistant
  points.
}
\examples{
## Generates a solid sphere with equidistant points
sphere.solid.grid(p = 3, n = 8)
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/sphere/}
}
\keyword{dynamic}

