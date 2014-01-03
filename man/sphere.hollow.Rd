\name{sphere.hollow}
\alias{sphere.hollow}
\title{Sphere}
\usage{
  sphere.hollow(p, n = p * 500)
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
  A function to generate a sphere with points on the
  surface
}
\examples{
## Generates a sphere with points on the surface
sphere.hollow(p = 3, n = 1000)
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/sphere/}
}
\keyword{dynamic}

