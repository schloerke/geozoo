\name{torus.flat}
\alias{torus.flat}
\title{Flat Torus}
\usage{
  torus.flat(p = 4, n = 10000)
}
\arguments{
  \item{p}{dimension of object (number of circles x2)}

  \item{n}{number of points}
}
\value{
  \item{points }{location of points} \item{edges }{edges of
  the object (null)}
}
\description{
  A function to generate a flat torus in any dimension
}
\examples{
## Generates a Flat Torus
torus.flat(p = 4, n = 1000)
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/torus/}
}
\keyword{dynamic}

