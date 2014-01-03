\name{torus}
\alias{torus}
\title{Torus}
\usage{
  torus(p = 3, n = 10000)
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
  A function to generate a torus in any dimension
}
\examples{
## Generates a torus
torus(p = 3, n = 1000)
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/torus/}
}
\keyword{dynamic}

