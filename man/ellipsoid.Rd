\name{ellipsoid}
\alias{ellipsoid}
\title{Ellipsoid}
\usage{
  ellipsoid(n = 10000, a = 1, b = 1, c = 3)
}
\arguments{
  \item{n}{number of points}

  \item{a}{radius in x direction}

  \item{b}{radius in y direction}

  \item{c}{radius in z direction}
}
\value{
  \item{points }{location of points} \item{edges }{edges of
  the object (null)}
}
\description{
  A function to generate an ellipsoid
}
\examples{
## Generates an ellipsoid
ellipsoid(n = 1000, a = 1, b = 1, c = 3)
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/}
}
\keyword{dynamic}

