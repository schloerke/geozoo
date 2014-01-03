\name{roman.surface}
\alias{roman.surface}
\title{Roman Surface}
\usage{
  roman.surface(n = 10000, a = 1)
}
\arguments{
  \item{n}{number of points}

  \item{a}{maximum radius of object}
}
\value{
  \item{points }{location of points} \item{edges }{edges of
  the object (null)}
}
\description{
  A function to generate a Roman surface, also known as a
  Steiner surface
}
\examples{
## Generates a Roman surface.
roman.surface(n = 1000, a = 1)
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/}
}
\keyword{dynamic}

