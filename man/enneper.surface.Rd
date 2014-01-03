\name{enneper.surface}
\alias{enneper.surface}
\title{Enneper's Surface}
\usage{
  enneper.surface(n = 10000, a = 4)
}
\arguments{
  \item{n}{number of points}

  \item{a}{angle, radians, minimum and maximum.  -a < angle
  < a}
}
\value{
  \item{points }{location of points} \item{edges }{edges of
  the object (null)}
}
\description{
  A function to generate Enneper's surface
}
\examples{
## Generates an Enneper Surface
enneper.surface(n = 1000, a = 4)
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/}
}
\keyword{dynamic}

