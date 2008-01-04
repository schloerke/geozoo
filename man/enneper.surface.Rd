\name{enneper.surface}
\alias{enneper.surface}
\title{ Enneper's Surface}
\description{
A function to generate Enneper's surface.
}
\usage{
enneper.surface(n = 10000, a = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ number of points }
  \item{a}{ angle, radians, minimum and maximum.  -a < angle < a }
}
\value{
  \item{points }{location of points}
  \item{edges }{edges of the object}
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/} }
\author{ Barret Schloerke }
\examples{
## Generates an Enneper Surface
enneper.surface(n = 10000, a = 4)

## Through rggobi
ggobi(enneper.surface(n = 10000, a = 4))
}
\keyword{ dynamic }