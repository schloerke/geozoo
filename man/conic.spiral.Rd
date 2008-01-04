\name{conic.spiral}
\alias{conic.spiral}
\title{ Conic Spiral }
\description{
A function to produce a conic spiral}
\usage{
conic.spiral(n = 10000, a = 0.2, b = 1, c = 0.1, w = 2)
}
\arguments{
  \item{n}{ the number of points used to generate the object. }
  \item{a}{ final radius of cone. }
  \item{b}{ height of object. }
  \item{c}{ inner radius. }
  \item{w}{ number of spirals. }
}
\value{
  \item{points }{location of points}
  \item{edges }{edges of the object (null)}
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/} }
\author{ Barret Schloerke }
\examples{
## Generates a Conic Spiral
conic.spiral( n = 10000 )

## Through rggobi
ggobi(conic.spiral(n = 10000))
}
\keyword{ dynamic }