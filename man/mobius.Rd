\name{mobius}
\alias{mobius}
\title{ Mobius }
\description{
A function to generate a mobius strip in the third or fourth dimension.
}
\usage{
mobius(p, n = 10000)
}
\arguments{
  \item{p}{ dimension of object.  3 or 4 }
  \item{n}{ number of points }
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/mobius/} }
\author{ Barret Schloerke }
\examples{
## Generates a mobius strip.
mobius(3, n = 10000)

## Through rggobi
ggobi(mobius(4, n = 10000))
}
\keyword{ dynamic }