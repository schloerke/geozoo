\name{mobius}
\alias{mobius}
\title{ Mobius }
\description{
A function to generate a mobius strip in the third or fourth dimension.
}
\usage{
mobius(n = 10000)
}
\arguments{
  \item{n}{ number of points }
}
\references{ \url{http://www.ggobi.org/packages/geozoo} }
\author{ Barret Schloerke }
\examples{
## Generates a mobius strip.
mobius( n = 10000)

## Through rggobi
ggobi(mobius( n = 10000))
}
\keyword{ dynamic }