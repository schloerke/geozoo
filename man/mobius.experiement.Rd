\name{mobius.experiment}
\alias{mobius.experiment}
\title{ Mobius }
\description{
A function to generate a 5-D mobius strip in the third dimension.
}
\usage{
mobius.experiment(p=5, n = 10000)
}
\arguments{
  \item{p}{ dimension of object.  (5) }
  \item{n}{ number of points }
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/mobius/} }
\author{ Barret Schloerke }
\examples{
## Generates a mobius strip.
mobius.experiment(5, n = 10000)

## Through rggobi
ggobi(mobius.experiment(5, n = 10000))
}
\keyword{ dynamic }