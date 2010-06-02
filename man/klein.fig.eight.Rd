\name{klein.fig.eight}
\alias{klein.fig.eight}
\title{ Figure Eight Klein Bottle}
\description{
A function to generate a figure eight Klein bottle.
}
\usage{
klein.fig.eight(n = 10000, a = 3, b = 1)
}
\arguments{
  \item{n}{ number of points }
  \item{a}{ radius of outer radius }
  \item{b}{ radius of inner radius }
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/} }
\author{ Barret Schloerke }
\examples{
## Generates a figure eight Klein bottle.
klein.fig.eight(n = 1000, a = 3, b = 1)

\dontrun{
## Through rggobi
ggobi(klein.fig.eight(n = 10000, a = 3, b = 1))
}}
\keyword{ dynamic }