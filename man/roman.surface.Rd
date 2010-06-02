\name{roman.surface}
\alias{roman.surface}
\title{ Roman Surface }
\description{
A function to generate a Roman surface, also known as a Steiner surface.
}
\usage{
roman.surface(n = 10000, a = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ number of points }
  \item{a}{ maximum radius of object }
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/} }
\author{ Barret Schloerke }
\examples{
## Generates a Roman surface.
roman.surface(n = 1000, a = 1)

\dontrun{
## Through rggobi
ggobi(roman.surface(n = 10000, a = 1))
}}
\keyword{ dynamic }