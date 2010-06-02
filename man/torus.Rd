\name{torus}
\alias{torus}
\title{ Torus }
\description{
A function to generate a torus in any dimension.
}
\usage{
torus(p = 3, n = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{ dimension of object }
  \item{n}{ number of points }
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/torus/} }
\author{ Barret Schloerke }
\examples{
## Generates a torus.
torus(p = 3, n = 1000)

\dontrun{
## Through rggobi
ggobi(torus(p = 4, n = 5000))
}
}
\keyword{ dynamic }