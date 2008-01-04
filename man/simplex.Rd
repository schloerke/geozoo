\name{simplex}
\alias{simplex}
\title{ Simplex }
\description{
A function to generate a simplex.
}
\usage{
simplex(p = 3)
}
\arguments{
  \item{p}{ dimension of object }
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/simplex/} }
\author{ Barret Schloerke }
\examples{
## Generates a simplex
simplex(p = 3)

## Through rggobi
ggobi(simplex(p = 3))
}
\keyword{ dynamic }