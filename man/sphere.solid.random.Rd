\name{sphere.solid.random}
\alias{sphere.solid.random}
\title{ Solid sphere with Random Points }
\description{
A function to generate a solid sphere with random points.
}
\usage{
sphere.solid.random(p, n = p * 500)
}
\arguments{
  \item{p}{ dimension of object }
  \item{n}{ number of points }
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/sphere/} }
\author{ Barret Schloerke }
\examples{
## Generates a solid sphere with random points.
sphere.solid.random(p = 3, n = 1000)

\dontrun{
## Through rggobi
ggobi(sphere.solid.random(p = 5))
}}
\keyword{ dynamic }