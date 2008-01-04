\name{dini.surface}
\alias{dini.surface}
\title{ Dini Surface}
\description{
A function to generate a dini surface.
}
\usage{
dini.surface(n = 10000, a = 1, b = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{ number of points }
  \item{a}{ outer radius of object }
  \item{b}{ space between loops }
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{points }{location of points}
  \item{edges }{edges of the object}
  ...
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/cube/} }
\author{ Barret Schloerke }
\examples{
## Generates a Dini surface
dini.surface(n = 10000, a = 1, b = 1)

## Through rggobi
ggobi(dini.surface(n = 10000, a = 1, b = 1))
}
\keyword{ dynamic }