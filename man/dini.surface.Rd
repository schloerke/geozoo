\name{dini.surface}
\alias{dini.surface}
\title{Dini Surface}
\usage{
  dini.surface(n = 10000, a = 1, b = 1)
}
\arguments{
  \item{n}{number of points}

  \item{a}{outer radius of object}

  \item{b}{space between loops}
}
\value{
  \item{points }{location of points} \item{edges }{edges of
  the object (null)}
}
\description{
  A function to generate a dini surface.
}
\examples{
## Generates a Dini Surface
dini.surface(n = 1000, a = 1, b = 1)
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/}
}
\keyword{dynamic}

