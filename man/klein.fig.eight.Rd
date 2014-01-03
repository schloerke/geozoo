\name{klein.fig.eight}
\alias{klein.fig.eight}
\title{Figure Eight Klein Bottle}
\usage{
  klein.fig.eight(n = 10000, a = 3, b = 1)
}
\arguments{
  \item{n}{number of points}

  \item{a}{radius of outer radius}

  \item{b}{radius of inner radius}
}
\value{
  \item{points }{location of points} \item{edges }{edges of
  the object (null)}
}
\description{
  A function to generate a figure eight Klein bottle
}
\examples{
## Generates a figure eight Klein bottle.
klein.fig.eight(n = 1000, a = 3, b = 1)
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/}
}
\keyword{dynamic}

