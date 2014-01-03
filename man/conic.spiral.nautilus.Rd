\name{conic.spiral.nautilus}
\alias{conic.spiral.nautilus}
\title{Conic Spiral (Nautilus Shape)}
\usage{
  conic.spiral.nautilus(n = 10000, a = 0.2, b = 0.1, c = 0,
    w = 2)
}
\arguments{
  \item{n}{number of points}

  \item{a}{final radius of cone}

  \item{b}{height of object}

  \item{c}{inner radius}

  \item{w}{number of spirals}
}
\value{
  \item{points }{location of points} \item{edges }{edges of
  the object (null)}
}
\description{
  A function to produce a Conic Spiral in a nautilus shape
}
\examples{
## Generates a Nautilus Conic Spiral
conic.spiral.nautilus( n = 1000 )
}
\author{
  Barret Schloerke
}
\references{
  \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/}
}
\keyword{dynamic}

