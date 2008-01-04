\name{cross.cap}
\alias{cross.cap}
\title{Cross Cap}
\description{
A function to generate a cross cap.
}
\usage{
cross.cap(n = 10000)
}
\arguments{
  \item{n}{ number of points used to generate the object. }
}
\value{
  \item{points }{location of points}
  \item{edges }{edges of the object (null)}
}
\references{ \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/} }
\author{ Barret Schloerke }
\examples{
## Generates a Cross Cap
cross.cap( n = 10000 )

## Through rggobi
ggobi(cross.cap(n = 10000))
}
\keyword{ dynamic }