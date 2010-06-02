\name{boy.surface}
\alias{boy.surface}
\title{ Boy Surface for GeoZoo}
\description{
A function to produce a Boy Surface. 
}
\usage{
boy.surface(n = 10000)

}
\arguments{
  \item{n}{ the number of points used when creating the object.   }
}
\seealso{\url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/other/}}
\author{ Barret Schloerke }
\examples{
## Generates a Boy Surface
boy.surface( n = 1000 )

\dontrun{
## Through rggobi
ggobi(boy.surface(n = 10000))
}}
\keyword{ dynamic }
