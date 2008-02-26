\name{ggobi.geozoo}
\alias{ggobi.geozoo}
\title{GGobi adaptation for GeoZoo}
\description{
  Allows GeoZoo objects to be put into GGobi through rggobi with a pre-specified color and 2D Tour mode.
  \code{ggobi}.
}
\usage{
ggobi.geozoo(data, ...)
}
\arguments{
  \item{data}{The geozoo data object to view}
  \item{...}{}
}
\seealso{
}
\examples{
ggobi(cube.iterate(p = 3))

ggobi(simplex(p=4))
}
\keyword{dynamic}