\name{ggobi}
\alias{ggobi}
\title{GGobi adaptation for GeoZoo}
\description{
  Allows GeoZoo objects to be put into GGobi through rggobi with a prespified color and 2D Tour mode.
  \code{ggobi}.
}
\usage{
ggobi(object.function())
}
\arguments{
  \item{file}{a connection or a character string giving the
    name of the file to load.}
  \item{envir}{the environment where the data should be
    loaded.}
}
\seealso{
#  \code{\link{save}}.
}
\examples{
## 3-D Cube
ggobi(cube(p=3))

## 4-D Solid Sphere
ggobi(sphere.solid(p=4))

}
\keyword{ggobi}