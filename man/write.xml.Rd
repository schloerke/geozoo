\name{write.xml}
\alias{write.xml}
\title{ Write XML's }
\description{
A function that allows the user to write an object into an .xml.
}
\usage{
write.xml(object.function, filepath, title)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object.function}{ i.e. cube() or sphere() }
  \item{filepath}{ where you would like to save the file in quotes }
  \item{title}{ title of the file in quotes }
}
\author{ Barret Schloerke }
\examples{
## Saves a cube in the fourth dimension to the folder "example" that already exists with a title of "Cube-4"
write.xml(cube(p=4),"/example","Cube-4")
}
\keyword{ dynamic }