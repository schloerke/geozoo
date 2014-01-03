## Sphere
#' @keywords internal
.norm <-function(x) {
	x <- sqrt(sum(x ^ 2))
	x
}
#' @keywords internal
.norm.vec <-function(x) {
	x <- x/	sqrt(sum(x ^ 2))
	x
}

#' Sphere
#'
#' A function to generate a sphere with points on the surface
#'
#' @param p dimension of object
#' @param n number of points
#' @return
#'  \item{points }{location of points}
#'  \item{edges }{edges of the object (null)}
#' @references \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/sphere/}
#' @author Barret Schloerke
#' @examples
#' ## Generates a sphere with points on the surface
#' sphere.hollow(p = 3, n = 1000)
#'
#' @keywords dynamic
#' @export
sphere.hollow <-function(p, n = p * 500) {
	tmp <- matrix(rnorm(n * p),ncol = p)
	vert <- t(apply(tmp,1,.norm.vec))
	wires <- NULL
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)
}


#' Solid Sphere with Equidistant Points
#'
#' A function to generate a solid sphere with equidistant points.
#'
#' @param p dimension of object
#' @param n maximum number of points in the diameter
#' @return
#'  \item{points }{location of points}
#'  \item{edges }{edges of the object (null)}
#' @references \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/sphere/}
#' @author Barret Schloerke
#' @examples
#' ## Generates a solid sphere with equidistant points
#' sphere.solid.grid(p = 3, n = 8)
#'
#' @keywords dynamic
#' @export
sphere.solid.grid <- function(p = 3,n = 8){
	cube.solid.grid <- do.call(expand.grid, rep(list(c((0:n)/n)),p))
	cube.solid.grid <- as.matrix(cube.solid.grid)

	cube.solid.grid <- cube.solid.grid - .5
	sphere <- NULL

	for( i in 1:nrow(cube.solid.grid)) {
		tmp <- cube.solid.grid[i,]
		if (.norm(tmp) <= (1/2)){
			sphere <- rbind(sphere,tmp)
		}
	}
	vert <- sphere
	wires <- NULL
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}

#' Solid sphere with Random Points
#'
#' A function to generate a solid sphere with random points
#'
#' @param p dimension of object
#' @param n number of points
#' @return
#'  \item{points }{location of points}
#'  \item{edges }{edges of the object (null)}
#' @references \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/sphere/}
#' @author Barret Schloerke
#' @examples
#' ## Generates a solid sphere with random points.
#' sphere.solid.random(p = 3, n = 1000)
#'
#' @keywords dynamic
#' @export
sphere.solid.random <-function(p, n = p * 500) {
	sphere <- sphere.hollow(p,n)$points
	vert <- sphere * runif(n) ^ (1/p)
	wires <- NULL
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}
