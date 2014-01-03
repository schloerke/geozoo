##Cube Vertice and Wire Function
#' @keywords internal
.cube.vertices <- function(p){
	cube.vertices <- do.call(expand.grid, rep(list(c(0,1)), p))
	as.matrix(cube.vertices)
}

#' @keywords internal
.cube.wires <- function(p) {
	vertices <- 0:(2 ^ p - 1)
	from <- vertices[rep(1:length(vertices), each = p)]
	edges <- 2 ^ (0:(p-1))
	to <- bitXor(from, edges)
	cube.wires <- subset(data.frame(from, to), from < to) + 1
	row.names(cube.wires) <- 1:nrow(cube.wires)
	cube.wires <- as.matrix(cube.wires)
	cube.wires
}

#' Cube
#'
#' A function generate a cube with vertices and a wire frame
#'
#' @param p dimension of object
#' @return
#'  \item{points }{location of points}
#'  \item{edges }{edges of the object}
#' @references \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/cube/}
#' @author Barret Schloerke
#' @examples
#' ## Generates a cube
#' cube.iterate(p = 3)
#'
#' @keywords dynamic
#' @export
cube.iterate <- function(p = 3){
	vert <- .cube.vertices(p)
	wires <- .cube.wires(p)
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)
}

#' Solid Cube
#'
#' A function to generate a solid cube with random points
#'
#' @param p dimension of object
#' @param n number of points
#' @return
#'  \item{points }{location of points}
#'  \item{edges }{edges of the object}
#' @references \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/cube/}
#' @author Barret Schloerke
#' @examples
#' ## Generates a solid cube with random points
#' cube.solid.random(p = 3, n = 1000)
#'
#' @keywords dynamic
#' @export
cube.solid.random <- function(p, n = 850 * 2^p){
  	n <- min(n, 75000)
	suppressWarnings(solid <- matrix( runif(n * p), ncol = p))

	vert <- rbind(.cube.vertices(p), solid)
	wires <- .cube.wires(p)

	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)
}



#' Equidistant Solid Cube
#'
#' A function to generate a solid cube with equidistant points
#'
#' @param p dimension of object
#' @param n length of number of points in each dimension
#' @return
#'  \item{points }{location of points}
#'  \item{edges }{edges of the object}
#' @references \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/cube/}
#' @author Barret Schloerke
#' @examples
#' ## Generates a solid cube with equidistant points
#' cube.solid.grid(p = 3, n = 8)
#'
#' @keywords dynamic
#' @export
cube.solid.grid <- function(p = 3,n = 8){
	cube.verts <- do.call(expand.grid, rep(list(c((0:n)/n)),p))
	vert <- unique(rbind(.cube.vertices(p),as.matrix(cube.verts)))
	wires <- .cube.wires(p)
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)
}



#' Cube with points on the 'face'
#'
#' A function to generate a cube with points on its face
#'
#' @param p dimension of object
#' @return
#'  \item{points }{location of points}
#'  \item{edges }{edges of the object}
#' @references \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/cube/}
#' @author Barret Schloerke
#' @examples
#' ## Generates a cube with points on its face
#' cube.face(p = 3)
#'
#' @keywords dynamic
#' @export
cube.face <- function(p = 3){
	cube.verts <- .cube.vertices(p)
	tmp <- NULL
	faces <- NULL
	for (i in 1:p) {
		tmp <- matrix( runif(1890 * p),ncol = p)
		tmp[1:(945), i] <- 0
		tmp[(946):1890, i] <- 1
		faces <- rbind(faces, tmp)
	}
	vert <- rbind(cube.verts,faces)

	wires <- .cube.wires(p)
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}



#' Cube with points along the wire frame
#'
#' A function to generate a cube with points on its face
#'
#' @param p dimension of object
#' @return
#'  \item{points }{location of points}
#'  \item{edges }{edges of the object}
#' @references \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/cube/}
#' @author Barret Schloerke
#' @examples
#' ## Generates a cube with points along its wire frame
#' cube.dotline(p = 3)
#'
#' @keywords dynamic
#' @export
cube.dotline <- function(p = 3){
	cube.verts <- .cube.vertices(p)

	cube.wire <- .cube.wires(p)

	dot.lines <-NULL
	n <- 8
	for (j in 1:(nrow(cube.wire))) {
		x <- cube.wire[j,2]
		y <- cube.wire[j,1]
		d1 <- (cube.verts[x,] - cube.verts[y,])
		d3 <- sum((d1) ^ 2)
		if (d3 == 1) {
			d2 <- sum(abs(d1))
			if (d2 == 1){
				for (k in 1:p) {
					if (d1[k] == 1){
						tmp <- matrix(rep(cube.verts[y,],n+1), ncol = p, byrow = TRUE)
						tmp[2:(n+1),k] <- seq(length = (n), from = (0+1/(n+1)), by = (1/(n+1)))
						dot.lines <- rbind(dot.lines,tmp[2:(n+1),])
					}
				}
			}
		}
	}
	vert <- rbind(cube.verts,dot.lines)
	structure(
		list(points = vert, edges = cube.wire),
		class = "geozoo"
	)

}
