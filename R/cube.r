##Cube Vertice and Wire Function
.cube.vertices <- function(p){
	cube.vertices <- do.call(expand.grid, rep(list(c(0,1)), p))
	as.matrix(cube.vertices)
}

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

##CUBE
cube.iterate <- function(p = 3){
	vert <- .cube.vertices(p)
	wires <- .cube.wires(p)
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)
}

## Cube Solid
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



##Cube SolidEQ
cube.solid.grid <- function(p = 3,n = 8){
	cube.verts <- do.call(expand.grid, rep(list(c((0:n)/n)),p))
	vert <- unique(rbind(.cube.vertices(p),as.matrix(cube.verts)))
	wires <- .cube.wires(p)
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)
}



## Cube Face
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



## Cube Dot Line
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