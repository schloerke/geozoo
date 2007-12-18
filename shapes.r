library(rggobi)

ggobi.geozoo <- function(data){
	gg <- ggobi(data$points)
	glyph_colour(gg[1]) <- 6
	glyph_size(gg[1]) <- 3

	if(!is.null(data$edges)) {
		edges(gg) <- data$edges
		glyph_colour(gg[2]) <- 6
	}
	close(displays(gg)[[1]])
	g.display <- display(gg[1], "2D Tour")
	edges(g.display) <- gg[2]
	
#	invisible(gg)
}

##Cube Vertice and Wire Function
cube.vertices <- function(p){
	cube.vertices <- do.call(expand.grid, rep(list(c(0,1)), p))
	cube.vertices <- as.matrix(cube.vertices)
	cube.vertices
}
cube.wires <-function(p){
	library(bitops)
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
cube <- function(p = 3){
	vert <- cube.vertices(p)
	wires <- cube.wires(p)
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}



## Cube Solid
cube.solid <- function(p = 3){
	M <- 85082 ^ p
	if (M >= 75000)
		M <- 75000
	suppressWarnings(solid <- matrix( runif(850 * 2 ^ p), ncol = p))
	vert <- rbind(cube.vertices(p),solid)
	
	wires <- cube.wires(p)

	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}



##Cube SolidEQ
cube.solid.eq <- function(p = 3,n = 8){
	cube.vertices <- do.call(expand.grid, rep(list(c((0:n)/n)),p))
	vert <- unique(rbind(cube.vertices(p),as.matrix(cube.vertices)))
	wires <- cube.wires(p)
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)
}



## Cube Face
cube.face <- function(p = 3){
	cube.verts <- cube.vertices(p)
	tmp <- NULL
	faces <- NULL
	for (i in 1:p) {
		tmp <- matrix( runif(1890 * p),ncol = p)
		tmp[1:(945), i] <- 0
		tmp[(946):1890, i] <- 1
		faces <- rbind(faces, tmp)
	}
	vert <- rbind(cube.verts,faces)	

	wires <- cube.wires(p)
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}



## Cube Dot Line
cube.dotline <- function(p = 3){
	cube.verts <- cube.vertices(p)

	cube.wire <- cube.wires(p)
	
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



## Sphere
norm <-function(x) { 
	sqrt(sum(x ^ 2))
	}
norm.vec <-function(x) {
	x <- x/norm(x)
	x
	}

sphere <-function(p = 3) {
	n <- 500
	tmp <- matrix(rnorm(n * p),ncol = p)
	vert <- t(apply(tmp,1,norm.vec))
	wires <- NULL
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}



##Sphere Solid-EQ
sphere.solid.eq <- function(p = 3,n = 8){
	cube.solid.eq <- do.call(expand.grid, rep(list(c((0:n)/n)),p))
	cube.solid.eq <- as.matrix(cube.solid.eq)

	cube.solid.eq <- cube.solid.eq - .5
	sphere.solid.eq <- NULL

	for( i in 1:nrow(cube.solid.eq)) {
		tmp <- cube.solid.eq[i,]
		if (norm(tmp) <= (1/2)){
			sphere.solid.eq <- rbind(sphere.solid.eq,tmp)
		}
	}
	vert <- sphere.solid.eq
	wires <- NULL
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}



## Sphere Solid
sphere.solid <-function(p = 3) {
	n <- p * 500
	tmp <- matrix(rnorm(n * p),ncol = p)
	tmp2 <- t(apply(tmp,1,norm.vec))
	sphere.solid <- tmp2 * runif(n) ^ (1/p)
	vert <- sphere.solid
	wires <- NULL
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}




## Simplex
f.helmert <- function(d){
	helmert <- rep(1/sqrt(d), d)
	for(i in 1:(d-1)){
		x <- rep(1/sqrt(i*(i+1)), i)
		x <- c(x, -i/sqrt(i*(i+1)))
		x <- c(x, rep(0, d - i - 1))
		helmert <- rbind(helmert, x)
	}
	return(helmert)
}
f.composition <- function(data){
	d <- dim(data)[2]
	hm <- f.helmert(d)
	x <- data - matrix(1/d, dim(data)[1], d)
	return((x %*% t(hm))[,-1])
}

simplex <- function(p = 3){
	vert <- f.composition(diag(p+1))

	wires <- do.call(expand.grid, list(c(1:nrow(vert)),c(1:nrow(vert))))

	structure(
		list(points = vert, edges = wires[!wires[,1]==wires[,2],]),
		class = "geozoo"
	)
}


## Torus
torus <- function(p = 3,n = 10000){
	radius <- (2 ^ ((p - 2):0))
	vert <- matrix(do.call("rbind", as.list(replicate(n, torus.row(radius,p)))),ncol = p,byrow = TRUE)
	wires <- NULL
	cat("\n",
		"NOTE:",
		"\n","\t","On the Main Window:  Tools > Variable Manipulation ",
		"\n","\t","1.  Highlight all rows","\n","\t","2.  Click 'Limits...' at the bottom","\n","\t","3.  Make 'Minimum' and 'Maximum' the global minimum and maximum of the object.","\n","\n")

	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)
	}
torus.row <-function(radius,p) {
	##Generates Angles
	angles <- runif(p-1, min = 0, max = 2 * pi)

	##Generates Row of Data
	torus <- c(rep(cos(angles[p-1]) * radius[p-1], p-1), sin(angles[p-1]) * radius[p-1])
	
	if(p>2)
	for (i in (p-1):2) {
		for(j in (i-1):1){
			torus[j] <- (torus[j] + radius[i-1]) * cos(angles[i-1])
		}
		torus[i] <- (torus[i]+radius[i-1]) * sin(angles[i-1])
	}
	torus
}



##Flat Torus
torus.flat <-function(p = 4,n = 10000){
	p <- floor(p / 2)
	vert <- do.call("rbind", replicate(n,torus.flat.row(p), simplify = FALSE))
	wires <- NULL
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}
torus.flat.row <-function(p){
	a <-runif(p,min = 0,max = 2 * pi)
	as.vector(rbind(cos(a),sin(a)))
}










