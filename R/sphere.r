## Sphere
.norm <-function(x) { 
	sqrt(sum(x ^ 2))
}

.norm.vec <-function(x) {
	x <- x/.norm(x)
	x
}

sphere.hollow <-function(p = 3) {
	n <- 500
	tmp <- matrix(rnorm(n * p),ncol = p)
	vert <- t(apply(tmp,1,.norm.vec))
	wires <- NULL
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)
}

##Sphere Solid-EQ
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

## Sphere Solid
sphere.solid.random <-function(p = 3) {
	n <- p * 500
	tmp <- matrix(rnorm(n * p),ncol = p)
	tmp2 <- t(apply(tmp,1,.norm.vec))
	sphere.solid <- tmp2 * runif(n) ^ (1/p)
	vert <- sphere.solid
	wires <- NULL
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}
