## Sphere
.norm <-function(x) {
	x <- sqrt(sum(x ^ 2))
	x
}
.norm.vec <-function(x) {
	x <- x/	sqrt(sum(x ^ 2))
	x
}

sphere.hollow <-function(p, n = p * 500) {
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
sphere.solid.random <-function(p, n = p * 500) {
	sphere <- sphere.hollow(p,n)$points
	vert <- sphere * runif(n) ^ (1/p)
	wires <- NULL
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}
