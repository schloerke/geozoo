## Sphere
.norm <-function(x) { 
	sqrt(sum(x ^ 2))
}

.norm.vec <-function(x) {
	x <- x/.norm(x)
	x
}

sphere <-function(p = 3) {
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
sphere.solid.eq <- function(p = 3,n = 8){
	cube.solid.eq <- do.call(expand.grid, rep(list(c((0:n)/n)),p))
	cube.solid.eq <- as.matrix(cube.solid.eq)

	cube.solid.eq <- cube.solid.eq - .5
	sphere.solid.eq <- NULL

	for( i in 1:nrow(cube.solid.eq)) {
		tmp <- cube.solid.eq[i,]
		if (.norm(tmp) <= (1/2)){
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
	tmp2 <- t(apply(tmp,1,.norm.vec))
	sphere.solid <- tmp2 * runif(n) ^ (1/p)
	vert <- sphere.solid
	wires <- NULL
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}
