.cross.polytope.verts <-function(p){
	rbind(diag(1,p),diag(-1,p))
}

.cross.polytope.wires <- function(p){
	a <- do.call(expand.grid, rep(list(1:(2*p)), 2))
	b <- a[ (a[,1]) != (a[,2] +p) , ]
	as.matrix(b[ b[,1] > b[,2] , ])
}

cross.polytope <- function(p = 3){

structure(
		list(points = .cross.polytope.verts(p), edges = .cross.polytope.wires(p)),
		class = "geozoo"
	)
}