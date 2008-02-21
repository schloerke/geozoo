## Torus
torus <- function(p = 3,n = 10000){
	radius <- (2 ^ ((p - 2):0))
	vert <- matrix(do.call("rbind", as.list(replicate(n, .torus.row(radius,p)))),ncol = p,byrow = TRUE)
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

.torus.row <-function(radius,p) {
	##Generates Angles
	t <- runif(p-1, min = 0, max = 2 * pi)

	##Generates Row of Data
	torus <- c(
		rep(cos(t[p-1]) * radius[p-1], p-1), 
		sin(t[p-1]) * radius[p-1]
	)
	
	if(p>2)
	for (i in (p-1):2) {
		for(j in (i-1):1){
			torus[j] <- (torus[j] + radius[i-1]) * cos(t[i-1])
		}
		torus[i] <- (torus[i]+radius[i-1]) * sin(t[i-1])
	}
	torus
}

##Flat Torus
torus.flat <-function(p = 4,n = 10000){
	p <- floor(p / 2)
	vert <- do.call("rbind", replicate(n,.torus.flat.row(p), simplify = FALSE))
	wires <- NULL
	structure(
		list(points = vert, edges = wires),
		class = "geozoo"
	)

}

.torus.flat.row <-function(p){
	a <-runif(p,min = 0,max = 2 * pi)
	as.vector(rbind(cos(a),sin(a)))
}
