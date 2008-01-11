##Parametric Eqn's

## Boy Surface
boy.surface <- function( n = 10000){
	vert <- matrix( do.call( "rbind", as.list( replicate( n, .boy.surface.row( )))), ncol = 3, byrow = TRUE)
	wires <- NULL

	structure( 
		list( points = vert, edges = wires), 
		class = "geozoo"
	)
}

.boy.surface.row <- function( ){
	u <- runif( 1, min = 0, max = pi)
	v <- runif( 1, min = 0, max = pi)
	a <- cos( v) * sin( u)
	b <- sin( v) * sin( u)
	c <- cos( u)
	x <- 1 / 2 * ( ( 2 * a ^ 2 - b ^ 2 - c ^ 2) + 2 * b * c * ( b ^ 2 - c ^ 2) + c * a * ( a ^ 2 - c ^ 2) + a * b * ( b ^ 2 - a ^ 2))
	y <- sqrt( 3) / 2 * ( ( b ^ 2 - c ^ 2) + c * a * ( c ^ 2 - a ^ 2) + a * b * ( b ^ 2 - a ^ 2))
	z <- ( a + b + c) * ( ( a + b + c) ^ 3 + 4 * ( b - a) * ( c - b) * ( a - c))
	z <- z / 8
	return( cbind( x, y, z))
}


## Conic Spiral
conic.spiral <- function( n = 10000, a = .2, b = 1, c = .1, w = 2){
	vert <- matrix( do.call( "rbind", as.list( replicate( n, .conic.spiral.row( a, b, c, w)))), ncol = 3, byrow = TRUE)
	wires <- NULL

	structure( 
		list( points = vert, edges = wires), 
		class = "geozoo"
	)
}

.conic.spiral.row <- function( a, b, c, w) {
	u <- runif( 1, min = 0, max = 2 * pi)
	v <- runif( 1, min = 0, max = 2 * pi)

	x <- a * ( 1 - v / ( 2 * pi)) * cos( w * v) * ( 1 + cos( u)) + c * cos( w * v)
	y <- a * ( 1 - v / ( 2 * pi)) * sin( w * v) * ( 1 + cos( u)) + c * sin( w * v)
	z <- ( b * v + a * ( 1 - v / ( 2 * pi)) * sin( u)) / ( 2 * pi)
	return( cbind( x, y, z))
}



## Conic Spiral Nautilus
conic.spiral.nautilus <- function( n = 10000, a = .2, b = .1, c = 0, w = 2){
	vert <- matrix( do.call( "rbind", as.list( replicate( n, .conic.spiral.row( a, b, c, w)))), ncol = 3, byrow = TRUE)
	wires <- NULL

	structure( 
		list( points = vert, edges = wires), 
		class = "geozoo"
	)
}


## Cross Cap
cross.cap <- function( n = 10000){
	vert <- matrix( do.call( "rbind", as.list( replicate( n, .cross.cap.row( )))), ncol = 3, byrow = TRUE)
	wires <- NULL

	structure( 
		list( points = vert, edges = wires), 
		class = "geozoo"
	)
}

.cross.cap.row <- function( ) {
	u <- runif( 1, min = 0, max = pi)
	v <- runif( 1, min = 0, max = pi)
	x <- cos( u) * sin( 2 * v)
	y <- sin( u) * sin( 2 * v)
	z <- cos( v) * cos( v) - cos( u) * cos( u) * sin( v) * sin( v)
	return( cbind( x, y, z))
}


## Dini Surface
dini.surface <- function( n = 10000, a = 1, b = 1){
	vert <- matrix( do.call( "rbind", as.list( replicate( n, .dini.surface.row( a, b)))), ncol = 3, byrow = TRUE)
	wires <- NULL

	structure( 
		list( points = vert, edges = wires), 
		class = "geozoo"
	)
}

.dini.surface.row <- function( a = 1, b = 1) {
	u <- runif( 1, min = 0, max = 4 * pi)
	v <- runif( 1, min = 0.0000000001, max = 2)
	x <- a * cos( u) * sin( v)
	y <- a * sin( u) * sin( v)
	z <- a * ( cos(v) + log(tan(v/2))) + b*u
	return( cbind( x, y, z))
}


## Ellipsoid
ellipsoid <- function( n = 10000, a = 1, b = 1, c = 3){
	vert <- matrix( do.call( "rbind", as.list( replicate( n, .ellipsoid.row( a, b, c)))), ncol = 3, byrow = TRUE)
	wires <- NULL

	structure( 
		list( points = vert, edges = wires), 
		class = "geozoo"
	)
}

.ellipsoid.row <- function( a, b, c) {
	u <- runif( 1, min = 0, max = 2 * pi)
	v <- runif( 1, min = 0, max = 2 * pi)
	x <- a * cos( u) * sin( v)
	y <- b * sin( u) * sin( v)
	z <- c * cos( v)
	return( cbind( x, y, z))
}


## Enneper
enneper.surface <- function( n = 10000, a = 4){
	vert <- matrix( do.call( "rbind", as.list( replicate( n, .enneper.surface.row( a)))), ncol = 3, byrow = TRUE)
	wires <- NULL

	structure( 
		list( points = vert, edges = wires), 
		class = "geozoo"
	)
}

.enneper.surface.row <- function( a = 4) {
	u <- runif( 1, min = - a, max = a)
	v <- runif( 1, min = - a, max = a)
	x <- u - u ^ 3 / 3 + u * v ^ 2
	y <- v - v ^ 3 / 3 + v * u ^ 2
	z <- u ^ 2 - v ^ 2
	return( cbind( x, y, z))
}



## Klein Figure Eight
klein.fig.eight <- function( n = 10000, a = 3, b = 1){
	vert <- matrix( do.call( "rbind", as.list( replicate( n, .klein.fig.eight.row( a, b)))), ncol = 4, byrow = TRUE)
	wires <- NULL

	structure( 
		list( points = vert, edges = wires), 
		class = "geozoo"
	)
}

.klein.fig.eight.row <- function( a = 3, b = 1) {
	u <- runif( 1, min = - pi, max = pi)
	v <- runif( 1, min = - pi, max = pi)
	x <- ( b * cos( v) + a) * cos( u)
	y <- ( b * cos( v) + a) * sin( u)
	z <- b * sin( v) * cos( u / 2)
	w <- b * sin( v) * sin( u / 2)
	return( cbind( x, y, z, w))
}



## Roman Surface
roman.surface <- function( n = 10000, a = 1){
	vert <- matrix( do.call( "rbind", as.list( replicate( n, .roman.surface.row( a)))), ncol = 3, byrow = TRUE)
	wires <- NULL

	structure( 
		list( points = vert, edges = wires), 
		class = "geozoo"
	)
}
.roman.surface.row <- function( a = 1) {
	u <- runif( 1, min = 0, max = pi)
	v <- runif( 1, min = 0, max = pi)
	x <- a ^ 2 * cos( v) * cos( v) * sin( 2 * u) / 2
	y <- a ^ 2 * sin( u) * sin( 2 * v) / 2
	z <- a ^ 2 * cos( u) * sin( 2 * v) / 2
	return( cbind( x, y, z))
}
