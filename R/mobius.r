#' Mobius
#'
#' A function to generate a mobius strip in the third or fourth dimension.
#'
#' @param p dimension of object.  (3)
#' @param n number of points
#' @references \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/mobius/}
#' @author Barret Schloerke
#' @examples
#' ## Generates a mobius strip.
#' mobius(3, n = 1000)
#'
#' @keywords dynamic
#' @export
mobius <- function(p=3,n=10000){
	cat("\n",
		"NOTE:",
		"\n","\t","On the Main Window:  Tools > Variable Manipulation ",
		"\n","\t","1.  Highlight all rows","\n","\t","2.  Click 'Limits...' at the bottom","\n","\t","3.  Make 'Minimum' and 'Maximum' the global minimum and maximum of the object.","\n","\n")

	vert<-matrix(do.call("rbind", as.list(replicate(n, .mobius.row(3)))),ncol=3,byrow=TRUE)
	wires <- NULL

	structure(
		list( points = vert, edges = wires),
		class = "geozoo"
	)
	}

#' @keywords internal
.mobius.row <- function(p) {

	##Generates Angles
	a <- runif(1,min=0,max=2*pi)
	a<-c(a,a/2)

	##Generates Small Radius
	radius<-c(1,runif(1,min=-.4,max=.4))

	##Generates Row of Data
	mobius<-c((cos(a[2])*radius[2]+radius[1])*cos(a[1]),(cos(a[2])*radius[2]+radius[1])*sin(a[1]),sin(a[2])*radius[2])

	mobius
}

#' Mobius Experiment
#'
#' A function to generate a 5-D mobius strip in the third dimension.
#'
#' @param p dimension of object.  (5)
#' @param n number of points
#' @references \url{http://streaming.stat.iastate.edu/~dicook/geometric-data/mobius/mobius/}
#' @author Barret Schloerke
#' @examples
#' ## Generates a mobius strip.
#' mobius.experiment(5, n = 1000)
#'
#' @keywords dynamic
#' @export
mobius.experiment <- function(p=5,n=10000){
	cat("\n",
		"NOTE:",
		"\n","\t","On the Main Window:  Tools > Variable Manipulation ",
		"\n","\t","1.  Highlight all rows","\n","\t","2.  Click 'Limits...' at the bottom","\n","\t","3.  Make 'Minimum' and 'Maximum' the global minimum and maximum of the object.","\n","\n")
	p = 5
	vert<-matrix(do.call("rbind", as.list(replicate(n, .mobius.experiment.row()))),ncol=3,byrow=TRUE)
	wires <- NULL

	structure(
		list( points = vert, edges = wires),
		class = "geozoo"
	)
	}

#' @keywords internal
.mobius.experiment.row<- function(){

	##Generates Angles
	a <- runif(1,min=0,max=2*pi)
	a<-c(a,a/2)

	##Generates Small Radius
	radius<-c(1,runif(1,min=-.4,max=.4))

	##Generates Row of Data
	mobius<-c((cos(a[2])*radius[2]+radius[1])*cos(a[1]),(cos(a[2])*radius[2]+radius[1])*sin(a[1]),sin(a[2])*radius[2])

	k<-runif(1,min=0,max=pi)
	## Rot over x axis
		rot.1 <- matrix(c(0,cos(k),-sin(k),1,0,0,0,sin(k),cos(k)),ncol=3,byrow=TRUE)
	## Rot over z axis
		rot.2 <- matrix(c(cos(2*k),-sin(2*k),0,sin(2*k),cos(2*k),0,0,0,1),ncol=3,byrow=TRUE)
	## Trans perpendicular to z axis
		trans <- matrix(c(4*cos(2*k),4*sin(2*k),0),ncol=1)

	mobius<-rot.2%*%rot.1%*%mobius+trans


	mobius
}
