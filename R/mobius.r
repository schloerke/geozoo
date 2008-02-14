## Mobius
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

.mobius.row<- function(p){

	##Generates Angles
	a <- runif(1,min=0,max=2*pi)
	a<-c(a,a/2)

	##Generates Small Radius
	radius<-c(1,runif(1,min=-.4,max=.4))

	##Generates Row of Data
	mobius<-c((cos(a[2])*radius[2]+radius[1])*cos(a[1]),(cos(a[2])*radius[2]+radius[1])*sin(a[1]),sin(a[2])*radius[2])

	mobius
}

## Mobius
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