source("/Users/barret/Sites/Geometric Website/geometric-data/cube/codes/writeXML.R")

write.xml <- function(object.function,filepath,title) {

object.function

f.writeXML(dat1= vert,filename= filepath,data.num=2,data.name= title,dat1.name="vertices",dat2=wires,dat2.source=wires[,1],dat2.destination=wires[,2],dat2.name="edges", default.color="5")
}
