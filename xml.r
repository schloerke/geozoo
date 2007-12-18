source("/Users/barret/Sites/Geometric Website/geometric-data/cube/codes/writeXML.R")

write.xml <- function(object.function,filepath,title) {

f.writeXML(dat1= object.function$points,filename= filepath,data.num=2,data.name= title,dat1.name="vertices",dat2=object.function$edges,dat2.source=object.function$edges[,1],dat2.destination=object.function$edges[,2],dat2.name="edges", default.color="5")
}
