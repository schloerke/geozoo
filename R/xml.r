write.xml <- function(object.function,filepath,title) {
  .f.writeXML(
    dat1 = object.function$points,
    filename = filepath,
    data.num = 2,
    data.name = title,
    dat1.name = "vertices",
    dat2 = object.function$edges,
    dat2.source = object.function$edges[,1],
    dat2.destination = object.function$edges[,2],
    dat2.name = "edges",
    default.color = "5"
  )
}

.f.writeXML<-function(dat1,filename,data.num=1,data.name="data",
  default.color="5",default.glyph="fc 2",catvars1=NULL,
  dat1.colors=NULL,dat1.glyphs=NULL,dat1.id=NULL,
  dat1.description=NULL,dat1.name="Data 1",
  dat2=NULL,catvars2=NULL,dat2.colors=NULL,dat2.glyphs=NULL,
    dat2.id=NULL,dat2.source=NULL,
    dat2.destination=NULL,dat2.description=NULL,dat2.name="Data 2",
  dat3=NULL,catvars3=NULL,dat3.colors=NULL,dat3.glyphs=NULL,
    dat3.id=NULL,dat3.description=NULL,dat3.name="Data 3") 
{
# Write the header information
cat(sep="","<?xml version=\"1.0\"?>\n<!DOCTYPE ggobidata SYSTEM \"ggobi.dtd\">\n",file=filename)
cat(sep="","<ggobidata count=\"",data.num,"\">\n",file=filename,append=T)
cat(sep="","<data name=\"",dat1.name,"\">\n",file=filename,append=T)
cat(sep="","<description>\n",file=filename,append=T)
cat(sep="",dat1.description,"\n",file=filename,append=T)
cat(sep="","</description>\n",file=filename,append=T)
p1<-ncol(dat1)
n1<-nrow(dat1)
cat(p1,n1,"\n")
var.name1<-colnames(dat1)
if (is.null(var.name1))
  for (i in 1:p1)
    var.name1<-c(var.name1,paste("Var ",i))
cat(sep="","<variables count=\"",p1,"\">\n",file=filename,append=T)
for (i in 1:p1) {
  if (is.factor(dat1[,i])) {
      l1<-length(levels(dat1[,i]))
      cat(sep="","  <categoricalvariable name=\"",var.name1[i],
        "\" >\n",file=filename,append=T)
      cat("    <levels count=\"",l1,"\" >\n",sep="",file=filename,append=T)
      for (j in 1:l1) 
        cat("    <level value=\"",j,"\" >",levels(dat1[,i])[j],"</level>\n",
          sep="",file=filename,append=T)
      cat("    </levels>\n",file=filename,append=T)
      cat("  </categoricalvariable>\n",file=filename,append=T)
  }
  else if (i%in%catvars1) {
    cat(sep="","  <categoricalvariable name=\"",var.name1[i],
        "\" levels=\"auto\"/>\n",file=filename,append=T)
  }
  else {
    cat(sep="","  <realvariable name=\"",var.name1[i],"\"/>\n",
      file=filename,append=T)
  }
}
cat(sep="","</variables>\n",file=filename,append=T)
cat(sep="","<records count=\"",n1,"\" glyph=\"",default.glyph,
"\" color=\"",default.color,"\" >\n",file=filename,append=T) 
row.name1<-rownames(dat1)
if (is.null(row.name1))
  row.name1<-c(1:n1)
if (is.null(dat1.id)) 
  dat1.id<-c(1:n1)
if (length(dat1.colors)!=n1) {
  if (!is.null(dat1.colors))
    cat("Length of data 1 colors vector is not the same as the number of rows.\n")
  dat1.colors<-rep(default.color,n1)
}
if (length(dat1.glyphs)!=n1) {
  if (!is.null(dat1.glyphs))
    cat("Length of data 1 glyphs vector is not the same as the number of rows.\n")
  dat1.glyphs<-rep(default.glyph,n1)
}
for(i in 1:n1)
{  
   cat(sep="","<record id=\"",dat1.id[i],"\" label=\"",row.name1[i],
     "\" ",file=filename,append=T)
   cat(sep="","color=\"",dat1.colors[i],"\" ",file=filename,append=T)
   cat(sep="","glyph=\"",dat1.glyphs[i],"\"",file=filename,append=T)
   cat(sep="",">\n",file=filename,append=T)
   for (j in 1:p1)
     cat(dat1[i,j]," ",file=filename,append=T)
   cat(sep="","\n</record>\n",file=filename,append=T)   
}
cat(sep="","</records>\n</data>\n",file=filename,append=T) 

if (data.num>1) {
# 2nd data
p2<-ncol(dat2)
n2<-nrow(dat2)
var.name2<-colnames(dat2)
if (is.null(var.name2))
  for (i in 1:p2)
    var.name2<-c(var.name2,paste("Var ",i))
cat(sep="","<data name=\"",dat2.name,"\">\n",file=filename,append=T)
cat(sep="","<description>\n",file=filename,append=T)
cat(sep="",dat2.description,"\n",file=filename,append=T)
cat(sep="","</description>\n",file=filename,append=T)
cat(sep="","<variables count=\"",p2,"\">\n",file=filename,append=T)
for (i in 1:p2) {
  if (i%in%catvars2) {
    cat(sep="","<categoricalvariable name=\"",var.name2[i],
      "\" levels=\"auto\"/>\n",file=filename,append=T)
  }
  else 
  cat(sep="","<realvariable name=\"",var.name2[i],"\"/>\n",
    file=filename,append=T)
}
cat(sep="","</variables>\n",file=filename,append=T)
cat(sep="","<records count=\"",n2,"\" glyph=\"",default.glyph,
  "\" color=\"",default.color,"\">\n",file=filename,append=T) 
row.name2<-rownames(dat2)
if (is.null(row.name2))
  row.name2<-c(1:n2)
#if (is.null(dat2.id))
#  dat2.id<-c(1:n2)
if (length(dat2.colors)<n2) {
  if (!is.null(dat2.colors))
    cat("Length of data 2 colors vector is not the same as the number of rows.\n")
  dat2.colors<-rep(default.color,n2)
}
if (length(dat2.glyphs)<n2) {
  if (!is.null(dat2.glyphs))
    cat("Length of data 2 glyphs vector is not the same as the number of rows.\n")
  dat2.glyphs<-rep(default.glyph,n2)
}
if (is.null(dat2.id)) {
  if (is.null(dat2.source)) {
  for(i in 1:n2)
  {  
    cat(sep="","<record  label=\"",
      row.name2[i],"\" ",file=filename,append=T)
    cat(sep="","color=\"",dat2.colors[i],"\" ",file=filename,append=T)
    cat(sep="","glyph=\"",dat2.glyphs[i],"\"",file=filename,append=T)
    cat(sep="",">\n",file=filename,append=T)
    cat(sep=" ",dat2[i,],"\n",file=filename,append=T)
    cat(sep="","</record>\n",file=filename,append=T)   
  }
  cat(sep="","</records>\n</data>\n",file=filename,append=T) 
}
else {  
  for(i in 1:n2)
  {  
    cat(sep="","<record  source=\"",dat2.source[i],
      "\" destination=\"",dat2.destination[i],
      "\"  label=\"",row.name2[i],"\" ",file=filename,append=T)
    cat(sep="","color=\"",dat2.colors[i],"\" ",file=filename,append=T)
    cat(sep="","glyph=\"",dat2.glyphs[i],"\"",file=filename,append=T)
    cat(sep="",">\n",file=filename,append=T)
    cat(sep=" ",dat2[i,],"\n",file=filename,append=T)
    cat(sep="","</record>\n",file=filename,append=T)   
  }
  cat(sep="","</records>\n</data>\n",file=filename,append=T) 
  }
}
else {
  if (is.null(dat2.source)) {
  for(i in 1:n2)
  {  
    cat(sep="","<record id=\"",dat2.id[i],"\" label=\"",
      row.name2[i],"\" ",file=filename,append=T)
    cat(sep="","color=\"",dat2.colors[i],"\" ",file=filename,append=T)
    cat(sep="","glyph=\"",dat2.glyphs[i],"\"",file=filename,append=T)
    cat(sep="",">\n",file=filename,append=T)
    cat(sep=" ",dat2[i,],"\n",file=filename,append=T)
    cat(sep="","</record>\n",file=filename,append=T)   
  }
  cat(sep="","</records>\n</data>\n",file=filename,append=T) 
}
else {  
  for(i in 1:n2)
  {  
    cat(sep="","<record id=\"",dat2.id[i],"\" source=\"",dat2.source[i],
      "\" destination=\"",dat2.destination[i],
      "\"  label=\"",row.name2[i],"\" ",file=filename,append=T)
    cat(sep="","color=\"",dat2.colors[i],"\" ",file=filename,append=T)
    cat(sep="","glyph=\"",dat2.glyphs[i],"\"",file=filename,append=T)
    cat(sep="",">\n",file=filename,append=T)
    cat(sep=" ",dat2[i,],"\n",file=filename,append=T)
    cat(sep="","</record>\n",file=filename,append=T)   
  }
  cat(sep="","</records>\n</data>\n",file=filename,append=T) 
  }
}
}

if (data.num>2) {
# 3rd data
p3<-ncol(dat3)
n3<-nrow(dat3)
var.name3<-colnames(dat3)
if (is.null(var.name3))
  for (i in 1:p1)
    var.name3<-c(var.name3,paste("Var ",i))
cat(sep="","<data name=\"",dat3.name,"\">\n",file=filename,append=T)
cat(sep="","<description>\n",file=filename,append=T)
cat(sep="",dat3.description,"\n",file=filename,append=T)
cat(sep="","</description>\n",file=filename,append=T)
cat(sep="","<variables count=\"",p3,"\">\n",file=filename,append=T)
for (i in 1:p3) {
  if (i%in%catvars3) {
    cat(sep="","<categoricalvariable name=\"",var.name3[i],
      "\" levels=\"auto\"/>\n",file=filename,append=T)
  }
  else 
  cat(sep="","<realvariable name=\"",var.name3[i],"\"/>\n",
    file=filename,append=T)
}
cat(sep="","</variables>\n",file=filename,append=T)
cat(sep="","<records count=\"",n3,"\" glyph=\"",default.glyph,
  "\" color=\"",default.color,"\">\n",file=filename,append=T) 
row.name3<-rownames(dat3)
if (is.null(row.name3))
  row.name3<-c(1:n3)
if (length(dat3.colors)!=n1) {
  if (!is.null(dat3.colors))
    cat("Length of data 3 colors vector is not the same as the number of rows.\n")
  dat3.colors<-rep(default.color,n3)
}
if (length(dat3.glyphs)!=n3) {
  if (!is.null(dat3.glyphs))
    cat("Length of data 3 glyphs vector is not the same as the number of rows.\n")
  dat3.glyphs<-rep(default.glyph,n3)
}
for(i in 1:n3)
{  
   cat(sep="","<record id=\"",dat3.id[i],"\" label=\"",
     row.name3[i],"\"> ",file=filename,append=T)
   cat(sep="","color=\"",dat3.colors[i],"\" ",file=filename,append=T)
   cat(sep="","glyph=\"",dat3.glyphs[i],"\"",file=filename,append=T)
   cat(sep="",">\n",file=filename,append=T)
   cat(sep=" ",dat3[i,],"\n",file=filename,append=T)
   cat(sep="","</record>\n",file=filename,append=T)  
}
cat(sep="","</records>\n</data>\n",file=filename,append=T) 
}

# wrap-up file
cat(sep="","</ggobidata>",file=filename,append=T) 
}
