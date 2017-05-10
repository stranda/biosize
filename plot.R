library(lattice)
source("read-data.R")


pdf("teaching-load.pdf")

with(secundergraduate[secundergraduate$classification=="R",],hist(enrolled,main="Distribution of section sizes in UG Biology for Roster Faculty"))
with(secundergraduate,hist(enrolled,main="Distribution of section sizes in Biology for all instructors"))
with(dat,hist(enrolled,main="Distribution of section sizes in Biology for all instructors, all levels (G and UG)"))



tmp <- with(secundergraduate[secundergraduate$classification=="R",],aggregate(cbind(enrolled),by=list(Instructor=Instructor),mean))
print(dotplot(Instructor~enrolled,data=tmp,cex.lab=0.5,main="Mean section size UG only"))

tmp <- with(secgraduate[secgraduate$classification=="R",],aggregate(cbind(enrolled),by=list(Instructor=Instructor),mean))
print(dotplot(Instructor~enrolled,data=tmp,cex.lab=0.5,main="Mean section size Grad only"))

tmp <- with(dat[dat$classification=="R",],aggregate(cbind(enrolled),by=list(Instructor=Instructor),mean))
print(dotplot(Instructor~enrolled,data=tmp,cex.lab=0.7,main="Mean section size UG+Grad"))

tmp <- with(dat[dat$classification=="R",],aggregate(cbind(scred),by=list(Instructor=Instructor),mean))
print(dotplot(Instructor~scred,data=tmp,cex.lab=0.7,main="Mean student credit hrs UG+Grad"))


dev.off()


histogram(~enrolled|semester,data=secundergraduate,main="Undergraduate section sizes")
