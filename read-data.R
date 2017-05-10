#
#
#

rdafile="classdata.rda"

if (!file.exists(rdafile))
{
  files <- list.files(path="./",pattern="*.txt")
  datlst <- lapply(files,function(x){d <- read.delim(x); d$semester <- x;d})
  dat <- do.call(rbind,datlst)
  dat$Instructor=gsub(" $","",as.character(dat$Instructor))
  instructors <- read.csv("newteach-names.csv")
  instructors$Instructor=gsub(" $","",as.character(instructors$Instructor))
  
  dat$Rem <- as.numeric(as.character(dat$Rem))
  dat <- dat[!is.na(dat$Cap),]
  dat <- dat[!is.na(dat$Rem),]
  dat$Rem <- as.numeric(dat$Rem)
  dat$Cap <- as.numeric(dat$Cap)
  dat$enrolled <- dat$Cap - dat$Rem
  
  dat <- with(dat,aggregate(cbind(enrolled),
                          by=list(Subj=Subj,Crse=Crse,Days=Days,Time=Time,Instructor=Instructor,Cred=Cred,semester=semester),
                          sum))
  dat <- merge(dat,instructors,all.x=T)
  write.csv(file="newteach.csv",row.names=F,unique(dat[,c("Instructor","classification")]))

  classes.to.exclude = c("101L","102L","111L","112L","399","448","450","451","499","397",as.character(693:900))

  grad.classes <- as.character(500:699)
  grad.classes <- c(grad.classes,paste(grad.classes,"L",sep=""))

  dat <- dat[!(dat$Crse %in% classes.to.exclude),]

  dat$Cred <- as.numeric(as.character(dat$Cred))
  dat <- dat[!is.na(dat$Cred),]
  dat$scred <- dat$Cred*dat$enrolled
  dat$semester = gsub(".txt","",dat$semester)
  dat$season = gsub("[0-9].*","",dat$semester)
  dat$year =  as.numeric(gsub("[A-Za-z]","",dat$semester))
  dat$upper=ifelse(dat$Crse%in%as.character(300:399),T,F)

  secgraduate <- dat[dat$Crse %in% grad.classes,]
  secundergraduate <- dat[!(dat$Crse %in% grad.classes),]
  secdat <- dat

  scred <- with(dat[],aggregate(cbind(scred),by=list(Instructor=Instructor,classification=classification,semester=semester),sum))
  mnscred <- with(scred,aggregate(cbind(scred),by=list(Instructor=Instructor),mean))

  save(file=rdafile,secgraduate,secundergraduate,secdat,dat,scred,mnscred)
} else {
  load(rdafile)
}

#write.table(file="teaching.csv",row.names=F,sep=",",secdat)
