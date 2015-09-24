require(data.table)
setwd("R:/Project/wilderness")
tabfile<-read.table("yfcc100m_dataset-0", header=FALSE, sep="\t",col.names=c("Photo/video identifier","User NSID","User nickname","Date taken","Date uploaded", "Capture device","Title",   "Description",  "User tags","Machine tags",  "Longitude", "Latitude", "Accuracy", "Photo/video page URL", "Photo/video download URL",  "License name",   "License URL","Photo/video server identifier", "Photo/video farm identifier",   "Photo/video secret", "Photo/video secret original",  "Extension of the original photo",  "Photos/video marker"),stringsAsFactors=FALSE)

tabsmall<-tabfile[which(is.na(tabfile$Longitude)==FALSE),]
#tabsmall is all files in first file with location information.
coordinates(tabsmall)<-c("Longitude","Latitude")

#longlist.temp=subset(tf2,grepl("nature",tf2$User.tags,fixed=TRUE)==TRUE)
longlist<-rbind(longlist,tf2)
require(rgdal)
nrow(tabwild)
wild<-readOGR("FLICKR/S_USA.Wilderness","S_USA.Wilderness")
wild<-spTransform(wild, CRS("+proj=longlat +ellps=WGS84"))
proj4string(tabsmall)<-proj4string(wild)

notagoodidea<-over(tabsmall, wild)
head(notagoodidea)
tabsmall$wilderness<-notagoodidea$WILDERNESS
tabsmall$wilderness_2<-notagoodidea$WILDERNESS_2

tabwild<-subset(tabsmall, is.na(tabsmall$wilderness)==FALSE)
plot(tabwild)


#Where do people go to be in "nature" Using twitter to model 

#Sports fields or "pitches" because OSM is european. This just uses pitches in the state of California.
points<-readOGR("california-latest.shp","landuse")
points<-subset(points, points$type=="pitch")
points<-spTransform(points, proj4string(tabsmall))
notagoodidea<-over(tabsmall, points)
head(notagoodidea)
rm(points)
tabsmall$pitchname<-notagoodidea$name
tabsmall$pitchid<-notagoodidea$osm_id

#this loads the machine tags and matches them up. takes awhile.
machinetags<-fread("yfcc100m_autotags-v1", sep="\t", header=FALSE, colClasses=c("numeric","character"))
colnames(machinetags)<-c("Photo.video.identifier","autotags")
class(machinetags$Photo.video.identifier)
class(as.data.table(tabsmall@data)$Photo.video.identifier)
tabsmall.dt<-merge(as.data.table(tabsmall@data), machinetags, by="Photo.video.identifier", all.x=TRUE)
tabsmall@data<-as.data.frame(tabsmall.dt)
rm(tabsmall.dt)

tabpitch<-subset(tabsmall, is.na(tabsmall$pitchid)==FALSE)
tabwild<-subset(tabsmall, is.na(tabsmall$wilderness)==FALSE)


#this function extracts the machine tags, the threshold is the confidence for the machine tag you want to limit by.
tagsplit<-function(tagcolumn,threshold){
lapply(tagcolumn,function(X){
if(nchar(X)<=2){temp<-data.frame("tags"=NA,"probs"=NA)} else {
tags_raw<-unlist(str_split(unlist(str_split(X,",")),":"))
tags<-tags_raw[seq(1,length(tags_raw),2)]
probs<-tags_raw[seq(2,length(tags_raw),2)]
temp<-data.frame(tags, probs)
temp<-subset(temp, as.numeric(as.character(temp$probs))>=threshold)}
paste(temp$tags, collapse=",")})
}


#making a dataset for machine learning
tabwild.split<-tagsplit(tabwild@data$autotags.y,.9)
require(reshape2)
tabwild.split<-melt(tabwild.split)
tabpitch.split<-tagsplit(tabpitch@data$autotags.y,.9)
tabpitch.split<-melt(tabpitch.split)
tabpitch.split$wild<-0
tabwild.split$wild<-1
modelset<-rbind(tabwild.split,tabpitch.split)
set.seed(2)
samp<-sample(seq(1:nrow(modelset)),floor(.75*nrow(modelset)), replace=F)
trainset<-modelset[samp,]
testset<-modelset[-samp,]
trainset$train<-1
testset$train<-0
modelset<-rbind(trainset,testset)
library(RTextTools)
rm(machinetags)
tmat<-create_matrix.rs(modelset$value, language="english",removeNumbers=FALSE, stemWords=TRUE, removePunctuation=TRUE,removeSparseTerms=.998, weighting=weightTf)
container<-create_container(tmat,modelset$wild,trainSize=1:floor(.75*nrow(modelset)), testSize=c(floor(.75*nrow(modelset))+1):nrow(modelset),virgin=FALSE)
SVM<-train_model(container,"SVM")
SVM_CLASSIFy<-classify_model(container, SVM)
analytics<-create_analytics(container,SVM_CLASSIFy)
summary(analytics)


#making a set for seattle,tacoma
cities<-readOGR("tl_2015_us_csa","tl_2015_us_csa")
seattleCSA<-subset(cities, cities$NAME=="Seattle-Tacoma, WA")
seattleCSA<-spTransform(seattleCSA,proj4string(tabsmall))
notagoodidea<-over(tabsmall,seattleCSA)
head(notagoodidea)
tabsmall$SeattleCSA<-notagoodidea$NAME
tabseattle<-subset(tabsmall, is.na(tabsmall$SeattleCSA)==FALSE)
plot(tabseattle)
tabseattle.split<-tagsplit(tabseattle@data$autotags.y,.9)
tabseattle.split<-melt(tabseattle.split)

tmatsea<-create_matrix.rs(tabseattle.split$value, language="english",removeNumbers=FALSE, stemWords=TRUE, removePunctuation=TRUE,removeSparseTerms=.998,originalMatrix=tmat, weighting=weightTf)
containersea<-create_container(tmatsea,labels=rep(NA,nrow(tabseattle.split)),testSize=1:nrow(tabseattle.split),virgin=TRUE)
create_container

class_SEA<-classify_model(containersea, SVM)    

tabseattle$Wild<-class_SEA$SVM_LABEL

table(tabseattle$Wild)
require(ggplot2)
head(tabseattle@data)



###future stuff


head(temp)
head(tabwild@data)

userid<-tabwild$User.NSID[1]
userinfo<-paste("https://api.flickr.com/services/rest/?method=flickr.people.getInfo&api_key=a09bfd56f0e65b3f333ce4199260bca3&user_id=",userid,"&format=rest&auth_token=72157658374799710-c78552fc2b2a1e5b&api_sig=705e315969519a1171fc78da3bd880c2",sep="")
userinfo



#https://api.flickr.com/services/rest/?method=flickr.test.echo&name=value
#flickrkey<-"726836146a74a77e28b142d3e5075aee"
#flirckrsecret<-"05a9abfa35e2c66f"

#https://api.flickr.com/services/rest/?method=flickr.photos.comments.getList&api_key=a09bfd56f0e65b3f333ce4199260bca3&photo_id=405682599%EF%BF%BD&format=rest&api_sig=ab49fd0496e4614637dc462dfd8c2773

