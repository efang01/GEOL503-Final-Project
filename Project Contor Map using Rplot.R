setwd("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project")
getwd()

kdata <- read.csv("ParedDown_K.csv")
vdata <- read.csv("ParedDown_v.csv")

library(reshape2)
library(sp) 
library(gstat) 
library(lattice)
library(latticeExtra)
library(plot3D)
library(gridExtra)

#Convert Vdata column from cm/d to cm/s
#vdata$v.cms <- vdata$v.cmd / (24 * 3600)

#Vdata

#read data
vdata <- vdata[c(1,2,3)]
colnames(vdata) <- c("x","y","vel")
ovdata <- vdata[order(vdata$x,vdata$y),]
ovdata <- apply(ovdata,c(1,2),as.numeric)
ovdata <- as.data.frame(ovdata)
lg <- function(x){
  if(!is.na(x)){x <- log(x)}
  else{x <- NA}
}
#lgvel <- ovdata$vel
#lgvel <- sapply(lgvel,lg)
#ovdata$vel <- lgvel

#wide view
x <- rep(NA,15)
y <- rep(NA,26)
mvdata <- matrix(rep(rep(NA,15),26),ncol=15,nrow=26)
jj = 0 
for(i in 1:15){
  for(j in 1:26){
    jj <- jj+1
    mvdata[j,i] <- ovdata[jj,3]
  }
}

x <- seq(from=1, to=15, by=1)
y <- seq(from=0.1, to=2.6, by=0.1)
mvdata <- t(mvdata)
colnames(mvdata) <- y
colnames(mvdata) <- x

#simple contour
contour(x,y,mvdata,nlevels=7)
par(new=T)
plot(ovdata$x,ovdata$y,pch=16, yaxt="n")


#extend grid
x.range <- as.integer(c(1,8)) 
y.range <- as.integer(c(1,6))
z.range <- as.integer(c(1.0,1.0))

grd <- expand.grid(x=seq(x.range[1],x.range[2],0.1),
                   y=seq(y.range[1],y.range[2],0.1))
vgrd <- grd
coordinates(grd) <- ~x+y
gridded(grid) <- TRUE
intgrd <- (grd)
plot(grd,cex=0.5,col="lightgray")
points(ovdata$x,ovdata$y,pch=1,col="red")

#inverse distance weighting
ovdata <- na.omit(ovdata)
idwlogvdata <- idw(vel~1, ~x+y, newdata=grd, idp=idp)
idwoutput = as.data.frame(idwlogvdata)
names(idwoutput)[1:3] <- c("x","y","logvel")

xidw <- idwoutput$x
yidw <- idwoutput$y
xcontour <- seq(from=1,to=8,by=0.1)
ycontout <- seq(from=1,to=6,by=0.1)

plot(xidw,yidw,pch=1,col="transparent", #emply,scaled plot space
     ylim=rev(range(yidw)),xlim=range(xidw))
points(ovdata$x,ovdata$yx,pch=1) 
par(new=TRUE)
mvdata2 <- matrix(rep(rep(NA,51),71),ncol=71)
jj = 0
for(i in 1:51){
  for(j in 1:71){ 
    jj<-jj+1
    mbtx2[i,j]<- idwloutput[jj,3]
  }
}

mvdata2 <- t(mvdata2)
contour(xcontour,ycontour,mvdata2,levels=seq(2,10, by=1), #contour overlay
        ylim=rev(range(yidw)),xlim=range(xidw))

#lattice plot
mvdata3 <- mvdata2
colnames(mvdata3) <- ycontour
rownames(mvdata3) <- xcontour
mvdata3 <- melt(mvdata3, ne.rm=FALSE,
                value.name="lgvel", id=c("x","y","lgvel"))
colnames(mvdata3) <- c("x","y","lgvel")
mvdata3$y <- 7-mvdata$y

contourplot(mvdata3$lgvel~mvdata$x+mvdata$y,
            main=paste(""), cuts=14,
            region=T, aspect="iso",ylim=range(yidw),
            col.regions=heat.colors(20,alpha=0.5,rev=T))

#Kdata


