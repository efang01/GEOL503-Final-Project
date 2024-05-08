setwd("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project")
getwd()

vdata <- read.csv("ParedDown_v.csv")

library(reshape2)
library(sp) 
library(gstat) 
library(lattice)
library(latticeExtra)
library(plot3D)
library(gridExtra)

idp=0.5

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

ovdata$logv <- sapply(ovdata$v,lg)
ovdata$logv <- -1*ovdata$logv

#extend grid
x.range <- as.integer(c(1,8)) 
y.range <- as.integer(c(1,6))
z.range <- as.integer(c(1.0,1.0))

grd <- expand.grid(x=seq(x.range[1],x.range[2],0.1),
                   y=seq(y.range[1],y.range[2],0.1))
vgrd <- grd
coordinates(grd) <- ~x+y
gridded(grd) <- TRUE
intgrd <- (grd)

plot(grd,cex=0.5,col="lightgray")
points(ovdata$x,ovdata$y,pch=1,col="red")

ovdata$x <- ovdata$x/5
grd <- as.data.frame(grd)
grd$x <- grd$x/5

#inverse distance weighting
ovdata <- na.omit(ovdata)
idwlogvdata <- idw(logv~1, ~x + y, ovdata, newdata=grd, idp=idp)
idwVoutput = as.data.frame(idwlogvdata)
names(idwVoutput)[1:3] <- c("x","y","logv")

idwlogvdata$x <- idwlogvdata$x*5
ovdata$x <- ovdata$x*5
grd$x <- grd$x*5

plot(grd,cex=0.5,col="lightgray")
points(ovdata$x,ovdata$y,pch=1,col="red")

xidwV <- ovdata$x
yidwV <- ovdata$y
xcontour <- seq(from=1,to=8,by=0.1)
ycontour <- seq(from=1,to=6,by=0.1)

#lattice plot
contourplot(logv~x+y, data = idwVoutput,
            main=paste(""), 
            cuts=40,
            region=T, 
            ylim=range(yidwV),
            col.regions=heat.colors(20,alpha=0.85,rev=T))
