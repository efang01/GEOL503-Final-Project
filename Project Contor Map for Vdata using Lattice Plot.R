#set working directory
setwd("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project")
getwd()

#read in data
vdata <- read.csv("ParedDown_v.csv")

#load library
library(reshape2)
library(sp) 
library(gstat) 
library(lattice)
library(latticeExtra)
library(plot3D)
library(gridExtra)

#set idp
idp=0.5

#read data
vdata <- vdata[c(1,2,3)]
#order cols
colnames(vdata) <- c("x","y","vel")
ovdata <- vdata[order(vdata$x,vdata$y),]
ovdata <- apply(ovdata,c(1,2),as.numeric)
ovdata <- as.data.frame(ovdata)
#create log fn
lg <- function(x){
  if(!is.na(x)){x <- log(x)}
  else{x <- NA}
}

#apply lg fn to normalize data
ovdata$logv <- sapply(ovdata$v,lg)
ovdata$logv <- -1*ovdata$logv

#create interpolation grid
x.range <- as.integer(c(1,8)) 
y.range <- as.integer(c(1,6))
z.range <- as.integer(c(1.0,1.0))
grd <- expand.grid(x=seq(x.range[1],x.range[2],0.1),
                   y=seq(y.range[1],y.range[2],0.1))
#convert grid to spatialpixel class
vgrd <- grd
coordinates(grd) <- ~x+y
gridded(grd) <- TRUE
intgrd <- (grd)

#plot expanded grid and sampling points
plot(grd,cex=0.5,col="lightgray")
points(ovdata$x,ovdata$y,pch=1,col="red")

#start anisotropy interpolation of data
ovdata$x <- ovdata$x/5
grd <- as.data.frame(grd)
grd$x <- grd$x/5

#inverse distance weighting
ovdata <- na.omit(ovdata)
idwlogvdata <- idw(logv~1, ~x + y, ovdata, newdata=grd, idp=idp)
idwVoutput = as.data.frame(idwlogvdata)
names(idwVoutput)[1:3] <- c("x","y","logv")

#finish anisotropy interpolation of data
idwlogvdata$x <- idwlogvdata$x*5
ovdata$x <- ovdata$x*5
grd$x <- grd$x*5

#plot expanded grid and sampling points after anisotropy interpolation
plot(grd,cex=0.5,col="lightgray")
points(ovdata$x,ovdata$y,pch=1,col="red")

#put x and y vector on grid
xidwV <- ovdata$x
yidwV <- ovdata$y
#vector of x and y values for contour plot
xcontour <- seq(from=1,to=8,by=0.1)
ycontour <- seq(from=1,to=6,by=0.1)

#create lattice plot of vdata
contourplot(logv~x+y, data = idwVoutput,
            main=paste(""), 
            cuts=40,
            region=T, 
            ylim=range(yidwV),
            col.regions=heat.colors(20,alpha=0.85,rev=T))