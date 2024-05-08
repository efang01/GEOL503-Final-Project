setwd("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project")
getwd()

kdata <- read.csv("ParedDown_K.csv")

library(reshape2)
library(sp) 
library(gstat) 
library(lattice)
library(latticeExtra)
library(plot3D)
library(gridExtra)

#Kdata transformed 

#convert cm/s to m/min
kdata$K.cms <- (kdata$K.cms/100)*60

#Q(flow rate)
A <- 1
i <- 4.3*10^-3
kdata$Q <- -1*(kdata$K.cms * A * i)

#q(specific discharge)
kdata$q <- (kdata$Q / A)

#v(seepage velocity)
n = 0.34
kdata$v <- (kdata$q / n)

#read data
kdata <- kdata[c(1,2,3)]
colnames(kdata) <- c("x","y","k")
okdata <- kdata[order(kdata$x,kdata$y),]
okdata <- apply(okdata,c(1,2),as.numeric)
okdata <- as.data.frame(okdata)
lg <- function(x){
  if(!is.na(x)){x <- log(x)}
  else{x <- NA}
}

okdata$logk <- sapply(okdata$k,lg)
okdata$logk <- -1 * okdata$logk

#extend grid
x.range <- as.integer(c(0,13)) 
y.range <- as.integer(c(215,220))
z.range <- as.integer(c(1.0,1.0))

grd <- expand.grid(x=seq(x.range[1],x.range[2],0.1),
                   y=seq(y.range[1],y.range[2],0.1))
kgrd <- grd
coordinates(grd) <- ~x+y
gridded(grd) <- TRUE
intgrd <- (grd)

plot(grd,cex=0.5,col="lightgray")
points(okdata$x,okdata$y,pch=1,col="red")

okdata$x <- okdata$x/10
grd <- as.data.frame(grd)
grd$x <- grd$x/10

#inverse distance weighting
okdata <- na.omit(okdata)
idwlogkdata <- idw(logk~1, ~x + y, okdata, newdata=grd, idp=idp)
idwkoutput = as.data.frame(idwlogkdata)
names(idwkoutput)[1:3] <- c("x","y","logk")

idwlogkdata$x <- idwlogkdata$x*10
okdata$x <- okdata$x*10
grd$x <- grd$x*10

plot(grd,cex=0.5,col="lightgray")
points(okdata$x,okdata$y,pch=1,col="red")

xidwk <- okdata$x
yidwk <- okdata$y
xcontour <- seq(from=0,to=12,by=1)
ycontour <- seq(from=0.1,to=3.6,by=0.1)

#lattice plot
contourplot(logk~x+y,data = idwkoutput,
            main=paste(""), 
            cuts=40,
            region=T,
            ylim=range(yidwk),
            col.regions=heat.colors(20,alpha=0.85,rev=T))
