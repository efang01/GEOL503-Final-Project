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
rownames(mvdata) <- x

#simple contour
contour(x,y,mvdata,nlevels=7)
par(new=T)
plot(ovdata$x,ovdata$y,pch=16, yaxt="n")