#set working directory
setwd("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project")
getwd()

#read in data
kdata <- read.csv("ParedDown_K.csv")

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
#order cols
colnames(kdata) <- c("x","y","k")
okdata <- kdata[order(kdata$x,kdata$y),]
okdata <- apply(okdata,c(1,2),as.numeric)
okdata <- as.data.frame(okdata)
#create log fn
lg <- function(x){
  if(!is.na(x)){x <- log(x)}
  else{x <- NA}
}

#set up wide view
x <- rep(NA,13)
y <- rep(NA,36)

#reshape wide view
mkdata <- matrix(rep(rep(NA,13),36),ncol=13,nrow=36)
jj = 0 
for(i in 1:13){
  for(j in 1:36){
    jj <- jj+1
    mkdata[j,i] <- okdata[jj,3]
  }
}
x <- seq(from=1, to=13, by=1)
y <- seq(from=0.1, to=3.6, by=0.1)
mkdata <- t(mkdata)
colnames(mkdata) <- y
rownames(mkdata) <- x

#simple contour of transformed kdata
contour(x,y,mkdata,nlevels=7)
par(new=T)
plot(okdata$x,okdata$y,pch=16, yaxt="n")