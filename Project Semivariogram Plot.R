#set working directory
setwd("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project")
getwd()

#read in data
kdata <- read.csv("ParedDown_K.csv")
vdata <- read.csv("ParedDown_v.csv")

#Convert Vdata column from cm/d to cm/s
vdata$v.cms <- vdata$v.cmd / (24 * 3600)

#load library
library(gstat)
library(sp)

#plot vdata on semivariogram
coordinates(vdata) = ~Xtrue.m+Z.m
vvariogram = variogram(v.cms~1,data=vdata)
plot(vvariogram)

#plot kdata on semivariogram
coordinates(kdata) = ~Xtrue.m+Z.masl
kvariogram = variogram(K.cms~1,data=kdata)
plot(kvariogram)
