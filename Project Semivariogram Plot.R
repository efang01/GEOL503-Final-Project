setwd("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project")
getwd()

kdata <- read.csv("ParedDown_K.csv")
vdata <- read.csv("ParedDown_v.csv")


#Convert Vdata column from cm/d to cm/s
vdata$v.cms <- vdata$v.cmd / (24 * 3600)

library(gstat)
library(sp)

#vdata

coordinates(vdata) = ~Xtrue.m+Z.m
vvariogram = variogram(v.cms~1,data=vdata)
plot(vvariogram)

#kdata

coordinates(kdata) = ~Xtrue.m+Z.masl
kvariogram = variogram(K.cms~1,data=kdata)
plot(kvariogram)
