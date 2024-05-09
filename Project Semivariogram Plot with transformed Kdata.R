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

#plot transformed kdata on semivariogram
coordinates(kdata) = ~Xtrue.m+Z.masl
kvariogram = variogram(v~1,data=kdata)
plot(kvariogram)

