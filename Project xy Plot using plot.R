setwd("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project")
getwd()

kdata <- read.csv("ParedDown_K.csv")
vdata <- read.csv("ParedDown_v.csv")


#Convert Vdata column from cm/d to cm/s
vdata$v.cms <- vdata$v.cmd / (24 * 3600)

#vdata xy plot with line

par(mfrow=c(1,1)) 
vdataplot <- data.frame(vdata$v.cms,vdata$Z.m)
vdataplot <- na.omit(vdataplot)
vnalin <- lowess(vdataplot)
plot(vdataplot)
lines(vnalin, col=grey(level=0.3,alpha=0.2),lwd=50)

#kdata xy plot with line

par(mfrow=c(1,1)) 
kdataplot <- data.frame(kdata$K.cms,kdata$Z.masl)
kdataplot <- na.omit(kdataplot)
knalin <- lowess(kdataplot)
plot(kdataplot)
lines(knalin,  col=grey(level=0.3,alpha=0.2),lwd=50)
