setwd("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project")
getwd()

kdata <- read.csv("ParedDown_K.csv")
vdata <- read.csv("ParedDown_v.csv")


#Convert Vdata column from cm/d to cm/s
vdata$v.cms <- vdata$v.cmd / (24 * 3600)

#vdata h ploting

plot(vdata$v.cms,vdata$Z.m,
     ylim=rev(range(vdata$Z.m)),
     type="h",pch=NA)

mvvelocity <- mean(vdata$v.cms)
mvvelocity

#vdata hist plotting

hist(vdata$v.cms)
abline(v = mean(vdata$v.cms),col="blue",lwd=3)
lines(density(vdata$v.cms),col="red",lwd=3)


#kdata h ploting

plot(kdata$K.cms,kdata$Z.masl,
     ylim=rev(range(kdata$Z.masl)),
     type="h",pch=NA)

mkconduc <- mean(kdata$K.cms)
mkconduc

#kdata hist plotting

hist(kdata$K.cms)
abline(v = mean(kdata$K.cms),col="blue",lwd=3)
lines(density(kdata$K.cms),col="red",lwd=3)


