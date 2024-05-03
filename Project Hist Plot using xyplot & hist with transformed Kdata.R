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

#kdata h ploting

plot(kdata$v,kdata$Z.masl,
     ylim=rev(range(kdata$Z.masl)),
     type="h",pch=NA)

mkconduc <- mean(kdata$v)
mkconduc

#kdata hist plotting

hist(kdata$v)
abline(v = mean(kdata$v),col="blue",lwd=3)
lines(density(kdata$v),col="red",lwd=3)


