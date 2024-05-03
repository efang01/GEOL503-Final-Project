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

#transformed kdata xy plot with line

par(mfrow=c(1,1)) 
kdataplot <- data.frame(kdata$v,kdata$Z.masl)
kdataplot <- na.omit(kdataplot)
knalin <- lowess(kdataplot)
plot(kdataplot)
lines(knalin,  col=grey(level=0.3,alpha=0.2),lwd=50)

#overlay new kdata on vdata
x1 <- kdata$Z.masl
y1 <- kdata$v
x2 <- vdata$Z.m
y2 <- vdata$v.cms
plot(x1,y1,col="blue")
abline(lm(y1~x1),col=grey(level=0.3,alpha=0.2),lwd=50)
par(new=T)
plot(x2,y2,col="red",xact="n",yaxt="n",ylab="",xlab="")
abline(lm(y2~x2),col=grey(level=0.3,alpha=0.2),lwd=50)
axis(side=4)
mtext("v",side=4,line=3)

