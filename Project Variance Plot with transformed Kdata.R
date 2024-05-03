setwd("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project")
getwd()

kdata <- read.csv("ParedDown_K.csv")
vdata <- read.csv("ParedDown_v.csv")


#Convert Vdata column from cm/d to cm/s
vdata$v.cms <- vdata$v.cmd / (24 * 3600)

library(ggplot2)

#ln() of vdata

vdata$ln <- log(vdata$v.cms)

#plot vdata 

var(vdata)
ggplot(data = data.frame(x = vdata), aes(x = vdata$ln)) + geom_density()


#Kdata transformed 

#convert cm/s to m/min
kdata$K.cms <- (kdata$K.cms/100)*60

#Q(flow rate)
A <- 1
i <- 4.3*10^-3
kdata$Q <- (kdata$K.cms * A * i)

#q(specific discharge)
kdata$q <- (kdata$Q / A)

#v(seepage velocity)
n = 0.34
kdata$v <- (kdata$q / n)

#ln() of kdata

kdata$ln <- log(kdata$v)

#plot kdata

var(kdata)
ggplot(data = data.frame(x = kdata), aes(x = kdata$ln)) + geom_density()

#combined plot

ggplot() + xlab("ln(cm/s)") + ylab("Density") +
  geom_density(aes(x = vdata$ln, fill="vdata"), alpha = 0.2)+
  geom_density(aes(x = kdata$ln, fill="kdata"), alpha = 0.2)+
  scale_fill_manual(name = "dataset", values = c(vdata="blue", kdata="red"))

