#set working directory
setwd("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project")
getwd()

#read in data
kdata <- read.csv("ParedDown_K.csv")
vdata <- read.csv("ParedDown_v.csv")

#Convert Vdata column from cm/d to cm/s
vdata$v.cms <- vdata$v.cmd / (24 * 3600)

#load library
library(ggplot2)

#ln() of vdata
vdata$ln <- log(vdata$v.cms)

#plot vdata on ggplot
var(vdata)
ggplot(data = data.frame(x = vdata), aes(x = vdata$ln)) + geom_density()


#ln() of kdata
kdata$ln <- log(kdata$K.cms)

#plot kdata on ggplot
var(kdata)
ggplot(data = data.frame(x = kdata), aes(x = kdata$ln)) + geom_density()

#combined plot of vdata and kdata on ggplot
ggplot() + xlab("ln(cm/s)") + ylab("Density") +
  geom_density(aes(x = vdata$ln, fill="vdata"), alpha = 0.2)+
  geom_density(aes(x = kdata$ln, fill="kdata"), alpha = 0.2)+
  scale_fill_manual(name = "dataset", values = c(vdata="blue", kdata="red"))

