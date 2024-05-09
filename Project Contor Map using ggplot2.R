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
library(akima)

#Vdata

#use akima on Vdata to implement bivariate interpolation onto a grid
gridv <- akima::interp(vdata$Xtrue.m, vdata$Z.m, vdata$v.cms)
griddfv <- data.frame(x = rep(gridv$x, ncol(gridv$z)),
                     y = rep(gridv$y, each = nrow(gridv$z)),
                     z = as.numeric(gridv$z))

#plot vdata using ggplot
ggplot(data = griddfv, aes(x = x, y = y, z  = z)) +
  geom_contour_filled(size = 0.3, bins = 20) +
  geom_contour(size = 0.1, bins = 20, color = "black") +
  geom_point(data = vdata, aes(x = Xtrue.m, y = Z.m, z = v.cms)) +
  labs(title = "Contour Plot of Velocity Data (cm/s)", x = "Distance Along Transect (m)", y = "Depth (m)")

#Kdata

#use akima on kdata to implement bivariate interpolation onto a grid
gridk <- akima::interp(kdata$Xtrue.m, kdata$Z.masl, kdata$K.cms)
griddfk <- data.frame(x = rep(gridk$x, ncol(gridk$z)),
                      y = rep(gridk$y, each = nrow(gridk$z)),
                      z = as.numeric(gridk$z))

#plot kdata using ggplot
ggplot(data = griddfk, aes(x = x, y = y, z  = z)) +
  geom_contour_filled(size = 0.3, bins = 20) +
  geom_contour(size = 0.1, bins = 20, color = "black") +
  geom_point(data = kdata, aes(x = Xtrue.m, y = Z.masl, z = K.cms)) +
  labs(title = "Contour Plot of Hydraulic Conductivty Data (cm/s)", x = "Distance Along Transect (m)", y = "Depth (masl)")