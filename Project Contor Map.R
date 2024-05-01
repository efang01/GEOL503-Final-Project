#MAC
#setwd("/Users/ellisfangmann/Library/CloudStorage/OneDrive-UniversityofKansas/GEOL 503/Project")
#getwd()
#library(ggplot2)

#Kdata <- read.csv("/Users/ellisfangmann/Library/CloudStorage/OneDrive-UniversityofKansas/GEOL 503/Project/ParedDown_K.csv")
#vdata <- read.csv("/Users/ellisfangmann/Library/CloudStorage/OneDrive-UniversityofKansas/GEOL 503/Project/ParedDown_v.csv")

#PC
setwd("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project")
getwd()

kdata <- read.csv("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project/ParedDown_K.csv")
vdata <- read.csv("C:/Users/efang/OneDrive - University of Kansas/GEOL 503/Project/ParedDown_v.csv")


#Convert Vdata column from cm/d to cm/s
vdata$v.cms <- vdata$v.cmd / (24 * 3600)

##MAIN IDEA

library(ggplot2)

library(akima)

gridv <- akima::interp(vdata$Xtrue.m, vdata$Z.m, vdata$v.cms)

griddfv <- data.frame(x = rep(gridv$x, ncol(gridv$z)),
                     y = rep(gridv$y, each = nrow(gridv$z)),
                     z = as.numeric(gridv$z))

ggplot(data = griddfv, aes(x = x, y = y, z  = z)) +
  geom_contour_filled(size = 0.3, bins = 20) +
  geom_contour(size = 0.1, bins = 20, color = "black") +
  geom_point(data = vdata, aes(x = Xtrue.m, y = Z.m, z = v.cms)) +
  labs(title = "Contour Plot of Velocity Data", x = "Distance Along Transect (m)", y = "Depth (m)")


gridk <- akima::interp(kdata$Xtrue.m, kdata$Z.masl, kdata$K.cms)

griddfk <- data.frame(x = rep(gridk$x, ncol(gridk$z)),
                      y = rep(gridk$y, each = nrow(gridk$z)),
                      z = as.numeric(gridk$z))

ggplot(data = griddfk, aes(x = x, y = y, z  = z)) +
  geom_contour_filled(size = 0.3, bins = 20) +
  geom_contour(size = 0.1, bins = 20, color = "black") +
  geom_point(data = kdata, aes(x = Xtrue.m, y = Z.masl, z = K.cms)) +
  labs(title = "Contour Plot of Hydraulic Conductivty Data", x = "Distance Along Transect (m)", y = "Depth (masl)")