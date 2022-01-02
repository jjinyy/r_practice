
setwd("c:\\a_temp")
 library(devtools)

devtools::install_github("hadley/ggplot2")
library(ggplot2)
devtools::install_github("dkahle/ggmap")
 library(ggmap)


 pop <- read.csv("�������α���Ȳ.csv",header=T)
 pop
 lon <- pop$LON
 lat <- pop$LAT
 data <- pop$���α���
 df <- data.frame(lon,lat,data)
 df
 map1 <- get_map("Jeonju",zoom=7 , maptype='roadmap')
 map1 <- ggmap(map1)
 map1 + geom_point(aes(x=lon,y=lat,colour=data,size=data),data=df)
 ggsave("pop.png",scale=1,width=7,height=4,dpi=1000)
