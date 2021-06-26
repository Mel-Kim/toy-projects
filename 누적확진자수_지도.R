setwd("C:/R/corona")
d=read.csv("PatientInfo.csv",header=TRUE)
#r=read.csv("map.csv",header=TRUE)
province=tapply(d$province,d$province,length)
data.frame(province)
str(province)
length(province)
province$id=0:16
colnames(province)=c("´©Àû¼ö","id")

install.packages("ggmap")
install.packages("raster")
install.packages("rgeos")
install.packages("maptool")
install.packages("rgdal")

library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)

korea=shapefile("C:/R/corona/CTPRVN_201905/TL_SCCO_CTPRVN.shp")
korea=spTransform(korea,CRS("+proj=longlat"))
korea_map=fortify(korea,region='CTPRVN_CD')
korea=merge(korea_map,r,by="id")

head(korea)

ggplot()+geom_polygon(data=korea,aes(x=long,y=lat,group=group))

       