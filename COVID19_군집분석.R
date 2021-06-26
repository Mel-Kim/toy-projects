setwd("C:/R/corona")
patinfo=read.csv("PatientInfo.csv",header=TRUE)
head(patinfo,n=10)
str(patinfo)
attach(patinfo)

#데이터 전처리
patinfo=patinfo[,-6]
str(patinfo)

patinfo=na.omit(patinfo)
table(is.na(patinfo))

complete.cases(patinfo)


#군집분석
dist_data=dist(patinfo)
hc_c=hclust(dist_data,method="ward.D2")
plot(hc_c,hang=-1,cex=0.7)

install.packages("factoextra")
library(factoextra)

fviz_nbclust(patinfo,kmeans,method="wss")
