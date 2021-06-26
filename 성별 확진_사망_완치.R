setwd("C:/R/corona")
d=read.csv("PatientInfo.csv",header=TRUE)
str(d)
attach(d)

#sex & age 데이터추출 
female=subset(d,sex=="female")
male=subset(d,sex=="male")

#여성 사망/완치비율
female.state=female[,18]
str(female.state)
table(female.state)

lb=c("사망","격리","완치")
pct=round(table(female.state)/sum(table(female.state))*100)
lb=paste(lb,pct,"%")
pie(table(female.state),labels=lb,main="여성 사망/완치 비율")

#남성 사망/완치비율
male.state=male[,18]
str(male.state)
table(male.state)

lb=c("사망","격리","완치")
pct=round(table(male.state)/sum(table(male.state))*100)
lb=paste(lb,pct,"%")
pie(table(female.state),labels=lb,main="남성 사망/완치 비율")

par(mfrow=c(1,2))

#연령별 확진자수

