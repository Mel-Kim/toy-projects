rm(list=ls())

d=read.csv("PatientInfo.csv",header=TRUE)
str(d)
nrow(d)

#data frame by 연령별 상태
age0_19=subset(d,d$birth_year>'2000-01-01' & d$birth_year<='2020-01-01') #0-19세
age20_39=subset(d,d$birth_year>'1980-01-01' & d$birth_year<='2000-01-01') #20-39세
age40_59=subset(d,d$birth_year>'1960-01-01' & d$birth_year<='1980-01-01') #40-59세
age60_79=subset(d,d$birth_year>'1940-01-01' & d$birth_year<='1960-01-01') #60-79세
age80_99=subset(d,d$birth_year>'1920-01-01' & d$birth_year<='1940-01-01') #80-99세

age0_19.state=age0_19[,18]
age20_39.state=age20_39[,18]
age40_59.state=age40_59[,18]
age60_79.state=age60_79[,18]
age80_99.state=age80_99[,18]

#pie chart
lb=c("사망","격리","완치")
pct1=round(table(age0_19.state)/sum(table(age0_19.state))*100)
lb1=paste(lb,pct1,"%")
pie(table(age0_19.state),labels=lb1,main="0-19세 확진자")

pct2=round(table(age20_39.state)/sum(table(age20_39.state))*100)
lb2=paste(lb,pct2,"%")
pie(table(age20_39.state),labels=lb2,main="20-39세 확진자")

pct3=round(table(age40_59.state)/sum(table(age40_59.state))*100)
lb3=paste(lb,pct3,"%")
pie(table(age40_59.state),labels=lb3,main="40-59세 확진자")

pct4=round(table(age60_79.state)/sum(table(age60_79.state))*100)
lb4=paste(lb,pct4,"%")
pie(table(age60_79.state),labels=lb4,main="60-79세 확진자")

pct5=round(table(age80_99.state)/sum(table(age80_99.state))*100)
lb5=paste(lb,pct5,"%")
pie(table(age80_99.state),labels=lb5,main="80-99세 확진자")
par(mfrow=c(2,3))


#data frame by 상태별
re1=nrow(subset(age0_19,age0_19$state=="released")) #0-19세중 완치
re2=nrow(subset(age20_39,age20_39$state=="released"))
re3=nrow(subset(age40_59,age40_59$state=="released"))
re4=nrow(subset(age60_79,age60_79$state=="released"))
re5=nrow(subset(age80_99,age80_99$state=="released"))


slices1=c(re1,re2,re3,re4,re5)
lb=c("0-19세","20-39세","40-59세","60-79세","80-99세")
pct1=round(slices1/sum(slices1)*100)
lb1=paste(lb,pct1,"%")
pie(slices1,labels=lb1,main="확진자 연령대")

par(mfrow=c(1,1))

#사망자 연령대 
de1=nrow(subset(age0_19,age0_19$state=="deceased")) #0-19세중 사망
de2=nrow(subset(age20_39,age20_39$state=="deceased"))
de3=nrow(subset(age40_59,age40_59$state=="deceased"))
de4=nrow(subset(age60_79,age60_79$state=="deceased"))
de5=nrow(subset(age80_99,age80_99$state=="deceased"))


slices2=c(de1,de2,de3,de4,de5)
lb=c("0-19세","20-39세","40-59세","60-79세","80-99세")
pct2=round(slices2/sum(slices2)*100)
lb2=paste(lb,pct2,"%")
pie(slices2,labels=lb2,main="사망자 연령대")

#완치자 연령대 
is1=nrow(subset(age0_19,age0_19$state=="isolated")) #0-19세중 사망
is2=nrow(subset(age20_39,age20_39$state=="isolated"))
is3=nrow(subset(age40_59,age40_59$state=="isolated"))
is4=nrow(subset(age60_79,age60_79$state=="isolated"))
is5=nrow(subset(age80_99,age80_99$state=="isolated"))


slices3=c(is1,is2,is3,is4,is5)
lb=c("0-19세","20-39세","40-59세","60-79세","80-99세")
pct3=round(slices3/sum(slice3s)*100)
lb3=paste(lb,pct3,"%")
pie(slices3,labels=lb3,main="완치자 연령대")

par(mfrow=c(2,2))
