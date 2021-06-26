library(ggplot2)
rm(list=ls())

##data import
setwd("C:/R")
hd=read.csv("heart.csv",header=TRUE)
attach(hd)
head(hd)
str(hd)
hd.ex=hd[,1:13]
nrow(subset(hd,target=="0"))
nrow(subset(hd,target=="1"))
##variables factor
hd$sex=factor(hd$sex)
hd$cp=factor(hd$cp)
hd$fbs=factor(hd$fbs)
hd$restecg=factor(hd$restecg)
hd$exang=factor(hd$exang)
hd$slope=factor(hd$slope)
hd$ca=factor(hd$ca)
hd$thal=factor(hd$thal)
hd$target=factor(hd$target)
str(hd)

####################data summary############################
########################################################
summary(hd)

sd(hd$age)
sd(hd$trestbps)
sd(hd$chol)
sd(hd$thalach)
sd(hd$oldpeak)

##########################Graph########################
########################################################
#####1.contiuous variables
##histogram
par(mar=c(2,2,2,2))
par(mfrow=c(2,3))
hist(hd$age,main="age")
hist(hd$trestbps,main="trestbps")
hist(hd$chol,main="chol")
hist(hd$thalach,main="thalach")
hist(hd$oldpeak,main="oldpeak")

##boxplot-continuous variables
boxplot(hd$age,main="age")
boxplot(hd$trestbps,main="trestbps")
boxplot(hd$chol,main="chol")
boxplot(hd$thalach,main="thalach")
boxplot(hd$oldpeak,main="oldpeak")

#####2.factor variables
plot(hd[,14])
library(gridExtra)

sex=ggplot(hd, aes(x=sex, fill=target))+
  geom_histogram(binwidth = 0.2, stat="count")

cp=ggplot(hd, aes(x=cp, fill=target))+
  geom_histogram(binwidth = 0.2, stat="count")

fbs=ggplot(hd, aes(x=fbs, fill=target))+
  geom_histogram(binwidth = 0.2,stat="count")

restecg=ggplot(hd, aes(x=restecg, fill=target))+
  geom_histogram(binwidth = 0.2,stat="count")

exang=ggplot(hd, aes(x=exang, fill=target))+
  geom_histogram(binwidth = 0.2,stat="count")

slope=ggplot(hd, aes(x=slope, fill=target))+
  geom_histogram(binwidth = 0.2,stat="count")

ca=ggplot(hd, aes(x=ca, fill=target))+
  geom_histogram(binwidth = 0.2,stat="count")

thal=ggplot(hd, aes(x=thal, fill=target))+
  geom_histogram(binwidth = 0.2,stat="count")

target=ggplot(hd, aes(x=target, fill=target))+
  geom_histogram(binwidth = 0.2,stat="count")

grid.arrange(sex,cp,fbs,restecg,exang,slope,ca,thal,nrow=2,ncol=4)
grid.arrange(ca,thal,target,nrow=2,ncol=2)

#ggplot(data=hd, aes(y=age))+
#  geom_boxplot(outlier.color = 'red', outlier.shape = 2)

###########correlation/상관행렬###########
cor_hd=cor(hd); cor_hd
par(mar=c(0,0,0,0)); par(mfrow=c(1,1))
library(corrplot)
hd.cor=round(cor(hd),2)
corrplot(hd.cor,method="color",tl.col="black",tl.srt=30,addCoef.col = "black",
         diag=FALSE,number.cex = 0.7)



################Data mining##################
########################################################
#######1.PCA(주성분분석)
###target=0(심장병 no)
t0=subset(hd, target=="0")
t0=t0[,-14]
p_cor_0=princomp(t0, cor=TRUE)
summary(p_cor_0)
p_cor_0$loadings

screeplot(p_cor_0,npcs=10,type="lines",main="scree plot-correlation") #스크리 그래프
biplot(p_cor_0,main="target=0(심장병 없다)")

###target=1(심장병 yes)
t1=subset(hd, target=="1")
t1=t1[,-14]
p_cor_1=princomp(t1, cor=TRUE)
summary(p_cor_1)
p_cor_1$loadings

screeplot(p_cor_1,npcs=10,type="lines",main="scree plot-correlation") #스크리 그래프
biplot(p_cor_1,main="target=1(심장병 있다)")

###all(target=0,1)
p_cor=princomp(hd.ex,cor=TRUE)
summary(p_cor) ##num=5
p_cor$loadings

p_cor$sdev
p_cor$loadings

#scree plot
par(mar=c(3,3,3,3))
library(graphics)
screeplot(p_cor,npcs=10,type="lines",main="scree plot-correlation") #스크리 그래프

biplot(p_cor,pointSize=20,main="전체자료")

par(mfrow=c(1,1))


#산점도
library(MASS)
library(scatterplot3d)
scatterplot3d(p_cor$scores[,1],p_cor$scores[,2],p_cor$scores[,3],
              xlab="comp.1",ylab="comp.2",zlab="comp.3")
########################################################
###########2.factor(인자분석)

fact1=factanal(hd.ex,factors=5,rotation = "none") #최대우도법
fact2=factanal(hd.ex,factors=5,scores = "regression") #주성분 분석 varimax
fact3=factanal(hd,factors=5,rotation="promax") #promax
fact1
fact2
fact3

#screeplot
prin=princomp(hd)
screeplot(prin, npcs=14, type="lines", main="scree plot") 

#plot of factor pattern
par(mar=c(5,5,5,5))
par(mfrow=c(1,1))
namevar=names(fact2$loadings)=c("age","sex","cp","tresbps","chol","fbs","restecg",
                                "thalach","exang","oldpeak","slope","ca","thal","target")
plot(fact2$loadings[,1],fact2$loadings[,2],pch=16,xlab="factor1",ylab="factor2",
     main="factor pattern by varimax")
text(x=fact2$loadings[,1],y=fact2$loadings[,2],labels = namevar, adj=0)
abline(v=0,h=0)

plot(fact1$loadings[,1],fact1$loadings[,2],pch=16,xlab="factor1",ylab="factor2",
     main="factor pattern by MLE")
text(x=fact1$loadings[,1],y=fact1$loadings[,2],labels = namevar, adj=0)
abline(v=0,h=0)

plot(fact3$loadings[,1],fact3$loadings[,2],pch=16,xlab="factor1",ylab="factor2",
     main="factor pattern by promax")
text(x=fact3$loadings[,1],y=fact3$loadings[,2],labels = namevar, adj=0)
abline(v=0,h=0)

#plot of factor scores
plot(fact2$scores[,1], fact2$scores[,2],pch="*",xlab="factor1",ylab="factor2",
     main="factor scores")
text(x=fact2$scores[,1],y=fact2$scores[,2],labels = namevar, adj=0)

############################################################
######군집분석######
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms & for agnes function
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(dplyr)
hd.1=hd[,-14]
hd.1=scale(hd.1)
str(hd.1)

####k-means clustering(비계층적)
clust2 <- kmeans(hd.1, centers = 2, nstart = 25)
clust3 <- kmeans(hd.1, centers = 3, nstart = 25)
clust4 <- kmeans(hd.1, centers = 4, nstart = 25)
clust5 <- kmeans(hd.1, centers = 5, nstart = 25) 

# plots to compare
p1 <- fviz_cluster(clust2, geom = "point", data = hd.1) + ggtitle("k = 2")
p2 <- fviz_cluster(clust3, geom = "point", data = hd.1) + ggtitle("k = 3")
p3 <- fviz_cluster(clust4, geom = "point", data = hd.1) + ggtitle("k = 4")
p4 <- fviz_cluster(clust5, geom = "point", data = hd.1) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)

##the number of clusters           
# Elbow methods
set.seed(12345)
fviz_nbclust(hd.1, FUN = kmeans, method = "wss")

# Average Silhouette methods
fviz_nbclust(hd.1, FUN = kmeans, method = "silhouette")

# Gap Static methods
gap_stat <- clusGap(hd.1, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)


#final
final=kmeans(hd.1, centers = 2, nstart = 25)
print(final)

fviz_cluster(final, data = hd.1)

hd %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all(list(mean=mean, sd=sd))

table(hd$target,final$cluster,dnn=list("observed","predicted"))
mean(hd$target!=final$cluster)


#####계층적 군집분석
# Dissimilarity matrix
dist <- dist(hd.1, method = "euclidean") #유클리드 거리
library(cluster)
dist2=daisy(hd.1,metric="euclidean")

distance <- get_dist(hd.1) #거리
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

hc1 <- agnes(dist2, method = "single" ) #single Linkage(최단)
hc2 <- agnes(dist2, method = "complete" ) #Complete Linkage(최장)
hc3 <- agnes(dist2, method = "average" ) #average Linkage(평균)
hc4 <- agnes(dist2, method = "ward" ) #Ward's minimum variance method

### TO CONTROL THE NUMBER OF CLUSTERS
num.cl = 2
# Cut tree into 2 groups
sub_grp1 <- cutree(hc1, k = num.cl) 
sub_grp2 <- cutree(hc2, k = num.cl) 
sub_grp3 <- cutree(hc3, k = num.cl) 
sub_grp4 <- cutree(hc4, k = num.cl) 

# PLOT clusters in PCA field
p11=fviz_cluster(list(data = hd.1, cluster = sub_grp1))
p22=fviz_cluster(list(data = hd.1, cluster = sub_grp2))
p33=fviz_cluster(list(data = hd.1, cluster = sub_grp3))
p44=fviz_cluster(list(data = hd.1, cluster = sub_grp4))

library(gridExtra)
grid.arrange(p11, p22, p33, p44, nrow = 2)

#### For the number of clusters(클러스터 갯수)           
# Elbow methods
fviz_nbclust(hd.1, FUN = hcut, method = "wss") 
# Average Silhouette methods
fviz_nbclust(hd.1, FUN = hcut, method = "silhouette") #k=2
# Gap Static methods
gap_stat <- clusGap(hd.1, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat) 

#prediction
table(observed=hd[,14],predicted=sub_grp1) #single
table(observed=hd[,14],predicted=sub_grp2) #complete
table(observed=hd[,14],predicted=sub_grp3) #mean
table(observed=hd[,14],predicted=sub_grp4) #ward
#error
err1=mean(hd[,14]!=sub_grp1);err1
err2=mean(hd[,14]!=sub_grp2);err2
err3=mean(hd[,14]!=sub_grp3);err3
err4=mean(hd[,14]!=sub_grp4);err4

##k-medoids
fit.pam=pam(hd.1,k=2,metric="euclidean", stand=FALSE)
fit.pam$medoids
dev.off()
clusplot(fit.pam, main="Bivariate Cluster Plot")
str(fit.pam)
table(hd.1[,14],fit.pam$clustering)

###########################################
######다변량 정규분포
library(mvoutlier)
chisq.plot(hd)
str(hd)

###############################################33
########glm 로지스틱 회귀
#standalizaion
max1 = apply(hd, 2, max) ##변수별 최대값
min1 = apply(hd, 2, min) ##변수별 최소값
Sdat = scale(hd, center = min1, scale = max1 - min1) #Standaization(표준화)
Sdat1 = data.frame(Sdat[,-14], target=as.factor(hd$target)) 

#data partition
tr.ind=sample(1:2,nrow(hd),prob=c(0.5,0.5),replace=T)
ii=which(tr.ind==1)
tr.h=Sdat1[ii,]
ts.h=Sdat1[-ii,]
nrow(tr.h)
nrow(ts.h)

glm.const=glm(target~1,data=tr.h,family=binomial)
fmla=as.formula(paste("target","~",paste(colnames(tr.h)[-ncol(tr.h)],collapse = "+")))
glm.aic=step(glm.const,fmla,direction="forward")
summary(glm.aic)

#prediction
pred.glm=predict(glm.aic,ts.h,type="response")
pred=rep(0,length(pred.glm))
pred[pred.glm>=0.5]=1
table(ts.h$target,pred,dnn=list("observed","predict"))
mean(tr.h$target!=pred)
nrow(tr.h)
nrow(pred)

#################################
###신경망
library(neuralnet)
library(nnet)
gn = names(tr.h)
f = as.formula(paste("target ~", paste(gn[!gn %in% "target"], collapse = " + "))) 
# class변수만 제외한 변수들의 합

test.err = function(h.size){
     neu = nnet(target~.,tr.h,size = h.size, decay = 5e-4, trace=F)
     y = ts.h$target
     p = predict(neu, ts.h, type = "raw")
     #p.n = as.numeric(p[,2]>0.5)+1
     err = mean(y != p)
     c(h.size, err)
   }

# compare test error rates for neural networks with 2-10 hidden units
out = t(sapply(2:5, FUN = test.err))
out #error 최소인 은닉노드 수 선택 k=4
par(mfrow=c(1,1))
pdf("ch5-5.pdf")
plot(out, type="b", xlab="The number of Hidden units", ylab="Test Error")
dev.off()

#은닉노드 3개
fit.nn = neuralnet(f, data = tr.h, hidden=3, linear.output=F) 
fit.nn2=nnet(target~., data=tr.h, size=3, decay=5e-4)
summary(fit.nn)
summary(fit.nn2)
plot(fit.nn)

#predict
p.test.nn = neuralnet::compute(fit.nn, ts.h[,-14])$net.result 
yhat.test.nn = ifelse(p.test.nn > 0.5, 1, 0)
table(ts.h$target, yhat.test.nn[,2], dnn=c("Actual","Predicted"))
mean(ts.h$target!=yhat.test.nn[,2])

p.test.nn2=predict(fit.nn2, ts.h, type="class")
table(ts.h$target, p.test.nn2)
mean(ts.h$target!=p.test.nn2)

pdf("ch5-4.pdf")
plot(out, type="b", xlab="The number of Hidden units", ylab="Test Error")


##ROCR & AUC
library(ROCR)
perf.c = performance(prediction(p.test.nn[,2], ts.h$target), "tpr","fpr") ##신경망
ROC.glm = performance(prediction(pred.glm, ts.h$target), "tpr","fpr")  ##glm

par(mfrow=c(1,1))
plot(perf.c, col = 2, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve")
par(new = TRUE)
plot(ROC.glm, col = 3, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity")
lines(x = c(0,1), y = c(0,1), col = 4, lty = 2, lwd = 2)
legend("bottomright", 
                 legend = c("Neural Network(class):3","Logit", "Random"), 
                 col = c(2:4,1), lty = c(1,1,2), lwd = 2, cex=0.6)

############################################
#######앙상블
library(rpart)
library(adabag)
library(randomForest)
############ TREE(의사결정나무 모형)
### Grow a tree
fit = rpart(target ~., data=tr.h, method="class", control = rpart.control(xval=10, cp=0))
#full decision tree with 10 fold cross validation
par(mfrow=c(1,2),mar=c(2,2,2,2),oma=c(0,0,0,0))
plot(fit,main="Full Decision Tree")
text(fit, use.n=TRUE, all = T, cex=0.6)

### Prune the tree
tmp = printcp(fit)
cp.tmp = tmp[which.min(tmp[,"xerror"]),"CP"] #cross validatio error 최소가 되게하는 cp값
fit.pruned = prune(fit, cp=cp.tmp)

plot(fit.pruned,main="Pruned Decision Tree")
text(fit.pruned, use.n=TRUE, all = T, cex=0.6)

### Prediction
cutoff = 0.5
pred = predict(fit.pruned, newdata=ts.h, type="prob") #prediction
yhat = ifelse(pred[,2] > cutoff, 1, 0)  ##y=1일 때 사후확률 > 0.5이면 1로 분류, 아니면 0
ctable = table(ts.h$target, yhat, dnn=c("Actual", "Predicted"));ctable

### Errors
mean(ts.h$target!=yhat) ##오분류율
miss.err = 1-sum(diag(ctable))/sum(ctable);miss.err#오분류율
pred.acc = 1 - miss.err;pred.acc  ##정분류율
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity(민감도)
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity(특이도)

########### BAGGING(배깅)
### Grow trees
set.seed(1234)
my.control = rpart.control(xval=0, cp=0, minsplit=5, maxdepth=10)
#각 노드안에 최소 5개의 데이터 존재하게, 10개의 노드를 가진 의사결정나무 모형
fit.bag = bagging(target~., data=tr.h, mfinal=50, control=my.control)
#50개의 의사결정나무를 합쳐서 모형생성

print(fit.bag$importance) #주요변수
importanceplot(fit.bag) #plot

### Prediction
pred = predict.bagging(fit.bag, newdata=ts.h) #predict with test set ##직접 prob지정
cutoff = 0.5
yhat = ifelse(pred$prob[,2] > cutoff, 1, 0)
ctable = table(ts.h$target, yhat, dnn=c("Actual", "Predicted")); ctable #classification table

### Errors
miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate(오분류율)
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy(정분류율)
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


########### RANDOM FORESTS
### Grow trees
set.seed(126)
fit.RF = randomForest(target~., data=tr.h, ntree=100, mtry=5, importance=T)
#ntree=100개의 트리를 합쳐서 모형생성, mtry=5개 변수 선택.

importance(fit.RF)

plot(fit.RF, type="l")
legend(50, 0.45, legend=c("OOB: overall", "OOB:0's", "OOB:1's"), lty=c(1:3), col=c(1:3),cex=0.6)

### Prediction
pred = predict(fit.RF, newdata=ts.h, type="prob") #test set으로 예측
cutoff = 0.5
yhat = ifelse(pred[,2] > cutoff, 1, 0)
ctable = table(ts.h$target, yhat, dnn=c("Actual", "Predicted")); ctable #classification table
mean(ts.h$target!=yhat)

### Errors
miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity
########## BOOSTING(부스팅)
### Grow trees
set.seed(14785)
my.control = rpart.control(xval=0, cp=0, maxdepth=5)
fit.boo = boosting(target~., data=tr.h, boos=T, mfinal=50, control=my.control)
#mfinal=50번 반복하면서 가중치 배분

fit.boo$trees
print(fit.boo$importance)
importanceplot(fit.boo)

### Prediction
pred = predict.boosting(fit.boo, newdata=ts.h) #prediction with test set
cutoff = 0.5
yhat = ifelse(pred$prob[,2] > cutoff, 1, 0)
ctable = table(ts.h$target, yhat, dnn=c("Actual", "Predicted")); ctable #classification table
#pred$confusion -> 오분류율

### Errors
miss.err = 1-sum(diag(ctable))/sum(ctable); miss.err # Misclassification Rate
pred.acc = 1 - miss.err; pred.acc #Prediction Accuracy
diag(ctable)[2]/apply(ctable, 1, sum)[2] # Sensitivity
diag(ctable)[1]/apply(ctable, 1, sum)[1] # Specificity


######## ROC CURVE and AUC
library(ROCR)
par(mfrow=c(1,1))

pred2 = predict(fit.pruned, newdata=ts.h, type="prob") #pruned trees
pred.tree = prediction(pred2[,2], ts.h$target)
perf.tree = performance(pred.tree, "tpr","fpr")

pred3 = predict.bagging(fit.bag, newdata=ts.h)$prob  # bagging 
pred.bag = prediction(pred3[,2], ts.h$target)
perf.bag = performance(pred.bag, "tpr","fpr")

pred4 = predict(fit.RF, newdata=ts.h, type="prob") # random forests
pred.RF = prediction(pred4[,2], ts.h$target)
perf.RF = performance(pred.RF, "tpr","fpr")

pred5 = predict.boosting(fit.boo, newdata=ts.h)$prob # boosting
pred.boo = prediction(pred5[,2], ts.h$target)
perf.boo = performance(pred.boo, "tpr","fpr")

pred6=predict(glm.aic,ts.h,type="response") #glm
perf.glm= performance(prediction(pred.glm, ts.h$target), "tpr","fpr")  ##glm

#pred7=neuralnet::compute(fit.nn, ts.h[,-14])$net.result 
perf.neu=performance(prediction(p.test.nn[,2], ts.h$target), "tpr","fpr") ##신경망

plot(perf.tree, col = 7, lwd = 2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
par(new = TRUE)
plot(perf.bag, col = 6, lwd = 2, lty=2, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
par(new = TRUE)
plot(perf.RF, col = 5, lwd = 2, lty=3, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
par(new = TRUE)
plot(perf.boo, col = 4, lwd = 2, lty=4, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
par(new = TRUE)
plot(perf.neu, col = 3, lwd = 2, lty=5, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC
par(new = TRUE)
plot(perf.glm, col = 2, lwd = 2, lty=6, xlab = "1-Specificity", ylab = "Sensitivity", main = "ROC Curve") #ROC


lines(x = c(0,1), y = c(0,1), lty = 7, lwd = 2)
legend(0.6, 0.6, legend = c("Tree(Pruned)","Bagging", "Random Forests", "Boosting", "Neural","Logistic","Random"),
       col = c(7:1), lty = c(1:7), lwd = 2,cex=0.6)




performance(pred.tree, "auc")@y.values #AUC
performance(pred.bag, "auc")@y.values #AUC
performance(pred.RF, "auc")@y.values #AUC
performance(pred.boo, "auc")@y.values #AUC
performance(prediction(p.test.nn[,2], ts.h$target), "auc")@y.values #AUC
performance(prediction(pred.glm, ts.h$target), "auc")@y.values #AUC


