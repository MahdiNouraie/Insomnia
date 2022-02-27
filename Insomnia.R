install.packages("haven")
library(haven)
data=read_sas("F:\\Insomnia\\insomnia.sas7bdat")
attach(data)
datap=data[,-1]
summary(datap)
table(datap)
install.packages("vcd")
library(vcd)
mosaic(table(datap),shade=TRUE)
#TREATMENT=0#
x=matrix(table(datap)[1:4],2,2)
chisq.test(x)
#independence rejected#
install.packages("lsr")
library(lsr)
cramersV(x)
#TREATMENT=1#
y=matrix(table(datap)[5:8],2,2)
chisq.test(y)
#independence accepted#
library(lsr)
cramersV(y)
a11=cramersV(table(datap$Y1,datap$Y1))
a12=cramersV(table(datap$Y1,datap$Y2))
a13=cramersV(table(datap$Y1,datap$TREATMENT))
a21=cramersV(table(datap$Y2,datap$Y1))
a22=cramersV(table(datap$Y2,datap$Y2))
a23=cramersV(table(datap$Y2,datap$TREATMENT))
a31=cramersV(table(datap$TREATMENT,datap$Y1))
a32=cramersV(table(datap$TREATMENT,datap$Y2))
a33=cramersV(table(datap$TREATMENT,datap$TREATMENT))
C=matrix(c(a11,a12,a13,a21,a22,a23,a31,a32,a33),3,3,byrow=TRUE)
row.names(C)=c("Y1","Y2","TREATMENT")
colnames(C)=c("Y1","Y2","TREATMENT")
install.packages("corrplot")
library(corrplot)
corrplot(C, method="pie")
model1=glm(Y1~TREATMENT,family = binomial(logit),data=data)
summary(model1)
model2=glm(Y1~TREATMENT,family = binomial(probit),data=data) 
summary(model2)
model3=glm(Y1~TREATMENT,family = binomial(cloglog),data=data) 
summary(model3)
gd1=1-pchisq(281.61,237)
#goodness of fit rejected#
model4=glm(Y2~TREATMENT,family = binomial(logit),data=data)
summary(model4)
model5=glm(Y2~TREATMENT,family = binomial(probit),data=data)
summary(model5)
model6=glm(Y2~TREATMENT,family = binomial(cloglog),data=data)
summary(model6)
gd2=1-pchisq(300.74,237)
#goodness of fit rejected#
data0=data[which(data$Y1==0),]
data1=data[which(data$Y1==1),]
#newdata=data[order(ID,Y1),]#
#print(newdata,n=1e3)#
#data0=newdata[1:66,]#
#data1=newdata[67:239,]#
model7=glm(Y2~TREATMENT,family = binomial(logit),data=data0)
summary(model7)
model8=glm(Y2~TREATMENT,family = binomial(logit),data=data1)
summary(model8)
model9=glm(Y2~Y1+TREATMENT,family =  binomial(logit),data=data)
summary(model9)
gd3=1-pchisq(273.4,236)
detach(data)
