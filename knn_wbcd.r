# KNN using wbcd.csv

getwd()
wbcd_1<-read.csv("C:\\Users\\user\\Downloads\\wbcd.csv")
wbcd<-wbcd_1[,-33]
View(wbcd)
str(wbcd)
attach(wbcd)
summary(wbcd)

dim(wbcd)
table(wbcd$diagnosis)
wbcd<-wbcd[-1]
dim(wbcd)
str(wbcd$diagnosis)
wbcd$diagnosis <- factor(wbcd$diagnosis,levels=c("B","M"),labels=c("Benign","Malignant"))
View(wbcd)

round(prop.table(table(wbcd$diagnosis))*100,digits=3)

#to check if there is scaling issue

summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])


wbcd_n <- scale(wbcd[-1])
class(wbcd_n)
wbcd_n <- as.data.frame(wbcd_n)
summary(wbcd_n[c("radius_mean","area_mean","smoothness_mean")])
class(wbcd_n)
#splitting as train and test data

input_train<-wbcd_n[1:469,]
dim(input_train)

input_test<-wbcd_n[470:569,]
dim(input_test)

#taking output train from original data
output_train <- wbcd[1:469,1]
str(output_train)

#summary(diagnosis)
output_train <- as.data.frame(output_train)
output_train <- as.factor(output_train[[1]])
str(output_train)


output_test <- wbcd[470:569,1]
str(output_test)


output_test <- as.data.frame(output_test)
output_test <- as.factor(output_test[[1]])
str(output_test)

#install class library for knn algorithm
#knn() - 4 parameters - train,test,class,k

library(class)
m1 <- knn(input_train,input_test,cl=output_train,k=3)
class(m1)
m1

output_test
pv<- m1
pv
pv<-as.data.frame(pv)
View(pv)
#output test
av<- as.data.frame(wbcd[470:569,1])
final<-cbind(pv,av)

output_test==m1 # as we cant manually calculate go for below
mean(output_test==m1)# accuracy is 92

#crosstable matrix for verifiction
library(gmodels)
CrossTable(output_test,m1)
