Heartdata<-read.csv(file="heart.csv", header = TRUE) #you will have to download the UC irving heart data set and save to your working directory as heart.csv
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(lubridate)
library(tidyr)
library(scales)
library(tidytext)
library(rvest)
library(XML)
library(caret)
library(data.table)
#load all these just for good measure... ... ... * ) 


par(mfrow=c(3,3))
table1<-table(Heartdata$target)
barplot(table1,col=rgb(0.2,0.4,0.6,0.6), names.arg=c("No Disease","Disease"), ylim=range(0,225), xlab="Category", ylab="Frequency", main="Frequency of Target Variable",las=2)
text(.7, 165, "138" )
text(1.9,195, "165")

table2<-table(Heartdata$sex)
barplot(table2,col=rgb(0.2,0.4,0.6,0.6), names.arg=c("female ","male"), ylim=range(0,275), xlab="Category", ylab="Frequency", main="Frequency of Sex(Gender)",las=2)
text(.7, 135, "96" )
text(1.9,250, "207")

table3<-table(Heartdata$fbs)
barplot(table3,col=rgb(0.2,0.4,0.6,0.6), names.arg=c("no FBS ","FBS"), ylim=range(0,325), xlab="Category", ylab="Frequency", main="Frequency of FBS",las=2)
text(.7, 300, "258" )
text(1.9,80, "45")

table4<-table(Heartdata$exang)
barplot(table4,col=rgb(0.2,0.4,0.6,0.6), names.arg=c("no exang ","exang"), ylim=range(0,325), xlab="Category", ylab="Frequency", main="Frequency of exang",las=2)
text(.7, 250, "204" )
text(1.9,125, "99")

table5<-table(Heartdata$cp)
barplot(table5,col=rgb(0.2,0.4,0.6,0.6), ylim=range(0,150), xlab="Category", ylab="Frequency", main="Frequency of exang",las=2)
text(.7, 250, "204" )
text(1.9,125, "99")

table6<-table(Heartdata$restecg)
barplot(table6,col=rgb(0.2,0.4,0.6,0.6), ylim=range(0,150), xlab="Category", ylab="Frequency", main="Frequency of restecg",las=2)

table7<-table(Heartdata$slope)
barplot(table7,col=rgb(0.2,0.4,0.6,0.6), ylim=range(0,150), xlab="Category", ylab="Frequency", main="Frequency of Slope value",las=2)

table8<-table(Heartdata$thal)
barplot(table7,col=rgb(0.2,0.4,0.6,0.6), ylim=range(0,150), xlab="Category", ylab="Frequency", main="Frequency Thal",las=2)


par(mfrow=c(2,2))
Heartdata<-read.csv(file="heart.csv", header = TRUE)
Heartdata$age<-Heartdata$ï..age
Heartdata<-Heartdata[,-1]

#namevector<-names(Heartdata[,c(3,4,7,9,14)])

#map(namevector, function(x){
# histo<-Heartdata%>%ggplot()+geom_histogram(aes(x=as.numeric(x), y=..density..), fill="dark green", color="black")
#  histo
#})

par(mfrow=c(2,2))

histo9<-Heartdata%>%ggplot()+geom_histogram(aes(x=Heartdata$age),fill="dark green", color="black", bins=10)
histo9

histo10<-Heartdata%>%ggplot()+geom_histogram(aes(x=Heartdata$trestbps),fill="dark green", color="black", bins=10)
histo10

histo11<-Heartdata%>%ggplot()+geom_histogram(aes(x=Heartdata$chol),fill="dark green", color="black", bins=10)
histo11

histo12<-Heartdata%>%ggplot()+geom_histogram(aes(x=Heartdata$thalach),fill="dark green", color="black", bins=10)
histo12


histo13<-Heartdata%>%ggplot()+geom_histogram(aes(x=Heartdata$oldpeak),  binwidth= max(Heartdata$oldpeak)/10.0,fill="dark green", color="black")
histo13


library(caret)
library(e1071)
library(purrr)#support vector machine package
Heartdata<-read.csv(file="heart.csv", header = TRUE)
Heartdata$target<-as.factor(Heartdata$target) #ensures dependant variable is a factor with 2 levels, and not an integer

set.seed(1)
Heartdata_sampling_vector <- createDataPartition(Heartdata$target, p=0.8, list=FALSE)
Heartdata_train<- Heartdata[Heartdata_sampling_vector,]
Heartdata_test<- Heartdata[-Heartdata_sampling_vector,]

set.seed(1)
Linearfirst<-svm(target ~., data=Heartdata_train, kernel = "linear", cost=10)
Linear_Kernal_acc<-confusionMatrix(Linearfirst$fitted, Heartdata_train[,"target"])$overall[1]
test_predictions<-predict(Linearfirst, Heartdata_test[,c(1:13)])
First_pass_linear<-mean(Heartdata_test[,14] == test_predictions)


set.seed(1)
Radialfirst<-svm(target ~., data=Heartdata_train, kernel = "radial", cost=10, gamma=0.5)
Radial_Kernal_acc<-confusionMatrix(Radialfirst$fitted, Heartdata_train[,"target"])$overall[1]
test_predictions<-predict(Radialfirst, Heartdata_test[,c(1:13)])
First_pass_Radial<-mean(Heartdata_test[,14] == test_predictions)

Firstpassresults<-data.table(model_name=c("Linear Kernal", "Radial Kernal"), trainset_accuracy=c(Linear_Kernal_acc,Radial_Kernal_acc),Test_set_accuarcy=c(First_pass_linear,First_pass_Radial), Cost=c(Linearfirst$cost,Radialfirst$cost), Gamma=c("na", Radialfirst$gamma))

Firstpassresults

Heartdata<-read.csv(file="heart.csv", header = TRUE)
set.seed(1)
Heartdata$age<-Heartdata$ï..age
Heartdata<-Heartdata[,-1]
Heartdata<-Heartdata[,-1]
Heartdata$target<-as.factor(Heartdata$target)
Heartdata_sampling_vector <-createDataPartition(Heartdata$target, p=0.8, list=FALSE)
Heartdata_train<- Heartdata[Heartdata_sampling_vector,]
Heartdata_test<- Heartdata[-Heartdata_sampling_vector,]



set.seed(1)
yes<-seq(0,1,.1)
tuneradial<-tune(svm, target ~., data=Heartdata_train, kernal="radial", ranges=list(cost=seq(1:10), gamma= yes))

set.seed(1)
tunelinear<-tune(svm, target ~.,data=Heartdata_train, kernal="linear", ranges=list(cost=seq(1:10)))

set.seed(1)
model_SVM<-svm(target ~., data=Heartdata_train, kernel = "linear", cost=tunelinear$best.parameters$cost)

test_predictions<-predict(model_SVM, Heartdata_test[,c(1:11,13)])
TEST_SET_result_linear<-mean(Heartdata_test[,12] == test_predictions)

set.seed(1)
model_SVM<-svm(target ~., data=Heartdata_train, kernel = "radial", cost=tuneradial$best.parameters$cost, gamma=tuneradial$best.parameters$gamma)

test_predictions<-predict(model_SVM, Heartdata_test[,c(1:11,13)])
TEST_SET_result_radial<-mean(Heartdata_test[,12] == test_predictions)


Bestparameters<-data.table(model_name=c("Linear Kernal SVM", "Radial Kernal SVM"),Performance_train_set=c(1-tunelinear$best.performance, 1-tuneradial$best.performance), Cost=c(tunelinear$best.parameters$cost, tuneradial$best.parameters$cost),Gamma=c( "NA", tuneradial$best.parameters$gamma),Performance_test_set=c(TEST_SET_result_linear,TEST_SET_result_radial))

Bestparameters

library(e1071)
Heartdata<-read.csv(file="heart.csv", header = TRUE)
set.seed(1)
Heartdata<-Heartdata%>%filter(Heartdata$sex==1)
Heartdata$age<-Heartdata$ï..age
Heartdata<-Heartdata[,-1]
Heartdata<-Heartdata[,-1]
Heartdata$target<-as.factor(Heartdata$target)
Heartdata_sampling_vector <-createDataPartition(Heartdata$target, p=0.8, list=FALSE)
Heartdata_train<- Heartdata[Heartdata_sampling_vector,]
Heartdata_test<- Heartdata[-Heartdata_sampling_vector,]


set.seed(1)
yes<-seq(0,1,.1)
tuneradialFemale<-tune(svm, target ~., data=Heartdata_train, kernal="radial", ranges=list(cost=seq(1:10), gamma= yes))
tuneradialFemale

set.seed(1)
yes<-seq(0,1,.1)
tunelinearFemale<-tune(svm, target ~.,data=Heartdata_train, kernal="linear", ranges=list(cost=seq(1:10)))
tunelinearFemale

model_SVM<-svm(target ~., data=Heartdata_train, kernel = "linear", cost=tunelinearFemale$best.parameters$cost)

test_predictions<-predict(model_SVM, Heartdata_test[,c(1:11,13)])
TEST_SET_result_linear_female<-mean(Heartdata_test[,12] == test_predictions)


model_SVM<-svm(target ~., data=Heartdata_train, kernel = "radial", cost=tuneradialFemale$best.parameters$cost, gamma=tuneradialFemale$best.parameters$gamma)

test_predictions<-predict(model_SVM, Heartdata_test[,c(1:11,13)])
TEST_SET_result_radial_female<-mean(Heartdata_test[,12] == test_predictions)



Heartdata<-read.csv(file="heart.csv", header = TRUE)
set.seed(1)
Heartdata<-Heartdata%>%filter(Heartdata$sex==0)
Heartdata$age<-Heartdata$ï..age
Heartdata<-Heartdata[,-1]
Heartdata<-Heartdata[,-1]
Heartdata$target<-as.factor(Heartdata$target)
Heartdata_sampling_vector <-createDataPartition(Heartdata$target, p=0.8, list=FALSE)
Heartdata_train<- Heartdata[Heartdata_sampling_vector,]
Heartdata_test<- Heartdata[-Heartdata_sampling_vector,]

#pick parameters using K-fold validation, "tune" fucntion in e1071 package
set.seed(1)
yes<-seq(0,2,.1)
tuneradialMale<-tune(svm, target ~., data=Heartdata_train, kernal="radial", ranges=list(cost=seq(1:10), gamma= yes))


set.seed(1)
tunelinearMale<-tune(svm, target ~.,data=Heartdata_train, kernal="linear", ranges=list(cost=seq(1:5)),validation.x=Heartdata_test)



model_SVM<-svm(target ~., data=Heartdata_train, kernel = "linear", cost=tunelinearMale$best.parameters$cost)

test_predictions<-predict(model_SVM, Heartdata_test[,c(1:11,13)])
TEST_SET_result_linear_MALE<-mean(Heartdata_test[,12] == test_predictions)


model_SVM<-svm(target ~., data=Heartdata_train, kernel = "radial", cost=tuneradialMale$best.parameters$cost, gamma=tuneradialMale$best.parameters$gamma)

test_predictions<-predict(model_SVM, Heartdata_test[,c(1:11,13)])
TEST_SET_result_radial_MALE<-mean(Heartdata_test[,12] == test_predictions)
#table summary

Bestparameters<-data.table(model_name=c("Linear Kernal SVM Female", "Radial Kernal SVM Female ", "Linear Kernal SVM Male", "Radial Kernal SVM Male"),Performance_on_train_set=c(1-tunelinearFemale$best.performance, 1-tuneradialFemale$best.performance, 1-tunelinearMale$best.performance, 1-tuneradialMale$best.performance), Cost=c(tunelinearFemale$best.parameters$cost, tuneradialFemale$best.parameters$cost,tunelinearFemale$best.parameters$cost, tuneradialMale$best.parameters$cost), Gamma=c( "NA", tuneradialFemale$best.parameters$gamma,"NA", tuneradialMale$best.parameters$gamma), Performance_on_test_set=c(TEST_SET_result_linear_female,TEST_SET_result_radial_female,TEST_SET_result_linear_MALE,TEST_SET_result_radial_MALE))

Bestparameters
