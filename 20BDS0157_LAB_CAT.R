# 20BDS0157 SIDDHARTH MANDAL LABCAT
# PROGRAMMING FOR DATA SCIENCE
# CSE3046 SLOT L33+L34

# 1 

data=iris
data

library(dplyr)
# A.
slice(data, 5, 10, 15)
# B.
rename_with(data, toupper, 3)
# C.
sort(data$Petal.Width, decreasing = TRUE)
# D.
arrange(data, Sepal.Width, Sepal.Length)
# E.
library(mltools)
library(data.table)
data$Species=data.frame(one_hot(as.data.table(data$Species)))
data

# 2
# LOADING THE REQUIRED LIBRARIES
library(dplyr)
library(mlbench)
library(caTools)
library(e1071)
library(caret)
library(partykit)
library(randomForest)


# READING DATA
data=read.csv("/home/siddharth/Documents/R/DATASET_1.csv")
data

# CHECKING FOR NULL VALUES
colSums(is.na(data))

# HANDLING MISSING VALUES
data$education[is.na(data$education)]=mean(data$education, na.rm=TRUE)
data$cigsPerDay[is.na(data$cigsPerDay)]=mean(data$cigsPerDay, na.rm=TRUE)
data$BPMeds[is.na(data$BPMeds)]=mean(data$BPMeds, na.rm=TRUE)
data$totChol[is.na(data$totChol)]=mean(data$totChol, na.rm=TRUE)
data$BMI[is.na(data$BMI)]=mean(data$BMI, na.rm=TRUE)
data$heartRate[is.na(data$heartRate)]=mean(data$heartRate, na.rm=TRUE)
data$glucose[is.na(data$glucose)]=mean(data$glucose, na.rm=TRUE)

colSums(is.na(data))

data

# REMOVING THE CLASS LABEL
data2=data[-16]

data2=sapply(data2, function(x)((x-min(x))/(max(x)-min(x))))
data2=data.frame(data2)
data2

for (i in colnames(data2)) {
  data[[i]]=data2[[i]]
}

data

# SPILLITING THE DATASET
split=sample.split(data, SplitRatio = 0.7)
train=subset(data, split==TRUE)
test=subset(data, split==FALSE)
dim(train)
dim(test)

# FITTING THE MODEL
model=svm(TenYearCHD~., train, type="C-classification", kernel="linear")
summary(model)
# PREDICTING THE MODEL
prediction=predict(model, test)
prediction

# CONFUSION MATRIX
cm=table(test$TenYearCHD, prediction)
cm
# ACCURACY, PRECISION, RECALL
confusionMatrix(cm)
confusionMatrix(cm, mode="prec_recall", positive="1")

# 3.
# SCATTER PLOT OF PETAL WIDTH WITH TRIANGLE, WITHOUT AXES, AND COLOR
x=iris$Petal.Width
plot(x, type='p', pch=2, col="red", main="Petal width scatter plot", axes=FALSE)
