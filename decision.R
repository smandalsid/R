library(mlbench)
library(caTools)
library(caret)
library(dplyr)
library(e1071)
library(partykit)
library(randomForest)

# Logistic Regression

data<-mtcars

#standardize
newdata=select(data, -8)
newdata=select(data, -8)
newdata=sapply(data, function(x)((x-min(x))/(max(x)-min(x))))

newdata=data.frame(newdata)

for(i in colnames(newdata)) {
  data[[i]]=newdata[[i]]
}

#split
split=sample.split(data,SplitRatio =0.7)
train=subset(data,split==TRUE)
test=subset(data,split==FALSE)

#model
logisticmodel=glm (vs~wt+disp, data=train, family = "binomial")
logisticmodel

summary(logisticmodel)

#prediction
prediction=predict(logisticmodel, test[-8], type="response")
prediction
prediction<-ifelse(prediction>0.5, 1, 0)

#confusion matrix
cm<-table(test$vs, prediction)
cm
confusionMatrix(cm)
confusionMatrix(cm, mode = "prec_recall", positive="1")

#decision

data<-mtcars

#standardize
newdata=select(data, -8)
newdata=select(data, -8)
newdata=sapply(data, function(x)((x-min(x))/(max(x)-min(x))))

newdata=data.frame(newdata)

for(i in colnames(newdata)) {
  data[[i]]=newdata[[i]]
}

#split
split=sample.split(data,SplitRatio =0.7)
train=subset(data,split==TRUE)
test=subset(data,split==FALSE)


decisionmodel<-ctree(vs~., train)
decisionmodel

prediction=predict(decisionmodel, test)
prediction<-ifelse(prediction>0.5, 1, 0)
prediction

cm <- table(test$vs, prediction)
cm
confusionMatrix(cm)
confusionMatrix(cm, mode = "prec_recall", positive="1")

#naive

data<-mtcars
#standardize
newdata=select(data, -8)
newdata=select(data, -8)
newdata=sapply(data, function(x)((x-min(x))/(max(x)-min(x))))

newdata=data.frame(newdata)

for(i in colnames(newdata)) {
  data[[i]]=newdata[[i]]
}

#split
split=sample.split(data,SplitRatio =0.7)
train=subset(data,split==TRUE)
test=subset(data,split==FALSE)


naiveclassifier<-naiveBayes(vs~., train)
naiveclassifier

prediction=predict(naiveclassifier, test)
prediction

cm <- table(test$vs, prediction)
cm

confusionMatrix(cm)
confusionMatrix(cm, mode = "prec_recall", positive="1")

#svm

data<-mtcars
#standardize
newdata=select(data, -8)
newdata=select(data, -8)
newdata=sapply(data, function(x)((x-min(x))/(max(x)-min(x))))

newdata=data.frame(newdata)

for(i in colnames(newdata)) {
  data[[i]]=newdata[[i]]
}

#split
split=sample.split(data,SplitRatio =0.7)
train=subset(data,split==TRUE)
test=subset(data,split==FALSE)


svmclassifier<-svm(formula =vs~., data = train, type = 'C-classification', kernel = 'linear')
svmclassifier

prediction=predict(svmclassifier, test)
prediction

cm = table(test[, 8], prediction)
confusionMatrix(cm)
confusionMatrix(cm, mode = "prec_recall", positive="1")


#random forest

data<-mtcars
#standardize
newdata=select(data, -8)
newdata=select(data, -8)
newdata=sapply(data, function(x)((x-min(x))/(max(x)-min(x))))

newdata=data.frame(newdata)

for(i in colnames(newdata)) {
  data[[i]]=newdata[[i]]
}

#split
split=sample.split(data,SplitRatio =0.7)
train=subset(data,split==TRUE)
test=subset(data,split==FALSE)


randomforestclassifier<-randomForest(x=select(train, 1, 2, 6), y=as.factor(train$vs), ntree=500) 
randomforestclassifier

prediction=predict(randomforestclassifier, test)
prediction


cm=table(test$vs, prediction)
cm
confusionMatrix(cm)
confusionMatrix(cm, mode = "prec_recall", positive="1")

# 3. k means

irisnew <- iris[, -5]
kmeanscluster <- kmeans(irisnew, centers = 3, nstart = 20)
kmeanscluster
summary(kmeanscluster)
kmeanscluster$cluster
# Confusion Matrix
cm <- table(iris$Species, kmeanscluster$cluster)
cm
kmeanscluster$centers

# 4. hierarchical  clustering

dis_mat <- dist(mtcars, method = 'euclidean')
dis_mat
cluster <- hclust(dis_mat, method = "average")
cluster
plot(cluster)
abline(h = 110, col = "red")
fit <- cutree(cluster, k = 3 )
fit

table(fit)
rect.hclust(cluster, k = 3, border = "green")

