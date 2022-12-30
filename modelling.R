library(dplyr)
library(mlbench)
library(caTools)
library(e1071)
library(caret)
library(partykit)
library(randomForest)

data=mtcars
data

data2=data[-8]
data2=data2[-8]
data2

data2=sapply(data2, function(x)((x-min(x))/(max(x)-min(x))))
data2

data2=data.frame(data2)
data2
for(i in colnames(data2)) {
  data[[i]]=data2[[i]]
}

data=data[-9]
data
split=sample.split(data, SplitRatio = 0.7)
train=subset(data, split==TRUE)
test=subset(data, split==FALSE)
train
test

#Logistic regression
model=glm(vs~., train, family = "binomial")
summary(model)

pred=predict(model, test, type = "response")
pred

pred=ifelse(pred>0.5, 1, 0)
pred

cm=table(test$vs, pred)
cm

confusionMatrix(cm)
confusionMatrix(cm, mode="prec_recall", positive = "1")

# decision tree

model=ctree(vs~., train)
model

pred=predict(model, test)
pred
pred=ifelse(pred>0.5, 1, 0)
pred
cm=table(test$vs, pred)
cm
confusionMatrix(cm)
confusionMatrix(cm, mode="prec_recall", positive = "1")

# naive bayes
model=naiveBayes(vs~., train)
model
pred=predict(model, test)
pred
cm=table(test$vs, pred)
cm
confusionMatrix(cm)
confusionMatrix(cm, mode="prec_recall", positive = "1")

# svm
model=svm(vs~., train, type="C-classification", kernel="linear")
model
pred=predict(model, test)
pred
cm=table(test$vs, pred)
cm
confusionMatrix(cm)
confusionMatrix(cm, mode="prec_recall", positive="1")

# random forest
model=randomForest(as.factor(vs)~., train)
model
pred=predict(model, test)
pred
cm=table(test$vs, pred)
cm
confusionMatrix(cm)
confusionMatrix(cm, mode="prec_recall", positive="1")

# linear regression
model=lm(mpg~disp+wt, train)
model
pred=predict(model, test)
pred
error=pred-test$mpg
error
rmse=sqrt(mean(error^2))
rmse

# kmeans
data=iris[-5]
cluster=kmeans(data, centers = 3, nstart=20)
cluster
cluster$cluster
cm=table(iris$Species, cluster$cluster)
cm
cluster$centers

# hclust
data=iris[-5]
mat=dist(data, method="euclidean")
mat
cluster=hclust(mat, method="average")
plot(cluster)
fit=cutree(cluster, k=3)
fit
abline(h=1.5, col="red")
rect.hclust(cluster ,k=3, border="green")
