














































































































data=read.csv("/home/siddharth/Documents/R/Cancer_Data.csv")
data

data=iris
data

library(datasets)
library(mlbench)
data(BostonHousing)
data
data=mtcars
data

read.csv("/home/siddharth/Documents/R/Cancer_Data.csv", header=TRUE, sep=",", dec=".")

read.delim(file.choose(), header=FALSE)
read.delim(file.choose(), header=FALSE)

data=iris
data

head(iris)
head(iris, 2)

tail(iris)
tail(iris, 3)

dim(iris)

names(iris)

str(iris)

summary(data)

quantile(data[[1]])

data[1]
data[[1]]

min(data[1])
min(data[[1]])
max(data[1])

library(dplyr)
glimpse(data)

select(data, 1)
select(data, "Sepal.Length")
slice(select(data, "Sepal.Length"), 1:10)
slice(select(data, 2:3), 5:10)
head(select(data, 2, 4))
select(data, -2, -5)
select(data, starts_with("Sepal"))
select(data, ends_with("Length"))
select(data, contains("Wi"))

testy=data.frame(col=1:5, col1=25, col2=40, col3=47)
testy

select(testy, matches("col\\d+")) # 1 or more
select(testy, matches("col\\d*")) # 2 or more
select(testy, num_range("col", 2:3))

filter(data, Sepal.Length>5)
filter(data, between(Sepal.Length, 5, 6))
filter(data, near(Sepal.Width, 2))
filter(data, Sepal.Length<5 & Species!="setosa")

slice(select(filter(data, Sepal.Length>5), 3), 5:10)

sort(data[[1]], decreasing=TRUE)
x=c(6, 2, 8, 4, NA, 9, 0)
sort(x, na.last=TRUE)
sort(x)
sort(x, decreasing = TRUE)
sort(x, decreasing = TRUE, na.last=TRUE)
sort(x, decreasing = TRUE, na.last=FALSE)

x=data.frame(col1=c("x",'y','x','z'),col2=c(40,45,35,40),col3=45)
x=group_by(x, col1)
x

summarise(x, mean(col2), mean(col3))
summarise(x)
x=ungroup(x)

x=data.frame(col1=c("x",'y','x','z'),col2=c(40,45,35,40),col3=45)
arrange(x, desc(col1), col2)

x=c(2,5,1, NA, 10,7)
rank(x, na.last = TRUE)
rank(x, na.last = FALSE)
x=c(1, 6 , 3, NA, 3, 9, 0)
rank(x, na.last = TRUE) # by default average
rank(x, na.last = TRUE, ties.method="first")
rank(x, na.last = TRUE, ties.method="last")
rank(x, na.last = TRUE, ties.method="min")
rank(x, na.last = TRUE, ties.method="max")
rank(x, na.last = TRUE, ties.method="average")
rank(x, na.last = TRUE, ties.method="random")

x=c(9, 8, 7, 6, 5, 4)
order(x)

order(data$Sepal.Length)

rename(data, petal_length=Petal.Length)
rename_with( data, toupper)
head(rename_with(data, toupper, 2))
head(rename_with(data, toupper, starts_with("Petal")))
head(rename(data, petal_length=2))

yy=data.frame(col1=c('x','y','x','z'),col2=c(40,45,35,40),col3=45)
mutate(yy, newcol=col2+col3, .keep = "used")
mutate(yy, newcol=col2+col3, .keep = "unused")
mutate(yy, newcol=col2+col3, .keep = "all")
mutate(yy, newcol=col2+col3, .keep = "none")

transmute(yy, Marksum=col2+col3)

slice(data, 1)
slice(data, n())
slice(data, 5:n())
slice_head(data)
slice_tail(data)
slice_min(data, Sepal.Length)
slice_max(data, Sepal.Length)
slice_sample(data)

data=airquality
data
is.na(data)
na.omit(data)
data[, colSums(is.na(data))==0]

t=is.na(data)
data[t]="unknown"
data

data$Ozone[is.na(data$Ozone)]=mean(data$Ozone, na.rm=TRUE)
data 
data$Solar.R[is.na(data$Solar.R)]=mean(data$Solar.R, na.rm=TRUE)
data

library(tidyr)
data=data %>% group_by(Month) %>% mutate(Ozone= ifelse(is.na(Ozone),mean(Ozone,na.rm = TRUE),Ozone)) 
data = data %>% group_by(Month) %>% mutate(Solar.R= ifelse(is.na(Solar.R),mean(Solar.R,na.rm = TRUE),Solar.R))
data

df = data.frame(col1 = c(1,NA,NA,2,NA),col2 = c(NA,1,2,NA,NA))
df
fill(df,col1,col2,.direction = "up")
fill(df,col1,col2,.direction = "down")
fill(df,col1,col2,.direction = "updown")
fill(df,col1,col2,.direction = "downup")

data=iris
data = iris[-5]
y = sapply(data,function(data)((data-min(data))/(max(data)-min(data))))
y
data = iris[-5]
y = sapply(data,function(data)((data-mean(data))/sd(data)))
y
data=iris
data = iris[-5]
y = sapply(data,function(data)((data-mean(data))/(max(data)-min(data))))
y

data=iris
slice(data, 5, 10, 15)
select(data, -contains("al"))
sort(data$Petal.Width)
arrange(data, data$Sepal.Width)

#performing label encoding
library(CatEncoders)
labels=LabelEncoder.fit(data$Species)
labels
data$Species=transform(labels,data$Species)
data$Species
data
#performing one hot
library(mltools)
library(data.table)
data=one_hot(as.data.table(data))
data

data=iris
data=data[-5]
pca=prcomp(data, scale. =TRUE, center=TRUE, retx=T)
names(pca)
summary(pca)
pca$rotation
pca$sdev


##  modelling

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

model.summ = summary(model)
mean(model.summ$residuals^2)

mean((test$mpg-pred)^2)

mean(abs(test$mpg-pred))

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


## visualization

data<-iris
data

#1
plot(data$Petal.Length, data$Petal.Width)

#2
# Scatterplot
plot(data$Petal.Length, data$Petal.Width, main = "Scatterplot")

# Barplot
plot(iris$Species, main = "Barplot", col="red", border="green")

# Boxplot
plot(iris$Species, rnorm(150), main = "Boxplot")

fun <- function(x) x^2
# Plot R function
plot(fun, 0, 10, main = "Plot a function")

# Correlation plot
plot(iris[, 1:4], main = "Correlation plot")

#3 
plot(data$Petal.Length, data$Petal.Width)
# New window
X11() 
plot(data$Sepal.Length, data$Sepal.Width)

#4
plot(data$Petal.Length, data$Petal.Width, type = "l", main = "type = 'l'")
plot(data$Petal.Length, data$Petal.Width, type = "s", main = "type = 's'")
plot(data$Petal.Length, data$Petal.Width, type = "p", main = "type = 'p'")
plot(data$Petal.Length, data$Petal.Width, type = "o", main = "type = 'o'")
plot(data$Petal.Length, data$Petal.Width, type = "b", main = "type = 'b'")
plot(data$Petal.Length, data$Petal.Width, type = "h", main = "type = 'h'")
plot(data$Petal.Length, data$Petal.Width, type = "n", main = "type = 'n'")

#point shape/type
plot(data$Petal.Length, data$Petal.Width, pch = 1)
plot(data$Petal.Length, data$Petal.Width, pch = 10)
plot(data$Petal.Length, data$Petal.Width, pch = 20)
plot(data$Petal.Length, data$Petal.Width, pch = 25)

# point ke andar
plot(data$Petal.Length, data$Petal.Width, pch = 1, bg=1, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 10, bg=10, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 20, bg=20, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 25, bg=25, col = rainbow(25))

#border color
plot(data$Petal.Length, data$Petal.Width, pch = 1,col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 10,col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 20,col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 25,col = rainbow(25))

# point size
plot(data$Petal.Length, data$Petal.Width, pch = 1, bg=1, cex=3, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 10, bg=10, cex=3, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 20, bg=20, cex=3, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 25, bg=25, cex=3, col = rainbow(25))

#line width
plot(data$Petal.Length, data$Petal.Width, pch = 21, bg=1, cex=1, lwd=5, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 22, bg=10, cex=2, lwd=5, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 23, bg=20, cex=3, lwd=5, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 24, bg=25, cex=4, lwd=5, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 25, bg="green", cex=4, lwd=5, col = "red")

#heading
plot(data$Petal.Length, data$Petal.Width, pch = 25, bg=25, cex=3, lwd=5, col = rainbow(25), main = "HEHE")

#subheading
plot(data$Petal.Length, data$Petal.Width, pch = 25, bg=25, cex=3, lwd=5, col = rainbow(25), main = "HEHE", sub = "LOL")

#xaxis yaxis xlabel ylabel
plot(data$Petal.Length, data$Petal.Width, pch = 25, bg=25, cex=3, lwd=5, col = rainbow(25), main = "HEHE", sub = "LOL", xlab = "X axis", ylab = "Y axis")

# Remove both axis tick labels
plot(data$Petal.Length, data$Petal.Width, yaxt = "n", xaxt = "n", main = "xaxt = 'n', yaxt = 'n'")

#remove complete axes
plot(data$Petal.Length, data$Petal.Width, pch = 25, bg=25, cex=3, lwd=5, col = rainbow(25), main = "HEHE", sub = "LOL", xlab = "X axis", ylab = "Y axis", axes="FALSE")
# Add X-axis
axis(1)
# Add Y-axis
axis(2)

# range of axis
plot(data$Petal.Length, data$Petal.Width, pch = 25, bg=25, cex=3, lwd=5, col = rainbow(25), main = "HEHE", sub = "LOL", xlab = "X axis", ylab = "Y axis", axes="FALSE")
axis(1, at = 1:7)
axis(2, at = 0.5:2.5)

# specify limit to axis
plot(data$Petal.Length, data$Petal.Width, ylim = c(1, 2), xlim = c(2,5))

#size of headings, subheadings
plot(data$Petal.Length, data$Petal.Width, main = "My title", sub = "Subtitle", cex.main = 2,cex.sub = 1.5,cex.lab = 3, cex.axis = 0.5)

# creating plot with labels
# Create the plot
plot(data$Petal.Length, data$Petal.Width,
     main = "HEHE",
     xlab = "Petal Length", ylab = "Petal width",
     pch = 18, col = "blue")

# Plot the labels
text(data$Petal.Length, data$Petal.Width,
     labels = iris$Species,
     cex = 0.6, pos = 4, col = "red")

iris_species=as.numeric(iris$Species)
iris_species
plot(data$Petal.Length, data$Petal.Width, col=c("red", "green", "blue")[iris_species], pch=c(1,2,3)[iris_species])
legend(x="topleft", legend=c("Setosa", "Versicolor", "Virginica"), col=c("red", "green", "blue"), pch=c(1,2,3))

speciesID <- as.numeric(iris$Species)
speciesID
plot(data$Petal.Length, data$Petal.Width, pch = speciesID, col = speciesID, main = "Legend")
legend("topleft", levels(iris$Species),  pch = 1:3, col = 1:3)

hist(data$Petal.Width, xlab="x", ylab="y", axes="FALSE", col="orange", main="hist")
plot(iris$Species, main = "Barplot", col="red", border="green")

boxplot(data$Sepal.Width~data$Species, main="Category wise outliers", notch=TRUE, varwidth=TRUE,
        col=c("red", "green", "blue"), names=c(1, 2, 3))

hist(data$Sepal.Width, breaks = sqrt(nrow(data)))
hist(data$Sepal.Length, main="TEST", xlab="x", ylab="y", col="green", border="red", xlim=c(5, 7), ylim=c(10, 20))
hist(data$Sepal.Length, axes=FALSE)
hist(data$Sepal.Length, as.numeric(data$Species))
axis(1, at=5:7)
axis(2, at=15:20)

plot(data$Sepal.Length, data$Sepal.Width, main="IRIS", col="blue", bg="red", pch=25, xaxt='n', yaxt='n')

data=mtcars
heatmap(as.matrix(select(data, c(1, 2, 3, 4, 5, 6, 7))), main="TEST", xlab = "TEST2", ylab="TEST#")

colors=colorRampPalette(c("green", "red"))
heatmap(as.matrix(select(data, c(1, 2, 3, 4, 5, 6, 7))), col=colors(100))

ar=c(0,0,0)
for(i in 1:150) {
  if(data$Species[i]=="setosa") {
    ar[1]=ar[1]+1
  } else if (data$Species[i]=="versicolor") {
    ar[2]=ar[2]+1
  } else {
    ar[3]=ar[3]+1
  }
}

x=c(50,40,10)
pie(x, labels=c("Setosa", "Versicolor", "Virginica"), radius = 1)

barplot(data$Sepal.Length)
x=c(50,40,10)
barplot(x, names.arg = c(1, 2, 3))

# OUTLIER DETECTION

# histogram

data=read.csv("/home/siddharth/Downloads/DATASET_1.csv")
data
data$education[is.na(data$education)]=mean(data$education, na.rm=TRUE)
data$cigsPerDay[is.na(data$cigsPerDay)]=mean(data$cigsPerDay, na.rm=TRUE)
data$BPMeds[is.na(data$BPMeds)]=mean(data$BPMeds, na.rm=TRUE)
data$totChol[is.na(data$totChol)]=mean(data$totChol, na.rm=TRUE)
data$BMI[is.na(data$BMI)]=mean(data$BMI, na.rm=TRUE)
data$heartRate[is.na(data$heartRate)]=mean(data$heartRate, na.rm=TRUE)
data$glucose[is.na(data$glucose)]=mean(data$glucose, na.rm=TRUE)

hist(data$diaBP, breaks=sqrt(nrow(data)))

# scatter plot

qqnorm(data$diaBP)


# BOX plot

boxplot(data$diaBP, main="Boxplot for outlier detection", col="green",
        border = "red", notch=TRUE, cex=3, pch=25, bg="blue", lwd=2, ylab="DiaBP")
out=boxplot.stats(data$diaBP)$out
out
out_ind=which(data$diaBP %in% c(out))
out_ind
mtext(paste("Outliers: ", paste(out_ind, collapse = ",")))

#pie chart
x = c(50,40,10)
pie(x,labels = c("S1","S2","S3"), radius = 0.5, clockwise = TRUE)

# outlier using mean and SD

x=data$diaBP
mean=mean(x)
mean
sd=sd(x)
sd

ar=which(x<mean-3*sd|x>mean+3*sd)
ar

# hampel filter

median=median(x)
abs_dev=abs(x-median)
mad=1.4826*median(abs_dev)
ar=which(x<median-3*mad | x>median+3*mad)
ar

# percentile

lb=quantile(x, 0.025)
ub=quantile(x, 0.975)
lb
ub
ar=which(x<lb | x>ub)
ar

# dixon's test

library(outliers)
test=dixon.test(x[1:25])
test

test=dixon.test(x[1:25], opposite = TRUE)
test

# grubbs test

test=grubbs.test(x)
test

test=grubbs.test(x[1:25], opposite=TRUE)
test

# rosners test
library(EnvStats)
test=rosnerTest(x, k=10)
test$all.stats

# data balancing

table(data$TenYearCHD)
prop.table(table(data$TenYearCHD))
library(ROSE)
# undersampling
balancedata=ovun.sample(TenYearCHD~., data=data, method="under", N=nrow(data)-2628, seed=1)$data
balancedata

table(balancedata$TenYearCHD)
prop.table(table(balancedata$TenYearCHD))

#oversampling
balancedata=ovun.sample(TenYearCHD~., data=data, method="over", N=nrow(data)+1752, seed=1)$data
balancedata

table(balancedata$TenYearCHD)
prop.table(table(balancedata$TenYearCHD))

data.rose=ROSE(TenYearCHD~., data =data, seed = 1)$data
data.rose

data=iris
data
labels=as.numeric(data$Species)
labels


a=c(1, 2, 3)
b=c(1, 2, 3)
a&b

a=scan()

n=as.integer(readline(prompt = "Enter number: "))
temp=n
count=0
while(temp>0) {
  count=count+1
  temp=temp%/%10
}
count
temp=n
sum=0
while(temp>0){
  sum=sum+(temp%%10)^count
  temp=temp%/%10
}
if(sum==n) {
  print("Armstrong number")
} else {
  print("not armstrong number")
}

while(1) {
  a=as.integer(readline(prompt = "Enter A: "))
  b=as.integer(readline(prompt = "Enter B: "))
  oper=readline(prompt = "Enter operator: ")
  result=switch (oper,
    "+" = a+b,
    "-" = a-b,
    "*" = a*b,
    "/" = a/b,
  )
  print(result)
}


while(1) {
  a=as.integer(readline(prompt = "Enter A: "))
  b=as.integer(readline(prompt = "Enter B: "))
  oper=readline(prompt = "Enter operator: ")
  result=switch (oper,
                 "+" = a+b,
                 "-" = a-b,
                 "*" = a*b,
                 "/" = a/b,
                 "q" = break
  )
  print(result)
}


while(1) {
  a=as.integer(readline(prompt = "Enter A: "))
  b=as.integer(readline(prompt = "Enter B: "))
  oper=readline(prompt = "Enter operator: ")
  result=switch (oper,
                 "+" = a+b,
                 "-" = a-b,
                 "*" = a*b,
                 "/" = a/b,
                 "q" = next
  )
  print(result)
}

fres=0
func= function(x) {
  if(x%%2==0) {
    print("EVEN")
  } else {
    print("ODD")
  }
}

func=function(x) {
  for(i in 1:x) {
    if(i%%2!=0) {
      print(i)
    } else {
      next
    }
  }
}

func=function(x) {
  if(x==2) {
    print("PRIME")
  } else if (x==1 | x==0){
     print("COMPOSITE")
  } else if(x<0){
    print("INVALID INPUT")
  } else {
    res=1
    for(i in 2:(x-1)) {
      if(x%%i==0) {
        res=0
        break
      }
    }
    if (res==0) {
      print("NOT PRIME")
    } else {
      print("PRIME")
    }
  }
}
