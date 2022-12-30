library(ggplot2)
dat <- ggplot2::mpg
summary(dat$hwy)
min(dat$hwy)
max(dat$hwy)
range(dat$hwy)


ggplot(dat) +
  aes(x = hwy) +
  geom_histogram(bins = 30L, fill = "#0c4c8a") +
  theme_minimal()

ggplot(dat) +
  aes(x = "", y = hwy) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

boxplot.stats(dat$hwy)$out

out <- boxplot.stats(dat$hwy)$out
out_ind <- which(dat$hwy %in% c(out))
out_ind

dat[out_ind, ]

boxplot(dat$hwy,
        ylab = "hwy",
        main = "Boxplot of highway miles per gallon"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

boxplot(mpg~cyl, data=mtcars, xlab="Number of cylinders", ylab="Miles per gallon",
        main="MTCARS", notch=TRUE, varwidth=TRUE, col=c("red", "green", "blue"), names=c("low", "medium", "high"))

data=iris
hist(data$Sepal.Length, main="TEST", xlab="x", ylab="y", col="green", border="red", xlim=c(5, 7), ylim=c(10, 20))
hist(data$Sepal.Length, axes=FALSE)
hist(data$Sepal.Length, as.numeric(data$Species))
axis(1, at=5:7)
axis(2, at=15:20)

colors=colorRampPalette(c("cyan", "darkgreen"))
heatmap(as.matrix(iris[, 1:4]), col=colors(100))

data=mtcars
data

summary(data)
min(data$disp)
max(data$disp)
range(data$disp)

hist(data$disp, xlab="feature", main="histogram", breaks=sqrt(nrow(data)))

qqnorm(data$disp, main="QQplot", type="p", col="red", pch=25, bg="blue")

data=iris
boxplot(data$Species, rnorm(150))

plot(iris$Species, rnorm(150))

x=data$Sepal.Width
boxplot(data$Sepal.Width~data$Species, main="Outlier detection", names=c("HEHE", "HOHO", "LOL"), col=c("red", "green", "blue"))

boxplot(data$Sepal.Width)
out=boxplot.stats(x)$out
out_in=which(x %in% c(out))
data[out_in, ]

mtext(paste("Outliers: ", paste(out_in, collapse = ",")))


# boxplot with outliers  
x=data$Sepal.Width
x

boxplot(x)
out=boxplot.stats(x)$out
out
out_in=which(x %in% c(out))
out_in

mtext(paste("Outliers: ", paste(out, collapse = ",")))

mean=mean(x)
sd=sd(x)
mean-3*sd
mean+3*sd
ar=c()
for (i in 1:150) {
  if (x[i]<mean-3*sd || x[i]>mean+3*sd) {
    print(i)
    ar=append(ar, i)
  }
}

for (i in 1:150) {
  print(paste(i, x[i]))
}


median=median(x)
median
abs_dev=abs(x-median)
mad=1.4826*median(abs_dev)
median-3*mad
median+3*mad

ar=c()
for (i in 1:150) {
  if (x[i]<median-3*mad || x[i]>median+3*mad) {
    print(i)
    ar=append(ar, i)
  }
}

data=iris
lb=quantile(x, 0.01)
ub=quantile(x, 0.99)

outlier_ind=which(x<lb| x>ub)
outlier_ind

library(outliers)
data=ggplot2::mpg
subdata=data[1:20,]
test=dixon.test(subdata$hwy, opposite = TRUE)
test

test=grubbs.test(data$hwy)
test

data=iris
test=grubbs.test(data$Sepal.Width)
test

data=ggplot2::mpg
library(EnvStats)
boxplot(data$hwy)
test=rosnerTest(data$hwy, k=3)
test$all.stats


data=read.csv("/home/siddharth/Downloads/DATASET_1.csv")
data

table(data$TenYearCHD)
prop.table(table(data$TenYearCHD))

library(ROSE)

data=na.omit(data)



balancedata=ovun.sample(TenYearCHD~ ., data = data, method = "under", N = 1610)$data
table(balancedata$TenYearCHD)
prop.table(table(balancedata$TenYearCHD))

balancedata=ovun.sample(TenYearCHD~., data=data, method="over", N=1509+nrow(data))$data
table(balancedata$TenYearCHD)
prop.table(table(balancedata$TenYearCHD))

data=iris
pie(data$Sepal.Length)

