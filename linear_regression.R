library(AppliedPredictiveModeling)
library(ggplot2)
data(abalone)

y<-abalone$Rings
y
x<-abalone$Diameter
x

relation<-lm(y~x)
relation

plot(x,y,col = "blue",main = "Diameter and Age(rings) diameter", xlab="Diameter", ylab="Rings",
     abline(relation, col="red", lwd=5))
legend("topleft", c("Data points", "Best fit line"), fill=c("blue", "red"))
