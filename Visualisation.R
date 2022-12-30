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
plot(data$Petal.Length, data$Petal.Width, pch = 1, bg=1, cex=1, lwd=5, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 10, bg=10, cex=2, lwd=5, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 20, bg=20, cex=3, lwd=5, col = rainbow(25))
plot(data$Petal.Length, data$Petal.Width, pch = 25, bg=25, cex=4, lwd=5, col = rainbow(25))

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

hist(data$Petal.Width, xlab="x", ylab="y", axes="FALSE", col="orange", main="hist")
plot(iris$Species, main = "Barplot", col="red", border="green")

