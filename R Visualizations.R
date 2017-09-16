data("iris")

#Univariate Analysis
#Histo gram to verify distribution of data
par(mfrow <- c(1:4))
?par
for(i in 1:4)
  hist(iris[, i], main=names(iris)[i])

#Density plot
library(lattice)
for(i in 1:4)
  plot(density(iris[, i]), main=names(iris)[i])

#Box and Wishker plot
for(i in 1:4)
  boxplot(iris[, i], main=names(iris)[i])

#Bar plot for categorical data
library(mlbench)
data(BreastCancer)
par(nfrow <- c(2,4))
for(i in 2:9){
  counts = table(BreastCancer[, i])
  barplot(counts, main=names(BreastCancer)[i])
}

#Missing Plot
install.packages("Amelia")
library(Amelia)
data("Soybean")
missmap(Soybean, col = c('green', 'blue'), legend=FALSE)


#Multivariate Visualization

#Correlation plot
#Correlation plot shows the relation between pair of attributes
#install.packages('corrplot')
#library(corrplot)
correlations = cor(iris[, 1:4])
#corrplot(correlations, method="circle")

#Scatterplot matrix
pairs(iris)

#Scatterplot matrix by class
pairs(iris$Species~., data=iris, col=iris$Species)