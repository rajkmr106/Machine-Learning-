#Data Pre processing Techniques

library(caret)
?preProcess

# Why Data Transformation is Required? 

# Data Transformation is required because some Macine Learning Algorithms 
# can give better accuracy if the the data is trained in specific form 
# and some algoritms can perform well if the data exposed to them is prepared in specific way

#Thumb rules for data preparation
# 1. Instances based methods can give best accuracies if the input values are of the same scale. 
# 2. Regression based model can work better if the input values are standardied. 

#Summary of Preparation Methods used in carat package. 

# 1. Box-Cox : Its used if all the values are numeric and non zero
# 2. Yeo Johnson: Its used like Box-Cox, but also for negative values
# 3. expoTrans: It applies a power transformation
# 4. zv: Remove the attribues with zero variance(all the same value)
# 5. nzv: Remove the attributes with Near zero variance (close to the same value)
# 6. Center: devide all the values by standard deviation
# 7. scale: subtract all the values by standard deviation
# 8. range: normalize values
# 9. pca: Transform the data to the principle component
# 10. ica: Transform the data to the independent component
# 11. SpatialSign: Project the data on to the unit circle. 


#Scale Transformation : (substract all the values by standard deviation)
data(iris)
summary(iris)
summary(iris[, 1:4])
scaleTransformation <- preProcess(iris, method=c("scale") )
scaleTransformData <- predict(scaleTransformation, iris)
summary(scaleTransformData)

#Center Transformation: (devide all the values by standard deviation)
centerTransformation <- preProcess(iris, method = c("center"))
centerTransformData <- predict(centerTransformation, iris)
summary(centerTransformData)


#Strandarization: 
standarizeTransormation <- preProcess(iris, method = c("center", "scale"))
standardizedData <- predict(standarizeTransormation, iris)
summary(standardizedData)


#Normalization: 
normalizeTransformation <- preProcess(iris, method = c("range"))
normalizedData <- predict(normalizeTransformation, iris)
summary(normalizedData)

#Box-Cox : Can be applied if the values are positive numbers
# Note: When an attribute has a Gaussian-like distribution but is shifted, this is called a skew. 
# The distribution of an attribute can be shifted to reduce the skew and make it more Gaussian.
library(mlbench)
data("PimaIndiansDiabetes")
boxCoxTransformation <- preProcess(PimaIndiansDiabetes[, 7:8], method = c("BoxCox"))
boxCoxTransformedData <- predict(boxCoxTransformation, PimaIndiansDiabetes[, 7:8])
summary(boxCoxTransformedData)

# Yeo Johnson Transformation  : Can be applied if the values are zero or negativ numbers. 
yeoJohnsonTransformation <- preProcess(PimaIndiansDiabetes[, 7:8], method= c("YeoJohnson"))
yeoJohnsonTransformedData <- predict(yeoJohnsonTransformation, PimaIndiansDiabetes[, 7:8])
summary(yeoJohnsonTransformedData)

#Principal component analysis Transformation
pcaTransformation <- preProcess(iris[, 0:4], method=c("center", "scale", 'pca'))
pcaTransformedData <- predict(pcaTransformation, iris)
print(pcaTransformedData)

#Independent component analysis transformation
install.packages("fastICA")
#icaTransformation = preProcess(PimaIndiansDiabetes[, 1:9], method = c("center", "scale", 'ica'), n.comp = 5)
#icaTransformedData = predict(icaTransformation, PimaIndiansDiabetes[, 1:9])
#print(icaTransformedData)



