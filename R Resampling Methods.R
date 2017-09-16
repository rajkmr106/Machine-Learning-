########################
#Data Resampling methods
########################

# 1. Data Split
# 2. Bootstrap
# 3. K Fold cross validation
# 4. Repeated K Fold Cross validation
# 5. 

# 1. Data Split
data('iris')
trainIndex <- createDataPartition(iris$Species, p=0.8, list=FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

#fit model
library(caret)

install.packages("klaR")
library(klaR)
fit <- NaiveBayes(Species~., data= trainData)
predictions <- predict(fit, testData[1:4])

#confusion matrix
confusionMatrix(predictions$class, testData$Species)

# 2.Bootstrap resampling :
#Bootstrap resampling involves taking random samles from the same data set against which to evaluate the model.

tc1 = trainControl(method="boot", number = 100 )
fit = train(Species~., data=iris, trControl=tc, method="nb")
print(fit)


# 3.K Fold Cross validation: 
# In this process, data set is split into k subsets and each subset is held out while the model is trained for all other subsets. 
# This process is completed until accuracy is determined for each instance in the data set. and an overall accuracy estimate is evaluated.

tc2 = trainControl(method='cv', number = 10)
fit1 = train(Species~., data=iris, trControl=tc2, method="nb")
print(fit1)

# 4. Repeated Cross Validation: 
# In this process, data set is split into k subsets repeatedly. and the final model accuracy is taken as mean of the k fold validations
tc3 = trainControl(method='repeatedcv', number=10, repeats = 3)
fit2 = train(Species~., data=iris, trControl=tc3, method="nb")
print(fit2)

#Leave One Out Cross Validation: 
# In this process, one data set is left out and a model is constructed on all other data sets. and this process is repeated for all data instances.
tc4 = trainControl(method='LOOCV')
fit3 = train(Species~., data=iris, trControl=tc4, method="nb")
print(fit3)






